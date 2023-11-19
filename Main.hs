{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

import GHC.Generics

-- Aeson and related imports
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)

-- Text and ByteString related imports
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64

import Data.List.Split
import Data.Maybe (catMaybes, listToMaybe)
import Data.List (find)
-- XML imports
import Text.XML
import Text.XML.Cursor

-- File system and process imports
import System.Process
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

-- Zip archive imports
import Codec.Archive.Zip (toArchive, fromEntry, findEntryByPath)

import Data.Map ((!?))
import qualified Data.Map as M

-- Directory operations
mscore_paths :: IO [FilePath]
mscore_paths = do
  fs <- listDirectory "tunes/mscz"
  return [ "tunes/mscz" </> f | f <- fs, takeExtension f == ".mscz" ]

mscore_data :: FilePath -> IO RawTune
mscore_data pth = do
  let cmd = "musescore4portable --score-media '" ++ pth ++ "'"
  json_out <- readCreateProcess (shell cmd) ""
  let bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ json_out
  either fail return $ eitherDecode bstr

writeRawTunes :: [FilePath] -> FilePath -> IO ()
writeRawTunes ins out = do
  rawTunes <- mapM mscore_data ins
  B.writeFile out $ encodePretty rawTunes

-- Utility functions
mk_tsig :: String -> (Int, Int)
mk_tsig str =
  case map (read . T.unpack) . T.split (=='/') . T.pack $ str of
    [i,j] -> (i,j)
    _     -> error "Invalid time signature"

write_all :: IO ()
write_all = do
  fs <- mscore_paths
  writeRawTunes fs "tunes.json"



-- Data structures
data RawTune = RawTune {
  title  :: String,
  bpm    :: Int,
  tsig   :: (Int, Int),
  zscore :: B.ByteString
} deriving (Show, Generic)

-- JSON Instances
instance ToJSON RawTune where
  toJSON RawTune{title, bpm, tsig, zscore} = object
    [ "title" .= title
    , "bpm"   .= bpm
    , "tsig"  .= [fst tsig, snd tsig]
    , "mxml"  .= (T.unpack . TE.decodeUtf8 . B.toStrict $ zscore)
    ]

instance FromJSON RawTune where
  parseJSON = withObject "RawTune" $ \v -> do
    score_str <- v .: "mxml"
    meta <- v .: "metadata"
    title <- meta .: "title"
    bpm <- meta .: "tempo"
    tsig_str <- meta .: "timesig"
    let score_bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ score_str
    return $ RawTune title bpm (mk_tsig tsig_str) score_bstr

type Section = (Maybe Node, [Node])
type SecMap = M.Map Char Section
data Tune = Tune {
  title :: String,
  bpm   :: Int,
  tsig  :: (Int, Int),
  score :: SecMap
} deriving (Show, Generic)

-- Create a score from a base64 encoded string.
b64_to_xml :: B.ByteString → Document
b64_to_xml = parseLBS_ def . uzip_score . B64.decodeLenient
    where uzip_score zstr = (case findEntryByPath "score.xml" . toArchive $ zstr of
              Nothing -> error "no score.xml"
              Just e  -> fromEntry e)

-- Grab measures from MusicXML document.
measures :: Document -> [Node]
measures doc = node <$> (meas_q . fromDocument $ doc)
  where
    meas_q = element "score-partwise" &/ element "part" &/ element "measure"

-- Extract rehearsal ID from a 'measure' node.
rh_id :: Node -> Char
rh_id nd =
    case fromNode nd $// element "rehearsal" of
        [x] -> T.head . head $ (x $/ content)
        _   -> error $ "no rehearsal mark in: " ++ show nd

-- remove all 'direction' elements in a measure, we will add our own.
clean_measure ∷ Node → Node
clean_measure meas = undefined

-- Split and index measures by 'rehearsal' marks.
-- *** FIXME *** very hacky way of grabbing anacrusis of first section.
-- doesn't grab potential anacrusis of subsequent sections in tunes.
split_measures :: [Node] -> SecMap
split_measures nodes = M.fromList (zip keys secs)
  where
    hasRH node = not . null $ fromNode node $// element "rehearsal"
    splits = split (keepDelimsL . whenElt $ hasRH) nodes
    (anacrusis : (a_part : mss)) = splits
    secs = (listToMaybe anacrusis, a_part) : [(Nothing, ms) | ms <- mss]
    keys = map (rh_id . head . snd) secs

instance FromJSON Tune where
  parseJSON = withObject "Tune" $ \v -> do
    title <- v .: "title"
    bpm   <- v .: "bpm"
    tsig  <- v .: "tsig"
    zbstr <- v .: "mxml"
    let doc = b64_to_xml . B.fromStrict . TE.encodeUtf8 . T.pack $ zbstr
    return Tune{title, bpm, tsig, score = split_measures . measures $ doc}

allTunes :: IO [Tune]
allTunes = do
  f <- B.readFile "tunes.json"
  either fail return $ eitherDecode f

findTunes ∷ [String] → [Tune] → [Tune]
findTunes ts tunes =
    catMaybes $ map (\str -> find (\Tune{title} -> title == str) tunes) ts



-- WIP. strategy for merging tunes:

data Seata = Seata {
  title :: String,
  tune_seq :: [(Tune,[Char])]
} deriving (Show, Generic)





meas_notes ∷ Node → [Node]
meas_notes meas = map node (fromNode meas $/ element "note")

mk_measure ∷ [Node] → T.Text → Node
mk_measure ns idx = NodeElement elm
  where elm  = Element "measure" attr ns
        attr = M.singleton "number" idx

merge_measure ∷ (Node, Node) → Node
merge_measure (m1, m2) = mk_measure notes idx
  where notes = concatMap meas_notes [m1,m2]
        idx = head (fromNode m1 $| attribute "number")

-- when appending sections X to a list of nodes our policy is:
-- "if section Y has an anacrusis, merge the last measure of X with the anacrusis of Y"
add_sec ∷ [Node] → Section → [Node]
add_sec ns (_ana, ms) = case _ana of
  Just ana → init ns ++ [merge_measure (last ns, ana)] ++ ms
  Nothing  → ns ++ ms

get_tune_secs ∷ (Tune,[Char]) → [Section]
get_tune_secs (t, str) = catMaybes . map (score t !?) $ str

seata_measures ∷ Seata → [Node]
seata_measures Seata{title, tune_seq} = foldl add_sec [] sections
  where sections = concatMap get_tune_secs tune_seq

debug str k = NodeComment . T.pack $ ("TUNE: " ++ str ++ ", SECTION: " ++ show k)

insert_measures ∷ [Node] -> Document -> Document
insert_measures ms (Document pro (Element name attr ns) _) =
  Document pro new_root [] where
    new_root  = Element name attr (ns ++ [NodeElement part_elem])
    part_elem = Element "part" (M.singleton "id" "P1") ms


templateXML ∷ IO Document
templateXML = parseLBS_ def <$> (B.readFile "template.mxml")

-- Write XML to file.
writeXML :: Document -> FilePath -> IO ()
writeXML doc pth = B.writeFile pth $ renderLBS def{ rsPretty = False } doc

-- Main execution
main :: IO ()
main = do
  ts <- allTunes
  doc <- templateXML
  let ts' = findTunes ["Skye Boat Song", "The Dark Island", "The Black Mask"] ts
  let set = Seata "Waltz Set" (zip ts' ["AB", "AB", "ABCB"])
  let doc' = insert_measures (seata_measures set) doc
  writeXML doc' "Waltz_Set.musicxml"
