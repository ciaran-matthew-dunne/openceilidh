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

{- RawTunes are records with tune metadata and a 'zscore' field, which is a
  compressed Base64 bytestring representing the MusicXML of the tune. -}

data RawTune = RawTune {
  title  :: String,
  tsig   :: (Int, Int),
  zscore :: B.ByteString
} deriving (Show, Generic)

-- JSON Instances
instance ToJSON RawTune where
  toJSON RawTune{title, tsig, zscore} = object
    [ "title" .= title
    , "tsig"  .= [fst tsig, snd tsig]
    , "mxml"  .= (T.unpack . TE.decodeUtf8 . B.toStrict $ zscore)
    ]

instance FromJSON RawTune where
  parseJSON = withObject "RawTune" $ \v -> do
    score_str <- v .: "mxml"
    meta <- v .: "metadata"
    title <- meta .: "title"
    tsig_str <- meta .: "timesig"
    let bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ score_str
    return $ RawTune title (mk_tsig tsig_str) bstr

mk_tsig str =
    case map (read . T.unpack) . T.split (=='/') . T.pack $ str of
      [i,j] -> (i,j)
      _     -> error "Invalid time signature"

-- Call musescore to create read RawTune data from filepath.
mscore_data :: FilePath → IO RawTune
mscore_data pth = do
  let cmd = "musescore4portable --score-media '" ++ pth ++ "'"
  json_out <- readCreateProcess (shell cmd) ""
  let bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ json_out
  either fail return $ eitherDecode bstr

-- Given a list of filepaths, write RawTunes to JSON.
writeRawTunes :: [FilePath] → FilePath → IO ()
writeRawTunes ins out = do
  rawTunes <- mapM mscore_data ins
  B.writeFile out $ encodePretty rawTunes

-- Grab all musescore file paths in `tunes/mscz`.
mscore_paths :: IO [FilePath]
mscore_paths = do
  fs <- listDirectory "tunes/mscz"
  return [ "tunes/mscz" </> f | f <- fs, takeExtension f == ".mscz" ]

write_all :: IO ()
write_all = do
  fs <- mscore_paths
  writeRawTunes fs "tunes.json"

-- measures are XML nodes. generally, things of type Node are measures.
type Section = (Maybe Node, [Node])
type SecMap = M.Map Char Section
data Tune = Tune {
  title :: String,
  tsig  :: (Int, Int),
  score :: SecMap
} deriving (Show, Generic)

-- Create a musicXML score from a base64 encoded string.
b64_to_xml :: B.ByteString → Document
b64_to_xml = parseLBS_ def . uzip_score . B64.decodeLenient
    where uzip_score zstr = (case findEntryByPath "score.xml" . toArchive $ zstr of
              Nothing -> error "no score.xml"
              Just e  -> fromEntry e)

-- Grab measures from MusicXML document.
measures :: Document → [Node]
measures doc = node <$> (meas_q . fromDocument $ doc)
  where
    meas_q = element "score-partwise" &/ element "part" &/ element "measure"

-- Extract rehearsal ID from a 'measure' node.
rh_id :: Node -> Char
rh_id nd =
    case fromNode nd $// element "rehearsal" of
        [x] -> T.head . head $ (x $/ content)
        _   -> error $ "no rehearsal mark in: " ++ show nd

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
    tsig  <- v .: "tsig"
    zbstr <- v .: "mxml"
    let doc = b64_to_xml . B.fromStrict . TE.encodeUtf8 . T.pack $ zbstr
    return Tune{title, tsig, score = split_measures . measures $ doc}

allTunes :: IO [Tune]
allTunes = do
  f <- B.readFile "tunes.json"
  either fail return $ eitherDecode f

-- WIP strategy for merging tunes:
-- "if section Y has an anacrusis, merge the last measure of X with the anacrusis of Y
-- in a way that preserves the timing..." far too difficult right now.
-- would figure out how many beats in a measure and make sure the merge doesn't exceed this.
-- in the case that they do, you should just replace the last measure by the anacrusis.
-- for now, there will be NO anacrusis.

-- changing the tempo of a musicxml can be done by changing:
--  <sound tempo="XXX"> in the <direction> element
data Seata = Seata {
  title :: String,
  tempo :: Int,
  tunes :: [(String,[Char])]
} deriving (Show, Generic)

instance FromJSON Seata where
  parseJSON = withObject "Seata" $ \v -> do
    title <- v .: "title"
    tempo <- v .: "tempo"
    tunes <- v .: "tunes"
    return Seata{title, tempo, tunes}

allSeata :: IO [Seata]
allSeata = do
  f <- B.readFile "seata.json"
  either fail return $ eitherDecode f

-- Given a tune and a list of section-identifiers, create a list of sections.
get_tune_secs ∷ (Tune,[Char]) → [Section]
get_tune_secs (tune,str) = catMaybes . map (score tune !?) $ str

findTunes ∷ [Tune] → [String] → [Tune]
findTunes ts ts_str =
    catMaybes $ map (\str -> find (\Tune{title} -> title == str) ts) ts_str

-- Given a Seata, create a list of measures corresponding to the tune sequence.
-- Also, for any `<sound tempo="XXX">` element in any measure `Node`, change the value to `tempo`
seata_measures ∷ [Tune] → Seata → [Node]
seata_measures ts Seata{title, tempo, tunes} = (modify_tempo tempo n : ns)
  where
    tune_seq = zip (findTunes ts (map fst tunes)) (map snd tunes)
    nss = concatMap snd (concatMap get_tune_secs tune_seq)
    (n : ns) = map clean_measure nss

clean_measure ∷ Node → Node
clean_measure nd = case nd of
  NodeElement (Element name attrs _) -> NodeElement (Element name attrs (node <$> cs))
  _ → nd
  where cs = fromNode nd $/ checkName (/="direction")

modify_tempo :: Int → Node → Node
modify_tempo bpm (NodeElement (Element name attr ns)) =
  NodeElement (Element name attr (tempo_elem bpm : ns))

tempo_elem :: Int → Node
tempo_elem bpm = NodeElement $ Element "sound" (M.singleton "tempo" (T.pack . show $ bpm)) []

debug str k = NodeComment . T.pack $ ("TUNE: " ++ str ++ ", SECTION: " ++ show k)

insert_measures ∷ [Node] -> Document -> Document
insert_measures ms (Document pro (Element name attr ns) _) =
  Document pro new_root [] where
    new_root  = Element name attr (ns ++ [NodeElement part_elem])
    part_elem = Element "part" (M.singleton "id" "P1") ms

-- Read template musicXML file for us to insert measures.
templateXML ∷ IO Document
templateXML = parseLBS_ def <$> (B.readFile "template.mxml")

writeXML :: Document -> FilePath -> IO ()
writeXML doc pth = B.writeFile pth $ renderLBS def{ rsPretty = False } doc

makeSeataXML :: [Tune] → Seata → IO ()
makeSeataXML ts set@Seata{title,tempo,tunes} = do
  doc <- templateXML
  let doc' = insert_measures (seata_measures ts set) doc
  writeXML doc' ("tunes/mxml/" ++ title ++ ".musicxml")

-- Main execution
main :: IO ()
main = do
  ts <- allTunes
  seata <- allSeata
  mapM_ (makeSeataXML ts) seata


-- meas_notes ∷ Node → [Node]
-- meas_notes meas = map node (fromNode meas $/ element "note")

-- mk_measure ∷ [Node] → T.Text → Node
-- mk_measure ns idx = NodeElement elm
--   where elm  = Element "measure" attr ns
--         attr = M.singleton "number" idx

-- merge_measure ∷ (Node, Node) → Node
-- merge_measure (m1, m2) = mk_measure notes idx
--   where notes = concatMap meas_notes [m1,m2]
--         idx = head (fromNode m1 $| attribute "number")

-- add_sec ∷ [Node] → Section → [Node]
-- add_sec ns (_ana, ms) = case _ana of
--   Just ana → init ns ++ [merge_measure (last ns, ana)] ++ ms
--   Nothing  → ns ++ ms