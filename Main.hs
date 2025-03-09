{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

-- Aeson and related imports

-- Text and ByteString related imports

-- XML imports

-- File system and process imports

-- Zip archive imports
import Codec.Archive.Zip (findEntryByPath, fromEntry, toArchive)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as B
import Data.List (find)
import Data.List.Split
import Data.Map ((!?))
import qualified Data.Map as M
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, takeExtension, (</>))
import System.Process
import Text.XML
import Text.XML.Cursor

{- RawTunes are records with tune metadata and a 'zscore' field, which is a
  compressed Base64 bytestring representing the MusicXML of the tune. -}

data RawTune = RawTune
  { title :: String,
    tsig :: (Int, Int),
    zscore :: B.ByteString
  }
  deriving (Show, Generic)

-- JSON Instances
instance ToJSON RawTune where
  toJSON RawTune {title, tsig, zscore} =
    object
      [ "title" .= title,
        "tsig" .= [fst tsig, snd tsig],
        "mxml" .= (T.unpack . TE.decodeUtf8 . B.toStrict $ zscore)
      ]

-- truncate title on NEWLINE
instance FromJSON RawTune where
  parseJSON = withObject "RawTune" $ \v -> do
    scoreStr <- v .: "mxml"
    meta <- v .: "metadata"
    title <- meta .: "title"
    tsigStr <- meta .: "timesig"
    let bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ scoreStr
    return $ RawTune (takeWhile (/= '\n') title) (mkTsig tsigStr) bstr

mkTsig str =
  case map (read . T.unpack) . T.split (== '/') . T.pack $ str of
    [i, j] -> (i, j)
    _ -> error "Invalid time signature"

-- Call musescore to create read RawTune data from filepath.
mscoreData :: FilePath -> IO RawTune
mscoreData pth = do
  let cmd = "musescore4portable --score-media \"" ++ pth ++ "\""
  jsonOut <- readCreateProcess (shell cmd) ""
  let bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ jsonOut
  either fail return $ eitherDecode bstr

-- Given a list of filepaths, write RawTunes to JSON.
writeRawTunes :: [FilePath] -> FilePath -> IO ()
writeRawTunes ins out = do
  rawTunes <- mapM mscoreData ins
  B.writeFile out $ encodePretty rawTunes

-- Grab all musescore file paths in `tunes/mscz`.
mscorePaths :: IO [FilePath]
mscorePaths = do
  fs <- listDirectory "tunes/mscz"
  return ["tunes/mscz" </> f | f <- fs, takeExtension f == ".mscz"]

mkTuneJSON :: IO ()
mkTuneJSON = do
  fs <- mscorePaths
  writeRawTunes fs "tunes.json"

-- Create a musicXML score from a base64 encoded string.
b64ToXml :: B.ByteString -> Document
b64ToXml = parseLBS_ def . uzipScore . B64.decodeLenient
  where
    uzipScore zstr =
      case findEntryByPath "score.xml" . toArchive $ zstr of
        Nothing -> error "no score.xml"
        Just e -> fromEntry e

-- Measures are XML nodes. generally, things of type Node are measures.
data Tune = Tune
  { title :: String,
    tsig :: (Int, Int),
    score :: Document
  }
  deriving (Show, Generic)

instance FromJSON Tune where
  parseJSON = withObject "Tune" $ \v -> do
    title <- v .: "title"
    tsig <- v .: "tsig"
    zbstr <- v .: "mxml"
    let doc = b64ToXml . B.fromStrict . TE.encodeUtf8 . T.pack $ zbstr
    return $ Tune title tsig doc

type Section = [Node]

type SecMap = M.Map Char Section

allTunes :: IO [Tune]
allTunes = do
  f <- B.readFile "tunes.json"
  either fail return $ eitherDecode f

-- Grab measures from MusicXML document.
measures :: Document -> [Node]
measures doc = node <$> (measQ . fromDocument $ doc)
  where
    measQ = element "score-partwise" &/ element "part" &/ element "measure"

sections :: [Node] -> [Section]
sections nodes = split (keepDelimsL . whenElt $ hasRH) nodes
  where
    hasRH node = not . null $ fromNode node $// element "rehearsal"

-- Extract rehearsal ID from a 'measure' node.
rhId :: Node -> Char
rhId nd =
  case fromNode nd $// element "rehearsal" of
    [x] -> case x $/ content of
      [] -> error $ "empty!: " ++ show x
      xs -> T.head . head $ xs
    _ -> error $ "no rehearsal mark in: " ++ show nd

mkSecmap :: Tune -> SecMap
mkSecmap tune = M.fromList (zip keys xs)
  where
    xs = tail . sections . measures . score $ tune
    keys = map (rhId . head) xs

data Seata = Seata
  { title :: String,
    tempo :: Int,
    arrng :: [(String, [Char])]
  }
  deriving (Show, Generic)

-- Given a list of tunes `ts` and a string, return tune with that title.
findTune :: [Tune] -> String -> Tune
findTune ts str = case find (\Tune {title} -> title == str) ts of
  Just t -> t
  Nothing -> error $ "Tune not found: " ++ str

instance FromJSON Seata where
  parseJSON = withObject "Seata" $ \v -> do
    title <- v .: "title"
    tempo <- v .: "tempo"
    arrng <- v .: "tunes"
    return Seata {title, tempo, arrng}

allSeata :: IO [Seata]
allSeata = do
  f <- B.readFile "seata.json"
  either fail return $ eitherDecode f

-- Given a tune and a list of section IDs, return the measures for that arrangement.
arrangeScore :: Tune -> [Char] -> [Node]
arrangeScore t@Tune {title} code = concatMap getSec code
  where
    getSec idx = case M.lookup idx (mkSecmap t) of
      Just x -> x
      Nothing -> error $ "Can't find section " ++ [idx] ++ " in " ++ title

-- Given a list of tunes and a seata, return the measures for that set.
seataPreScore :: [Tune] -> Seata -> [Node]
seataPreScore ts seata = concatMap f (arrng seata)
  where
    f (str, code) = arrangeScore (findTune ts str) code

------ Controlling tempo of MXML
-- `cleanMeasure`, `tempoElem`, `modifyTempo` are very hacky ways of dealing with tempo normalization...
-- I think we just remove all 'direction' nodes in XML and then insert our own?
cleanMeasure :: Node -> Node
cleanMeasure nd = case nd of
  NodeElement (Element name attrs _) ->
    NodeElement (Element name attrs (node <$> cs))
  _ -> nd
  where
    cs = fromNode nd $/ checkName (/= "direction")

tempoElem :: Int -> Node
tempoElem bpm =
  NodeElement $
    Element "sound" (M.singleton "tempo" (T.pack . show $ bpm)) []

addTempoElem :: Int -> Node -> Node
addTempoElem bpm (NodeElement (Element name attr ns)) =
  NodeElement (Element name attr (tempoElem bpm : ns))

seataScore :: [Tune] -> Seata -> [Node]
seataScore ts s@Seata {title, tempo} = case seataPreScore ts s of
  [] -> error $ "Empty score for seata: " ++ title
  (x : xs) -> addTempoElem tempo x : xs

------ Main execution ----------------------------------------

-- grab template mxml doc
templateXML :: IO Document
templateXML = parseLBS_ def <$> B.readFile "template.mxml"

-- write mxml doc to filepath
writeXML :: Document -> FilePath -> IO ()
writeXML doc pth = B.writeFile pth $ renderLBS def {rsPretty = False} doc

-- insert measures in main body of mxml doc
insertMeasures :: [Node] -> Document -> Document
insertMeasures ms (Document pro (Element name attr ns) _) =
  Document pro newRoot []
  where
    newRoot = Element name attr (ns ++ [NodeElement partElem])
    partElem = Element "part" (M.singleton "id" "P1") ms

-- given a list of tunes and a seata,
--  DO: create score, insert in template, and write doc to file
makeSeataXML :: [Tune] -> Seata -> IO ()
makeSeataXML ts s@Seata {title} = do
  print $ "Making XML for " ++ title
  doc <- templateXML
  let doc' = insertMeasures (seataScore ts s) doc
  writeXML doc' ("seata/mxml/" ++ title ++ ".musicxml")

main :: IO ()
main = do
  ts <- allTunes
  ss <- allSeata
  mapM_ (makeSeataXML ts) ss

makeMP3 :: FilePath -> FilePath -> IO ()
makeMP3 inPath outPath = callCommand cmd
  where
    cmd = "musescore4portable --debug '" ++ inPath ++ "' -o '" ++ outPath ++ "'"

makeAllMP3 :: IO ()
makeAllMP3 = do
  fs <- listDirectory "seata/mxml"
  mapM_ (\f -> makeMP3 ("seata/mxml" </> f) ("seata/mp3" </> replaceExtension f ".mp3")) fs

-- Generate mp3s for each set.

-- -- meas_notes ∷ Node → [Node]
-- -- meas_notes meas = map node (fromNode meas $/ element "note")

-- -- mk_measure ∷ [Node] → T.Text → Node
-- -- mk_measure ns idx = NodeElement elm
-- --   where elm  = Element "measure" attr ns
-- --         attr = M.singleton "number" idx

-- -- merge_measure ∷ (Node, Node) → Node
-- -- merge_measure (m1, m2) = mk_measure notes idx
-- --   where notes = concatMap meas_notes [m1,m2]
-- --         idx = head (fromNode m1 $| attribute "number")

-- -- add_sec ∷ [Node] → Section → [Node]
-- -- add_sec ns (_ana, ms) = case _ana of
-- --   Just ana → init ns ++ [merge_measure (last ns, ana)] ++ ms
-- --   Nothing  → ns ++ ms
