{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
import GHC.Generics
import Control.Exception

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List.Split
import Data.Either.Combinators (mapLeft)


import Text.XML
import Text.XML.Cursor
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64

import System.Process
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

import Codec.Archive.Zip (toArchive, fromEntry, findEntryByPath)
data RawTune = RawTune {
  title ∷ String,
  bpm   ∷ Int,
  tsig  ∷ (Int, Int),
  zscore ∷ B.ByteString }
  deriving (Show, Generic)

mscore_data :: FilePath → IO RawTune
mscore_data pth = do
  let cmd = "musescore4portable --score-media '" ++ pth ++ "'"
  let prc = shell cmd
  json_out <- readCreateProcess prc ""
  --- do I need to pack/encode-utf8/bstring  before parsing?
  let bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ json_out
  let tune = eitherDecode bstr
  either fail return tune

instance ToJSON RawTune where
  toJSON RawTune{title, bpm, tsig, zscore} = object
    [ "title" .= title, "bpm"   .= bpm
    , "tsig"  .= [fst tsig, snd tsig]
    , "mxml"  .= ((T.unpack . TE.decodeUtf8 . B.toStrict) $ zscore) ]

writeRawTunes :: [FilePath] → FilePath → IO ()
writeRawTunes ins out = do
  rawTunes <- mapM mscore_data ins
  let json = encodePretty rawTunes
  B.writeFile out json

mscore_paths :: IO [FilePath]
mscore_paths = do
  fs <- listDirectory "tunes/mscz"
  return [ "tunes/mscz" </> f | f <- fs, takeExtension f == ".mscz" ]

write_all ∷ IO ()
write_all = do
  fs <- mscore_paths
  writeRawTunes fs "tunes.json"

instance FromJSON RawTune where
  parseJSON = withObject "RawTune" $ \v -> do
    score_str <- v .: "mxml"
    meta <- v .: "metadata"
    title <- meta .: "title"
    bpm <- meta .: "tempo"
    tsig_str <- meta .: "timesig"
    let score_bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ score_str
    return $ RawTune title bpm (mk_tsig tsig_str) score_bstr

mk_tsig ∷ String → (Int, Int)
mk_tsig str =
  case map (read . T.unpack) . T.split (=='/') . T.pack $ str of
    [i,j] -> (i,j)
    _     -> error "junk time signature"

uzip_score ∷ B.ByteString -> B.ByteString
uzip_score zstr =
  case findEntryByPath "score.xml" . toArchive $ zstr of
    Nothing -> error "no score.xml"
    Just e  -> fromEntry $ e

mk_score ∷ B.ByteString -> Document
mk_score = parseLBS_ def . uzip_score . B64.decodeLenient

-- grab measures from MusicXML object.
measures ∷ Document → [Node]
measures doc = node <$> (que . fromDocument $ doc)
  where que = (element "score-partwise" &/ element "part" &/ element "measure")


-- score data represented by pair of anacrusis and keyval map for sections. 
type ScoreData = ([Node], M.Map Char [Node])
-- split list of measures by 'rehearsal' marks.

rh_id ∷ Node → Char
rh_id nd = case fromNode nd $// (element "rehearsal") of
    [x] -> T.head . head $ (x $/ content)
    _   -> error $ "no rehearsal mark in: " ++ show nd 

split_measures :: [Node] -> ScoreData
split_measures ns = (ana, ch_idx)
  where
    rh node = (fromNode node $// (element "rehearsal"))    
    (ana : mss) = split (keepDelimsL $ whenElt (not . null . rh)) ns
    ch_idx = M.fromList [ (rh_id . head $ ms, ms) | ms <- mss ] 

data Tune = Tune
  { title ∷ String,
    bpm ∷ Int, tsig ∷ (Int, Int),
    score ∷ ScoreData }
  deriving (Show, Generic)

instance FromJSON Tune where
  parseJSON = withObject "Tune" $ \v → do
    title <- v .: "title"
    bpm <-   v .: "bpm"
    tsig <-  v .: "tsig"
    zbstr <- v .: "mxml"
    let doc = mk_score . B.fromStrict . TE.encodeUtf8 . T.pack $ zbstr
    let score = split_measures . measures $ doc
    return Tune{title, bpm, tsig, score}

allTunes ∷ IO [Tune]
allTunes = do
  f <- B.readFile "tunes.json"
  either fail return (eitherDecode f)

main ∷ IO ()
main = do
  docs <- allTunes
  print . score $ (docs !! 6)

writeXML ∷ Document -> IO ()
writeXML doc = B.writeFile "wil-cmd.mxml" xml_bstr
  where xml_bstr = renderLBS def{ rsPretty = False } doc

mk_doc ∷ (Prologue, [Node],[Node]) -> Document
mk_doc (pro,hd,bdy) = Document pro score_elem []
  where score_elem = Element "score-partwise" (M.singleton "version" "4.0")
                   $ hd ++ [NodeElement measure_elem]
        measure_elem = Element "part" (M.singleton "id" "P1") bdy


-- Sets of ceilidh tunes, sequenced by section IDs. 
data Seata = Seata {
  title ∷ String,
  tunes ∷ [(Tune, [String])]
} deriving (Show, Generic)




