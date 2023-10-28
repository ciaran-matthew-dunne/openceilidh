{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
import Data.List.Split
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import GHC.Generics
import System.Process
import Control.Exception

import qualified Data.Map as M

import Text.XML
import Text.XML.Cursor

import Data.Either.Combinators (mapLeft)

import Codec.Archive.Zip (toArchive, fromEntry, findEntryByPath)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64
import Data.Maybe (listToMaybe)

import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)

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


data Tune = Tune
  {title ∷ String,
   bpm ∷ Int, tsig ∷ (Int, Int),
   score ∷ Document }
  deriving (Show, Generic)



uzip_score ∷ B.ByteString -> B.ByteString
uzip_score zstr =
  case findEntryByPath "score.xml" . toArchive $ zstr of
    Nothing -> error "no score.xml"
    Just e  -> fromEntry $ e

mk_score ∷ B.ByteString -> Document
mk_score = parseLBS_ def . uzip_score . B64.decodeLenient

mk_tune ∷ RawTune -> Tune
mk_tune RawTune{title, bpm, tsig, zscore} =
  Tune title bpm tsig (mk_score zscore)

-- gets prologue, boilerplate, and measures.
doc_data ∷ Document → (Prologue, [Node],[Node])
doc_data doc = (pro, node <$> boiler_cs, node <$> measure_cs)
  where pro = documentPrologue doc
        cur = fromDocument  doc
        boiler_cs = (element "score-partwise" &/ checkName (`elem` ["work","identification","part-list"])) cur
        measure_cs = (element "score-partwise" &/ element "part" &/ element "measure") cur

has_rh ∷ Node → Bool
has_rh node = not . null $ fromNode node $// element "rehearsal"

split_measures ∷ [Node] → [[Node]]
split_measures ns = split (keepDelimsL $ whenElt has_rh) ns

(_,_,ms) = doc_data . mk_score $ spoot

mk_doc ∷ (Prologue, [Node],[Node]) -> Document
mk_doc (pro,hd,bdy) = Document pro score_elem []
  where score_elem = Element "score-partwise" (M.singleton "version" "4.0")
                   $ hd ++ [NodeElement measure_elem]
        measure_elem = Element "part" (M.singleton "id" "P1") bdy

writeXML ∷ Document -> IO ()
writeXML doc = B.writeFile "wil-cmd.musicxml" xml_bstr
  where xml_bstr = renderLBS def{ rsPretty = False } doc

main ∷ IO ()
main = do
  let [t1,t2,t3] = map mk_score [spoot,wil,wind]
  let (pro,hd,b1) = doc_data t1
  let (_,_,b2)  = doc_data t2
  let (_,_,b3)  = doc_data t3
  writeXML . mk_doc $ (pro, hd, b1 ++ b2 ++ b3)



data Seata = Seata {
  title ∷ String,
  tunes ∷ [(Tune, String)]
} deriving (Show, Generic)

spoot, wil, wind ∷ B.ByteString
spoot = "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"
wil = "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"
wind = "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"