{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Aeson
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


mscore_paths ∷ IO [FilePath]
mscore_paths = undefined -- TODO. populate with "/tunes/mscz"

data RawTune = RawTune {
  title ∷ String,
  bpm   ∷ Int,
  tsig  ∷ String, -- (Int, Int),
  zscore ∷ B.ByteString }
  deriving (Show, Generic)

mscore_data :: FilePath -> IO RawTune
mscore_data pth = do
  let cmd = "musescore4portable --score-media " ++ pth
  let prc = shell cmd
  json_out <- readCreateProcess prc ""
  --- do I need to pack/encode-utf8/bstring  before parsing?
  let bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ json_out
  let tune = eitherDecode bstr
  either fail return tune

instance FromJSON RawTune where
  parseJSON = withObject "RawTune" $ \v -> do
    score_str <- v .: "mxml"
    meta <- v .: "metadata"
    title <- meta .: "title"
    bpm <- meta .: "tempo"
    tsig_str <- meta .: "timesig"
    let score_bstr = B.fromStrict . TE.encodeUtf8 . T.pack $ score_str
    return $ RawTune title bpm tsig_str score_bstr

main :: IO ()
main = do
  let pth = "tunes/mscz/Willafjord.mscz"
  rtune <- mscore_data pth
  print . zscore $ rtune

data Tune = Tune
  {title ∷ String,
   bpm ∷ Int, tsig ∷ (Int, Int),
   score ∷ Document }
  deriving (Show, Generic)

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

mk_tune ∷ RawTune -> Tune
mk_tune RawTune{title, bpm, tsig, zscore} =
  Tune title bpm (mk_tsig tsig) (mk_score zscore)

measures ∷ Document → [Cursor]
measures = (element "score-partwise"
          &/ element "part"
          &/ element "measure") . fromDocument

wrap_score ∷ [Node] -> Element
wrap_score = Element "score-partwise" (M.singleton "version" "4.0")

data Seata = Seata {
  title ∷ String,
  tunes ∷ [(Tune, String)]
} deriving (Show, Generic)




wil ∷ B.ByteString
wil = "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"