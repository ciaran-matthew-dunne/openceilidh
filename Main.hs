{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Aeson
import GHC.Generics
import System.Process
import Control.Exception
import Text.XML
import Data.Either.Combinators (mapLeft)

import Data.Conduit.Zlib
import Data.Conduit.Binary (sinkLbs, sourceLbs)
import Codec.Archive.Zip

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64.Lazy as B64
import Data.Maybe (listToMaybe)


data Tune = Tune {
  title ∷ String,
  bpm   ∷ Int,
  tsig  ∷ (Int, Int),
  score ∷ T.Text }
  deriving (Show, Generic)

main :: IO ()
main = do
  let pth = "tunes/mscz/Willafjord.mscz"
  tune <- mscore_tune pth
  print tune

mscore_tune :: FilePath -> IO Tune
mscore_tune pth = do
  let cmd = "musescore4portable --score-media " ++ pth
  let prc = shell cmd
  json_out <- readCreateProcess prc ""
  --- do I need to pack/encode-utf8/bstring  before parsing?
  let bstr = BSL.fromStrict . TE.encodeUtf8 . T.pack $ json_out
  let tune = eitherDecode bstr
  either fail return tune

instance FromJSON Tune where
  parseJSON = withObject "Tune" $ \v -> do
    meta <- v .: "metadata"
    score_enc <- v .: "mxml"
    title <- meta .: "title"
    bpm <- meta .: "tempo"
    tsigStr <- meta .: "timesig"
    let tsig = (read (init $ takeWhile (/= '/') tsigStr) :: Int,
                read (tail $ dropWhile (/= '/') tsigStr) :: Int)
    return Tune { title = title, bpm = bpm, tsig = tsig, score = decode_mxml score_enc }

-- musescore is evil, and gives us a base64 string representing a compressed musicxml file.
decode_mxml :: String -> T.Text
decode_mxml str =
    case B64.decode . TE.encodeUtf8 . T.pack $ str of
        Left err -> error $ "ERROR: " ++ err
        Right zippedContent -> unzipMusicXML zippedContent

unzipMusicXML :: BSL.ByteString -> T.Text
unzipMusicXML zipped =
    case withArchive (sourceLbs zipped) $ do
      files <- entryNames
      let scoreEntry = filter (\e -> eRelativePath e == "score.xml") files
      case listToMaybe scoreEntry of
        Just entry -> sourceEntry entry .| sinkLbs
        Nothing -> error "score.xml not found in the archive"
    of
      Left err -> error $ "Zip Error: " ++ show err
      Right xmlLbs -> TE.decodeUtf8 $ BSL.toStrict xmlLbs

data Seata = Seata {
  title ∷ String,
  tunes ∷ [(Tune, String)]
} deriving (Show, Generic)
