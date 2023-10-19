{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}

import Data.Aeson
import GHC.Generics
import System.Process

import Text.XML

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64.Lazy as B64

data Tune = Tune {
  title ∷ String,
  bpm   ∷ Int,
  tsig  ∷ (Int, Int),
  score ∷ Document
} deriving (Show, Generic)

-- Your existing code for Seata
data Seata = Seata {
  title ∷ String,
  tunes ∷ [(Tune, String)]
}

---- processing metadata json from mscore4.
instance FromJSON Tune where
  parseJSON = withObject "Tune" $ \v -> do
    meta <- v .:  "metadata"
    scoreStr <- v .: "mxml"
    title <- meta .: "title"
    bpm <-   meta .: "tempo"
    tsigStr <- meta .: "timesig"
    let tsig = (read (init $ takeWhile (/= '/') tsigStr) :: Int,
                read (tail $ dropWhile (/= '/') tsigStr) :: Int)
    let decoded = B64.decode . BL.fromStrict . TE.encodeUtf8 . T.pack $ scoreStr
    let scoreText = (case decoded of
       Left err -> error err
       Right bs -> TLE.decodeUtf8 bs)
    let score = parseText_ def scoreText
    return $ Tune title bpm tsig score


get_mscore_json :: FilePath -> IO (Either String Tune)
get_mscore_json filePath = do
  jsonOutput <- readProcess "mscore4portable" [filePath, "--score-meta"] ""
  return . loadTune . BL.fromStrict . TE.encodeUtf8 . T.pack $ jsonOutput

loadTune :: BL.ByteString -> Either String Tune
loadTune jsonString = eitherDecode jsonString

main :: IO ()
main = undefined