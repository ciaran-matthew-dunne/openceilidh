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

main :: IO ()
main = do
  let pth = "tunes/mscz/Willafjord.mscz"
  tune <- mscore_tune pth
  print tune

mscore_tune :: FilePath → IO Tune
mscore_tune pth = do
  let (cmd,args) = ("musescore4portable", "--score-media")
  let prc = (proc cmd [args]){ std_in = Inherit, cwd = Just ".",detach_console=True }
  json_out <- readCreateProcess prc ""
  let tune = eitherDecode . BL.pack . map (fromIntegral . fromEnum) $ json_out
  either fail return tune  

instance FromJSON Tune where
  parseJSON = withObject "Tune" $ \v -> do
    meta <- v .: "metadata"
    score <- v .: "mxml"
    title <- meta .: "title"
    bpm <- meta .: "tempo"
    tsigStr <- meta .: "timesig"
    let tsig = (read (init $ takeWhile (/= '/') tsigStr) :: Int,
                read (tail $ dropWhile (/= '/') tsigStr) :: Int)
    scoreDoc <- either fail return (get_mxml score)
    return Tune { title = title, bpm = bpm, tsig = tsig, score = scoreDoc }


decode_mxml ∷ String → Either String BL.ByteString 
decode_mxml str = 
    mapLeft show $ B64.decode . BL.fromStrict . TE.encodeUtf8 . T.pack $ str

get_mxml ∷ String → Either String Document
get_mxml str = do
  decoded <- decode_mxml str
  mapLeft show $ parseLBS def decoded


data Seata = Seata {
  title ∷ String,
  tunes ∷ [(Tune, String)]
} deriving (Show, Generic)



