{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ZpoolParseSpec where

import SpecHelper
import Control.Monad
import Data.Either
import Data.Text
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO     as TIO

statusSamples = [
    "clean"
  , "backuper"
  , "devnode"
  , "stg"
  , "pgnd"
  , "nasbox"
  ]

readSamples :: IO [(String, Text)]
readSamples = forM statusSamples $ \x -> do
  txt <- TIO.readFile $ "test/samples/zpool_status_" ++ x
  return (x, txt)

inputSamples :: [Text]
inputSamples = [
    "tank\t7.94G\t82%\t29%\t1.00x\tONLINE\n"
  , "stor\t5.44T\t42%\t58%\t1.00x\tDEGRADED\n"
  ]

spec :: Spec
spec = do
  samples <- runIO readSamples
  forM_ samples $ \(name, sample) ->
    it ("parses status sample:" ++ name) $ do
      parseOnly parseStatus sample `shouldSatisfy` isRight

  forM_ inputSamples $ \sample ->
    it ("parses list sample:" ++ (unpack sample)) $ do
      parseOnly parseList sample `shouldSatisfy` isRight

  it "parses list correctly" $ do
    parseOnly parseList "tank\t7.94G\t82%\t29%\t1.00x\tONLINE\n"
    `shouldBe`
    (Right [Pool "tank" "7.94G" 82 29 1.00 Online])

main :: IO ()
main = hspec spec
