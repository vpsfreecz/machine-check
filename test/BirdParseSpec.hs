{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module BirdParseSpec where

import SpecHelper
import Control.Monad
import Data.Either
import Data.Text
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO     as TIO

birdSamples = [0..2]

readSamples :: IO [(String, Text)]
readSamples = forM birdSamples $ \x -> do
  txt <- TIO.readFile $ "test/samples/bird" ++ show x
  return (show x, txt)

spec :: Spec
spec = do
  samples <- runIO readSamples
  forM_ samples $ \(name, sample) ->
    it ("parses bird sample:" ++ name) $ do
      parseOnly parseBird sample `shouldSatisfy` isRight


main :: IO ()
main = hspec spec
