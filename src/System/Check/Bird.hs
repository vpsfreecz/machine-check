{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Check.Bird (
    parseBird
  , Bird(..)
  , birdCheck
  , bird6Check
  ) where

import Control.Monad
import Control.Applicative hiding (empty)
import Data.Text (Text)
import Data.Attoparsec.Text hiding (space, take)
import Data.Function ((&))

import Data.Prometheus

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T

import System.Check.Types

data ProtocolStatus =
    Up
  | Down
  | Start
  | Stop
  deriving (Eq, Show, Ord, Read, Enum)

data ProtocolInfo = ProtocolInfo
  { name  :: Text
  , proto :: Text
  , table :: Text
  , state :: ProtocolStatus
  , since :: Text
  , info  :: Maybe Text
  } deriving (Eq, Show, Ord)

data Bird = Bird {
  protocols :: [ProtocolInfo]
  } deriving (Eq, Show, Ord)

instance ToMetrics Bird where
  toMetrics baseMetric Bird{..} = do
    forM_ protocols $ \ProtocolInfo{..} -> do
      addMetric
        (baseMetric & label "name" (B.pack . T.unpack $ name))
        (enumToGauge state)

birdCheck :: Check Bird
birdCheck = Check {
    checkMetric       = metric "bird"
  , checkShellCommand = "birdc show protocols"
  , checkParser       = parseBird
  }

bird6Check :: Check Bird
bird6Check = Check {
    checkMetric       = metric "bird6"
  , checkShellCommand = "birdc6 show protocols"
  , checkParser       = parseBird
  }

uEOL = takeTill isEndOfLine
word = takeWhile1 (\x -> x /=' ' && x /='\n')

parseBird :: Parser Bird
parseBird = do
  string "BIRD" <* uEOL <* endOfLine
  string "name" <* uEOL <* endOfLine
  protocols <- many parseStatusLine
  return $ Bird {..}

parseStatusLine :: Parser ProtocolInfo
parseStatusLine = do
  name <- word
  skipSpace
  proto <- word
  skipSpace
  table <- word
  skipSpace
  state <- parseState
  skipSpace
  since <- word
  takeWhile1 (==' ')
  info <- optional (T.append <$> word <*> uEOL)
  endOfLine
  return $ ProtocolInfo {..}

parseState :: Parser ProtocolStatus
parseState = read . T.unpack . T.toTitle <$> word

