{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module System.Check.ZFS.Status (
    parseStatus
  , Status(..)
  , ConfigItem(..)
  , zpoolStatusCheck
  ) where

import Control.Monad
import Control.Applicative hiding (empty)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Attoparsec.Text hiding (space, take)
import Data.Function ((&))

import System.Process

import Data.Prometheus

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T

import System.Check.Types
import System.Check.ZFS.Types

data Status = Status
  { name    :: Text
  , state   :: PoolState
  , status  :: Maybe Text
  , action  :: Maybe Text
  , see     :: Maybe Text
  , scan    :: Text
  , config  :: ConfigItem
  , spares  :: Maybe [(Text, SpareState)]
  , errors  :: Text
  } deriving (Show, Eq, Ord)

instance ToMetrics Status where
  toMetrics baseMetric Status{..} = do
    let baseMetric' = baseMetric & label "name" (B.pack . T.unpack $ name)
    addMetric
      (baseMetric' & sub "healt" & desc "pool health")
      (Gauge $ fromIntegral $ fromEnum state)

    addMetric
      (baseMetric' & sub "errors" & desc "pool errors")
      (Gauge $ fromIntegral $ fromEnum $ errors /= "No known data errors")

    addMetric
      (baseMetric' & sub "status_reported" & desc "pool reports status if 1")
      (Gauge $ fromIntegral $ fromEnum $ isJust status)

zpoolStatusCheck :: Check Status
zpoolStatusCheck = Check {
    checkMetric       = metric "zpool" & sub "status"
  , checkShellCommand = "zpool status"
  , checkParser       = parseStatus
  }

data ConfigItem = ConfigItem
  { itemName       :: Text
  , itemState      :: PoolState
  , readErrors     :: Integer
  , writeErrors    :: Integer
  , checksumErrors :: Integer
  , subItems       :: [ConfigItem]
  } deriving (Show, Eq, Ord)

uEOL = takeTill isEndOfLine
word = takeWhile1 (\x -> x /=' ' && x /='\n')

parsePoolState :: Parser PoolState
parsePoolState = read . T.unpack . T.toTitle <$> word

parseSpareState :: Parser SpareState
parseSpareState = read . T.unpack . T.toTitle <$> word

blockData = do
  fstLine <- uEOL
  endOfLine
  more <- many (char '\t' *> uEOL <* endOfLine)
  return $ T.unwords $ fstLine:more

blockLead lead = do
  skipSpace
  string lead
  string ":"
  optional $ char ' '

block lead = blockLead lead *> blockData

parseState = do
  blockLead "state"
  parsePoolState

parseConfigLine nest = do
  char '\t'
  forM_ (take nest $ [0..]) $ pure $ char ' '
  itemName       <- word
  skipSpace
  itemState      <- parsePoolState
  skipSpace
  readErrors     <- decimal
  skipSpace
  writeErrors    <- decimal
  skipSpace
  checksumErrors <- decimal
  optional (string "  was " *> word)
  uEOL
  endOfLine
  subItems       <- many $ parseConfigLine (nest + 2)
  return $ ConfigItem {..}

parseSpareLine = do
  char '\t'
  char ' '
  char ' '
  itemName       <- word
  skipSpace
  itemState      <- parseSpareState
  uEOL
  endOfLine
  return (itemName, itemState)

parseConfig = do
  blockLead "config"
  endOfLine
  endOfLine -- empty line
  uEOL      -- header NAME STATE READ WRITE CKSUM
  endOfLine
  parseConfigLine 0

parseStatus :: Parser Status
parseStatus = do
  name   <- block "pool"
  state  <- parseState
  status <- optional $ block "status"
  action <- optional $ block "action"
  see    <- optional $ block "see"
  scan   <- block "scan"
  config <- parseConfig
  logs   <- optional (cfgPrefix "logs\t" *> many1 (parseConfigLine 2))
  caches <- optional (cfgPrefix "cache" *> many1 (parseConfigLine 2))
  spares <- optional (cfgPrefix "spares" *> many1 parseSpareLine)
  endOfLine
  errors <- block "errors"
  endOfInput
  return $ Status {..}
  where
    cfgPrefix what = char '\t' *> string what *> endOfLine
