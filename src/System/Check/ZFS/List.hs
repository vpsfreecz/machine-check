{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Check.ZFS.List (
    parseList
  , Pool(..)
  , zpoolListCheck
  ) where

import Control.Applicative hiding (empty)
import Data.Text (Text)
import Data.Attoparsec.Text hiding (space, take)
import Data.Function ((&))

import Data.Prometheus

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T

import System.Check.Types
import System.Check.ZFS.Types

data Pool = Pool
  { poolName                 :: Text
  , poolSize                 :: Text
  , poolCapacityPercent      :: Integer
  , poolFragmentationPercent :: Integer
  , poolDedupRatio           :: Double
  , poolHealth               :: PoolState
  } deriving (Eq, Show, Ord)

instance ToMetrics Pool where
  toMetrics baseMetric Pool{..} = do
    let baseMetric' = baseMetric & label "name" (B.pack . T.unpack $ poolName)
    addMetric
      (baseMetric' & sub "healt" & desc "pool health")
      (Gauge $ fromIntegral $ fromEnum poolHealth)

    addMetric
      (baseMetric' & sub "capacity" & desc "pool capacity percentage")
      (Gauge $ fromIntegral $ poolCapacityPercent)

    addMetric
      (baseMetric' & sub "fragmentation" & desc "pool fragmentation percentage")
      (Gauge $ fromIntegral $ poolFragmentationPercent)

    addMetric
      (baseMetric & sub "dedup_ratio" & desc "pool deduplication ratio")
      (Gauge $ poolDedupRatio)

zpoolListCheck :: Check [Pool]
zpoolListCheck = Check {
    checkMetric       = metric "zpool" & sub "list"
  , checkShellCommand = "zpool list -H -o name,size,capacity,fragmentation,dedupratio,health"
  , checkParser       = parseList
  }

tab = char '\t'
word = takeWhile1 (\x -> x /='\t' && x /='\n')
percentage = decimal <* char '%'

parsePoolState :: Parser PoolState
parsePoolState = read . T.unpack . T.toTitle <$> word

parsePool :: Parser Pool
parsePool = do
  poolName <- word
  tab
  poolSize <- word
  tab
  poolCapacityPercent <- percentage
  tab
  poolFragmentationPercent <- percentage
  tab
  poolDedupRatio <- (double <* char 'x')
  tab
  poolHealth <- parsePoolState
  endOfLine
  endOfInput
  return Pool{..}

parseList :: Parser [Pool]
parseList = many1 $ parsePool <* optional endOfLine
