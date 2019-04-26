{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Check.DNS where

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.IP (IPv4, IPv6)
import Network.DNS.Lookup
import Network.DNS.Resolver
import Network.DNS.Types

import System.Check.Types
import Data.Prometheus
import Data.Function ((&))
import Data.Ini.Config (SectionParser, fieldOf, string)

data DNSConfig = DNSConfig
  { name :: Text
  , domain :: ByteString
  , v4resolver :: String
  , v6resolver :: String
  } deriving (Show)

parseDNSConfig :: Text -> SectionParser DNSConfig
parseDNSConfig l = DNSConfig
  <$> pure l
  <*> (B.pack <$> fieldOf "domain" string)
  <*> fieldOf "v4resolver" string
  <*> fieldOf "v6resolver" string

data DNSResult = DNSResult
  { resultName :: Text
  , resultA    :: Either DNSError [IPv4]
  , resultAAAA :: Either DNSError [IPv6]
  } deriving (Show)

instance ToMetrics DNSResult where
  toMetrics baseMetric DNSResult{..} = do
    let baseMetric' = baseMetric & label "name" (B.pack . T.unpack $ resultName)
    addMetric
      (baseMetric' & sub "a" & desc "non-zero if A query failed")
      (eitherToGauge resultA)

    addMetric
      (baseMetric' & sub "aaaa" & desc "non-zero if AAAA query failed")
      (eitherToGauge resultAAAA)

checkDNS DNSConfig{..} = do
  rs4 <- makeResolvSeed defaultResolvConf
          { resolvInfo = RCHostName v4resolver
          , resolvTimeout = 1000000 -- 1 sec
          , resolvRetry   = 1 }

  rs6 <- makeResolvSeed defaultResolvConf
          { resolvInfo = RCHostName v6resolver
          , resolvTimeout = 1000000 -- 1 sec
          , resolvRetry   = 1 }

  resA    <- withResolver rs4 $ \resolver -> do
    lookupA resolver domain

  resAAAA <- withResolver rs6 $ \resolver -> do
    lookupAAAA resolver domain

  return $ DNSResult name resA resAAAA
