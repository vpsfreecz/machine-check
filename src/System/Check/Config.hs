{-# LANGUAGE OverloadedStrings #-}
module System.Check.Config where

import Data.Foldable (toList)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Environment
import Data.Ini.Config

import System.Check.DNS (DNSConfig, parseDNSConfig)

data Config = Config
  { dnsChecks :: [DNSConfig]
  } deriving (Show)

emptyConfig :: Config
emptyConfig = Config []

parseConfig = do
  menv <- lookupEnv "MCCFG"
  case menv of
    Nothing -> return emptyConfig
    Just env -> do
      f <- TIO.readFile env
      case parseIniFile f iniParser of
        Left err -> do
          print $ "Unable to parse config: " ++ err
          return emptyConfig
        Right x -> return x

iniParser :: IniParser Config
iniParser = do
  dns <- sectionsOf (T.stripPrefix "DNS_") parseDNSConfig

  return $ Config (toList dns)
