{-# LANGUAGE OverloadedStrings #-}
module System.Check where

import System.Check.DNS
import System.Check.Bird
import System.Check.ZFS
import System.Check.Runner
import System.Check.Config

runAll :: Bool -> IO ()
runAll dryRun = do
  cfg <- parseConfig

  res <- sequence ([
      runCheck birdCheck
    , runCheck bird6Check
    , runCheck zpoolListCheck
    , runCheck zpoolStatusCheck
    ]  ++ (map (runNative "dns" checkDNS) (dnsChecks cfg)))

  case dryRun of
    False -> saveChecks res
    True  -> dumpChecks res
