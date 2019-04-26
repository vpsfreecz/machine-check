module System.Check where

import System.Check.Bird
import System.Check.ZFS
import System.Check.Runner

runAll :: Bool -> IO ()
runAll dryRun = do
  res <- sequence ([
      runCheck birdCheck
    , runCheck bird6Check
    , runCheck zpoolListCheck
    , runCheck zpoolStatusCheck
    ]

  case dryRun of
    False -> saveChecks res
    True  -> dumpChecks res
