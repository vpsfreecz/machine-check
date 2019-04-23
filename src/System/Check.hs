module System.Check where

import System.Check.Bird
import System.Check.ZFS
import System.Check.Runner

runAll = do
  res <- sequence [
      runCheck birdCheck
    , runCheck bird6Check
    , runCheck zpoolListCheck
    , runCheck zpoolStatusCheck
    ]

  saveChecks res
