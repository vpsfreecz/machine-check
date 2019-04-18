module System.Check where

import System.Check.ZFS
import System.Check.Runner

runAll = do
  res <- sequence [
      runCheck zpoolListCheck
    , runCheck zpoolStatusCheck
    ]

  saveChecks res
