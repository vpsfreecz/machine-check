{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module System.Check.Runner (
    runCheck
  , runNative
  , dumpChecks
  , saveChecks
  ) where

import Control.Monad
import System.Process
import GHC.IO.Exception (ExitCode(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import Data.Attoparsec.Text (parseOnly)
import Data.Function ((&))

import Data.Prometheus

import System.AtomicWrite.Writer.ByteString
import System.Check.Types

runCheck :: ToMetrics a
         => Check a
         -> IO B.ByteString
runCheck Check{..} = do
  (exitcode, stdout, stderr) <- flip readCreateProcessWithExitCode "" $
    shell checkShellCommand

  runMetrics $ do
    addMetric
      (checkMetric & sub "success" & desc "process exitcode")
      (Gauge $ fromExit exitcode)

    when (exitcode == ExitSuccess) $ do
      let res = parseOnly checkParser (T.pack stdout)
      addMetric
        (checkMetric & sub "parse" & sub "success" & desc "parsing successful")
        (eitherToGauge res)

      case res of
        Left err -> logError $ B.pack err
        Right x -> toMetrics checkMetric x

-- |Run native check (Haskell function applied to config)
-- and generate metrics of its results
--
-- `name` is the start of the metric name
runNative :: (Monad m, ToMetrics a)
          => B.ByteString
          -> (t -> m a)
          -> t -> m B.ByteString
runNative name fn x = fn x >>= runMetrics . toMetrics (metric name)


saveChecks :: [B.ByteString] -> IO ()
saveChecks = atomicWriteFile "/run/metrics/machine-check.prom" . B.concat

dumpChecks :: [B.ByteString] -> IO ()
dumpChecks = B.putStr . B.concat

fromExit :: ExitCode -> Double
fromExit ExitSuccess = 0
fromExit (ExitFailure code) = fromIntegral code

