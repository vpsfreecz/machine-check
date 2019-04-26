{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module System.Check.Runner (
    runCheck
  , runNative
  , renderChecks
  , dumpChecks
  , saveChecks
  ) where

import Control.Monad
import System.Process
import GHC.IO.Exception (ExitCode(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T
import qualified Data.Map.Strict       as M
import Data.Attoparsec.Text (parseOnly)
import Data.Function ((&))

import Data.Prometheus

import System.AtomicWrite.Writer.ByteString
import System.Check.Types

runCheck :: ToMetrics a
         => Check a
         -> IO MetricState
runCheck Check{..} = do
  (exitcode, stdout, stderr) <- flip readCreateProcessWithExitCode "" $
    shell checkShellCommand

  execMetrics $ do
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
          -> t -> m MetricState
runNative name fn x = fn x >>= execMetrics . toMetrics (metric name)

renderChecks :: [MetricState] -> B.ByteString
renderChecks states = B.concat [
    prettyMetrics (M.unions $ map metrics $ states)
  , B.unlines (concatMap errors states)
  ]

saveChecks :: B.ByteString -> IO ()
saveChecks = atomicWriteFile "/run/metrics/machine-check.prom"

dumpChecks :: B.ByteString -> IO ()
dumpChecks = B.putStr

fromExit :: ExitCode -> Double
fromExit ExitSuccess = 0
fromExit (ExitFailure code) = fromIntegral code

