
module System.Check.Types (
    ToMetrics(..)
  , Check(..)
  ) where

import Data.Prometheus
import Data.ByteString (ByteString)
import Data.Attoparsec.Text (Parser)

class ToMetrics a where
  toMetrics :: (Monad m) => MetricId -> a -> MetricsT m

instance (ToMetrics a) => ToMetrics [a] where
  toMetrics baseMetricId = mapM_ $ toMetrics baseMetricId

data (ToMetrics a) => Check a = Check {
    checkMetric :: MetricId
  , checkShellCommand :: String
  , checkParser :: Parser a
  }
