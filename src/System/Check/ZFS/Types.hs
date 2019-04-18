module System.Check.ZFS.Types where

data PoolState =
    Online
  | Degraded
  | Faulted
  | Offline
  | Removed
  | Unavail
  deriving (Eq, Show, Ord, Read, Enum)

data SpareState =
    Avail
  | Inuse
  deriving (Eq, Show, Ord, Read, Enum)
