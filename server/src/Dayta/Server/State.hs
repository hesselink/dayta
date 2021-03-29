module Dayta.Server.State (State (State, dbConnectionPool, staticFileDir)) where

import Data.Pool (Pool)
import qualified Database.PostgreSQL.Simple as Db

data State = State
  { dbConnectionPool :: Pool Db.Connection
  , staticFileDir :: FilePath
  }
