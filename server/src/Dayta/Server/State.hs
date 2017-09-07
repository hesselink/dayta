module Dayta.Server.State (State (State, dbConnectionPool)) where

import Data.Pool (Pool)
import qualified Database.PostgreSQL.Simple as Db

data State = State
  { dbConnectionPool :: Pool (Db.Connection)
  }
