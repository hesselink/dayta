module Config (Config (..), get) where

import qualified Database.PostgreSQL.Simple as Db

data Config = Config
  { dbConnectInfo :: Db.ConnectInfo
  } deriving Show

get :: IO Config
get = return Config
  { dbConnectInfo = Db.defaultConnectInfo
      { Db.connectDatabase = "dayta" }
  }
