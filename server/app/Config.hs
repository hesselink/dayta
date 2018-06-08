{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric
  , StandaloneDeriving
  #-}
module Config (Config (..), get) where

import Data.Aeson (FromJSON)
import Data.Word (Word16)
import Data.Maybe (fromMaybe)
import GHC.Generics
import qualified Database.PostgreSQL.Simple as Db
import qualified Data.Yaml as Yaml

data Config = Config
  { dbConnectInfo :: Db.ConnectInfo
  } deriving (Show, Generic)

data ParsedConfig = ParsedConfig
  { dbConnection :: ConnectConfig
  } deriving (Show, Generic)

data ConnectConfig = ConnectConfig
  { host :: Maybe String
  , port :: Maybe Word16
  , user :: Maybe String
  , password :: Maybe String
  , database :: Maybe String
  } deriving (Generic, Show)

instance FromJSON ConnectConfig
instance FromJSON ParsedConfig

get :: IO Config
get = do
  result <- Yaml.decodeFileEither "dayta.yaml"
  case result of
    Right config -> return (mergeWithDefault config)
    Left (Yaml.AesonException e) -> print e >> return defaultConfig
    Left e -> print e >> return defaultConfig

mergeWithDefault :: ParsedConfig -> Config
mergeWithDefault pc = Config
  { dbConnectInfo = dbConnectionMergeWithDefault (dbConnection pc)
  }

dbConnectionMergeWithDefault :: ConnectConfig -> Db.ConnectInfo
dbConnectionMergeWithDefault cc = Db.ConnectInfo
  { Db.connectHost = fromMaybe (Db.connectHost def) (host cc)
  , Db.connectPort = fromMaybe (Db.connectPort def) (port cc)
  , Db.connectUser = fromMaybe (Db.connectUser def) (user cc)
  , Db.connectPassword = fromMaybe (Db.connectPassword def) (password cc)
  , Db.connectDatabase = fromMaybe (Db.connectDatabase def) (database cc)
  }
  where
    def = dbConnectInfo defaultConfig

defaultConfig :: Config
defaultConfig = Config
  { dbConnectInfo = Db.defaultConnectInfo
      { Db.connectDatabase = "dayta"
      }
  }
