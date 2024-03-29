{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
module Config (Config (..), get) where

import Data.Aeson (FromJSON)
import Data.Word (Word16)
import Data.Maybe (fromMaybe)
import GHC.Generics
import qualified Database.PostgreSQL.Simple as Db
import qualified Data.Yaml as Yaml

data Config = Config
  { dbConnectInfo :: Db.ConnectInfo
  , staticFileDir_ :: FilePath
  , migrationVerbose_ :: Bool
  } deriving (Show, Generic)

data ParsedConfig = ParsedConfig
  { dbConnection :: ConnectConfig
  , staticFileDir :: Maybe FilePath
  , migrationVerbose :: Maybe Bool
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

get :: FilePath -> IO Config
get cfgFile = do
  result <- Yaml.decodeFileEither cfgFile
  case result of
    Right config -> return (mergeWithDefault defaultConfig config)
    Left (Yaml.AesonException e) -> print e >> return defaultConfig
    Left e -> print e >> return defaultConfig

mergeWithDefault :: Config -> ParsedConfig -> Config
mergeWithDefault def pc = Config
  { dbConnectInfo = dbConnectionMergeWithDefault (dbConnectInfo def) (dbConnection pc)
  , staticFileDir_ = fromMaybe (staticFileDir_ def) (staticFileDir pc)
  , migrationVerbose_ = fromMaybe (migrationVerbose_ def) (migrationVerbose pc)
  }

dbConnectionMergeWithDefault :: Db.ConnectInfo -> ConnectConfig -> Db.ConnectInfo
dbConnectionMergeWithDefault def cc = Db.ConnectInfo
  { Db.connectHost = fromMaybe (Db.connectHost def) (host cc)
  , Db.connectPort = fromMaybe (Db.connectPort def) (port cc)
  , Db.connectUser = fromMaybe (Db.connectUser def) (user cc)
  , Db.connectPassword = fromMaybe (Db.connectPassword def) (password cc)
  , Db.connectDatabase = fromMaybe (Db.connectDatabase def) (database cc)
  }

defaultConfig :: Config
defaultConfig = Config
  { dbConnectInfo = Db.defaultConnectInfo
      { Db.connectDatabase = "dayta"
      }
  , staticFileDir_ = "../client/dist"
  , migrationVerbose_ = False
  }
