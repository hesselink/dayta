{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric
  , StandaloneDeriving
  #-}
module Config (Config (..), get) where

import Data.Aeson (FromJSON)
import Data.Word (Word16)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Paths_dayta (getDataFileName)
import qualified Database.PostgreSQL.Simple as Db
import qualified Data.Yaml as Yaml

data Config = Config
  { dbConnectInfo :: Db.ConnectInfo
  , staticFileDir_ :: FilePath
  , migrationDir_ :: FilePath
  , migrationVerbose_ :: Bool
  } deriving (Show, Generic)

data ParsedConfig = ParsedConfig
  { dbConnection :: ConnectConfig
  , staticFileDir :: Maybe FilePath
  , migrationDir :: Maybe FilePath
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
    Right config -> do
      def <- getDefaultConfig
      return (mergeWithDefault def config)
    Left (Yaml.AesonException e) -> print e >> getDefaultConfig
    Left e -> print e >> getDefaultConfig

mergeWithDefault :: Config -> ParsedConfig -> Config
mergeWithDefault defaultConfig pc = Config
  { dbConnectInfo = dbConnectionMergeWithDefault (dbConnectInfo defaultConfig) (dbConnection pc)
  , staticFileDir_ = fromMaybe (staticFileDir_ defaultConfig) (staticFileDir pc)
  , migrationDir_ = fromMaybe (migrationDir_ defaultConfig) (migrationDir pc)
  , migrationVerbose_ = fromMaybe (migrationVerbose_ defaultConfig) (migrationVerbose pc)
  }

dbConnectionMergeWithDefault :: Db.ConnectInfo -> ConnectConfig -> Db.ConnectInfo
dbConnectionMergeWithDefault def cc = Db.ConnectInfo
  { Db.connectHost = fromMaybe (Db.connectHost def) (host cc)
  , Db.connectPort = fromMaybe (Db.connectPort def) (port cc)
  , Db.connectUser = fromMaybe (Db.connectUser def) (user cc)
  , Db.connectPassword = fromMaybe (Db.connectPassword def) (password cc)
  , Db.connectDatabase = fromMaybe (Db.connectDatabase def) (database cc)
  }

getDefaultConfig :: IO Config
getDefaultConfig = do
  md <- getDataFileName "db-migrations"
  return Config
    { dbConnectInfo = Db.defaultConnectInfo
        { Db.connectDatabase = "dayta"
        }
    , staticFileDir_ = "../client/dist"
    , migrationDir_ = md
    , migrationVerbose_ = False
    }
