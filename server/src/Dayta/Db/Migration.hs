{-# LANGUAGE TemplateHaskell #-}
module Dayta.Db.Migration (migrate) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple.Migration (runMigrations, MigrationCommand (..), MigrationResult (..))
import qualified Database.PostgreSQL.Simple as Pg

migrate :: Pool Pg.Connection -> Bool -> IO ()
migrate pool verbose = withResource pool $ \conn -> do
  res <- runMigrations verbose conn (MigrationInitialization : map (uncurry MigrationScript) getMigrations)
  case res of
    MigrationSuccess -> return ()
    MigrationError e -> error e

getMigrations :: [(String, ByteString)]
getMigrations = sortBy (comparing fst) $(embedDir "db-migrations")
