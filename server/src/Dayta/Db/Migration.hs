module Dayta.Db.Migration (migrate) where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple.Migration (runMigrations, MigrationCommand (..), MigrationResult (..))
import qualified Database.PostgreSQL.Simple as Pg

migrate :: Pool Pg.Connection -> FilePath -> Bool -> IO ()
migrate pool migrationDir verbose = withResource pool $ \conn -> do
  res <- runMigrations verbose conn [MigrationInitialization, MigrationDirectory migrationDir]
  case res of
    MigrationSuccess -> return ()
    MigrationError e -> error e
