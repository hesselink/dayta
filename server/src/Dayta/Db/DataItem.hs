{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Dayta.Db.DataItem
( Id' (Id)
, Id
-- , Username' (Username)
, Username
-- , Dataset' (Dataset, unDataset)
-- , Dataset

, DataItem' (DataItem, id, datetime, values, username, datasetId)
, DataItem
, DataItemColumn
, DataItemColumnW

, table
, all
, byUser
, queryAll
, queryBy
, insert
, deleteAll
) where

import Prelude hiding (all, id, (.))

import Control.Arrow (returnA)
import Control.Category ((.))
import Control.Monad (void, forM_)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Int (Int64)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time.Clock (UTCTime)
import Opaleye hiding (values, table)
import qualified Data.Aeson                           as Json
import qualified Database.PostgreSQL.Simple           as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg

import Dayta.Db.Username (Username)
import Dayta.Db.DataSet (DatasetName)
import qualified Dayta.Db.DataSet as Dataset

newtype Id' a = Id { unId :: a } deriving (Show, Eq, Ord)
type Id = Id' Int64

makeAdaptorAndInstance "pId" ''Id'

instance Default ToFields Id (Column Id) where
  def = dimap unId unsafeCoerceColumn (def :: ToFields Int64 (Column PGInt8))

instance Pg.FromField Id where
  fromField fName mData = Id <$> Pg.fromField fName mData

instance DefaultFromField Id Id where
  defaultFromField = fromPGSFromField

instance DefaultFromField Json.Value Json.Value where
  defaultFromField = fromPGSFromField

instance Default ToFields Json.Value (Column Json.Value) where
  def = fmap unsafeCoerceColumn (def :: ToFields Json.Value (Column PGJsonb))

instance DefaultFromField UTCTime UTCTime where
  defaultFromField = fromPGSFromField

instance Default ToFields UTCTime (Column UTCTime) where
  def = fmap unsafeCoerceColumn (def :: ToFields UTCTime (Column PGTimestamptz))

instance SqlOrd UTCTime

data DataItem' a b c d e = DataItem
  { id        :: a
  , datetime  :: b
  , values    :: c
  , username  :: d
  , datasetId :: e
  } deriving Show

makeAdaptorAndInstance "pDataItem" ''DataItem'

type DataItem = DataItem' Id UTCTime Json.Value Username Dataset.Id
type DataItemColumn = DataItem' (Column Id) (Column UTCTime) (Column Json.Value) (Column Username) (Column Dataset.Id)
type DataItemColumnW = DataItem' (Maybe (Column Id)) (Column UTCTime) (Column Json.Value) (Column Username) (Column Dataset.Id)

table :: Table DataItemColumnW DataItemColumn
table = Table "data_item" $ pDataItem DataItem
  { id        = optionalTableField "id"
  , datetime  = requiredTableField "datetime"
  , values    = requiredTableField "values"
  , username  = requiredTableField "username"
  , datasetId = requiredTableField "dataset_id"
  }

all :: Query DataItemColumn
all = orderBy (asc datetime) (selectTable table)

by :: Username -> DatasetName -> Query DataItemColumn
by un dsn = proc () -> do
  ds <- Dataset.by un dsn -< ()
  di <- all -< ()
  restrict -< username di .== toFields un .&& datasetId di .== Dataset.id ds
  returnA -< di

byUser :: Username -> Query DataItemColumn
byUser un = keepWhen (\di -> username di .== toFields un) . all

queryAll :: MonadIO m => Pg.Connection -> m [DataItem]
queryAll conn = liftIO $ runSelect conn all

queryBy :: MonadIO m => Username -> DatasetName -> Pg.Connection -> m [DataItem]
queryBy un ds conn = liftIO $ runSelect conn (by un ds)

insert :: MonadIO m => Pg.Connection -> [DataItemColumnW] -> m ()
insert conn dis = liftIO $ void $ runInsert_ conn (Insert table dis rCount Nothing)

-- TODO 404 if doesn't exist
deleteAll :: MonadIO m => Username -> Dataset.DatasetName -> Pg.Connection -> m ()
deleteAll un dsn conn = liftIO $ void $ Pg.withTransaction conn $ do
  mDs <- Dataset.get un dsn conn
  forM_ mDs $ \ds ->
    runDelete_ conn Delete
      { dTable = table
      , dWhere = \fs -> username fs .== toFields un .&& datasetId fs .== toFields (Dataset.id ds)
      , dReturning = rCount
      }
