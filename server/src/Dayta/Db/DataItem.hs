{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module Dayta.Db.DataItem
( Id' (Id)
, Id
, Username' (Username)
, Username
, Dataset' (Dataset)
, Dataset

, DataItem' (DataItem, id, datetime, values, username, dataset)
, DataItem
, DataItemColumn
, DataItemColumnW

, table
, all
, queryAll
, queryBy
, insert
) where

import Prelude hiding (all, id, (.))

import Control.Category ((.))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Int (Int64)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Opaleye (Column, Constant, PGInt8, Query, QueryRunnerColumnDefault (..), Table (Table),
                fieldQueryRunnerColumn, queryTable, required, runInsertMany, runQuery,
                unsafeCoerceColumn, PGTimestamptz, PGJsonb, PGText, optional, keepWhen, (.==),
                (.&&), constant)
import qualified Data.Aeson                           as Json
import qualified Database.PostgreSQL.Simple           as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg

newtype Id' a = Id { unId :: a } deriving (Show, Eq, Ord)
type Id = Id' Int64

makeAdaptorAndInstance "pId" ''Id'

instance Default Constant Id (Column Id) where
  def = dimap unId unsafeCoerceColumn (def :: Constant Int64 (Column PGInt8))

instance Pg.FromField Id where
  fromField fName mData = Id <$> Pg.fromField fName mData

instance QueryRunnerColumnDefault Id Id where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault Json.Value Json.Value where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant Json.Value (Column Json.Value) where
  def = fmap unsafeCoerceColumn (def :: Constant Json.Value (Column PGJsonb))

instance QueryRunnerColumnDefault UTCTime UTCTime where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant UTCTime (Column UTCTime) where
  def = fmap unsafeCoerceColumn (def :: Constant UTCTime (Column PGTimestamptz))

newtype Username' a = Username { unUsername :: a } deriving (Show, Eq, Ord)
type Username = Username' Text

makeAdaptorAndInstance "pUsername" ''Username'

instance Default Constant Username (Column Username) where
  def = dimap unUsername unsafeCoerceColumn (def :: Constant Text (Column PGText))

instance Pg.FromField Username where
  fromField fName mData = Username <$> Pg.fromField fName mData

instance QueryRunnerColumnDefault Username Username where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

newtype Dataset' a = Dataset { unDataset :: a } deriving (Show, Eq, Ord)
type Dataset = Dataset' Text

makeAdaptorAndInstance "pDataset" ''Dataset'

instance Default Constant Dataset (Column Dataset) where
  def = dimap unDataset unsafeCoerceColumn (def :: Constant Text (Column PGText))

instance Pg.FromField Dataset where
  fromField fName mData = Dataset <$> Pg.fromField fName mData

instance QueryRunnerColumnDefault Dataset Dataset where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

data DataItem' a b c d e = DataItem
  { id       :: a
  , datetime :: b
  , values   :: c
  , username :: d
  , dataset  :: e
  } deriving Show

makeAdaptorAndInstance "pDataItem" ''DataItem'

type DataItem = DataItem' Id UTCTime Json.Value Username Dataset
type DataItemColumn = DataItem' (Column Id) (Column UTCTime) (Column Json.Value) (Column Username) (Column Dataset)
type DataItemColumnW = DataItem' (Maybe (Column Id)) (Column UTCTime) (Column Json.Value) (Column Username) (Column Dataset)

table :: Table DataItemColumnW DataItemColumn
table = Table "data_item" $ pDataItem DataItem
  { id       = optional "id"
  , datetime = required "datetime"
  , values   = required "values"
  , username = required "username"
  , dataset  = required "dataset"
  }

all :: Query DataItemColumn
all = queryTable table

by :: Username -> Dataset -> Query DataItemColumn
by un ds = keepWhen (\di -> username di .== constant un .&& dataset di .== constant ds)
         . queryTable table

queryAll :: MonadIO m => Pg.Connection -> m [DataItem]
queryAll conn = liftIO $ runQuery conn all

queryBy :: MonadIO m => Username -> Dataset -> Pg.Connection -> m [DataItem]
queryBy un ds conn = liftIO $ runQuery conn (by un ds)

insert :: MonadIO m => Pg.Connection -> [DataItemColumnW] -> m ()
insert conn dis = liftIO $ void $ runInsertMany conn table dis

deleteAll :: MonadIO m => Username -> Dataset -> Pg.Connection -> m ()
deleteAll = _
