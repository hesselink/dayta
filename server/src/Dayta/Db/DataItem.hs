{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module Dayta.Db.DataItem
( Id' (Id)
, Id

, DataItem' (DataItem, id, datetime, values)
, DataItem
, DataItemColumn
, DataItemColumnW

, table
, all
, queryAll
, insert
) where

import Prelude hiding (all, id)

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Proxy (Proxy (Proxy))

import Data.Int (Int64)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time.Clock (UTCTime)
import Opaleye (Column, Constant, PGInt8, Query, QueryRunnerColumnDefault (..), Table (Table),
                fieldQueryRunnerColumn, queryTable, required, runInsertMany, runQuery,
                unsafeCoerceColumn, PGTimestamptz, PGJsonb, optional, IsSqlType (showSqlType))
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

instance IsSqlType Id where
  showSqlType _ = showSqlType (Proxy :: Proxy PGInt8)

instance QueryRunnerColumnDefault Json.Value Json.Value where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant Json.Value (Column Json.Value) where
  def = fmap unsafeCoerceColumn (def :: Constant Json.Value (Column PGJsonb))

instance QueryRunnerColumnDefault UTCTime UTCTime where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant UTCTime (Column UTCTime) where
  def = fmap unsafeCoerceColumn (def :: Constant UTCTime (Column PGTimestamptz))

instance IsSqlType UTCTime where
  showSqlType _ = showSqlType (Proxy :: Proxy PGTimestamptz)

instance IsSqlType Json.Value where
  showSqlType _ = showSqlType (Proxy :: Proxy PGJsonb)

data DataItem' a b c = DataItem
  { id       :: a
  , datetime :: b
  , values   :: c
  } deriving Show

makeAdaptorAndInstance "pDataItem" ''DataItem'

type DataItem = DataItem' Id UTCTime Json.Value
type DataItemColumn = DataItem' (Column Id) (Column UTCTime) (Column Json.Value)
type DataItemColumnW = DataItem' (Maybe (Column Id)) (Column UTCTime) (Column Json.Value)

table :: Table DataItemColumnW DataItemColumn
table = Table "data_item" $ pDataItem DataItem
  { id       = optional "id"
  , datetime = required "datetime"
  , values   = required "values"
  }

all :: Query DataItemColumn
all = queryTable table

queryAll :: MonadIO m => Pg.Connection -> m [DataItem]
queryAll conn = liftIO $ runQuery conn all

insert :: MonadIO m => Pg.Connection -> DataItemColumnW -> m ()
insert conn di = liftIO $ void $ runInsertMany conn table [di]
