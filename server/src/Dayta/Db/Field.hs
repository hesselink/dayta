{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dayta.Db.Field
  ( Id
  , FieldName
  , FieldName' (FieldName, unFieldName)
  , Field
  , FieldColumnW
  , Field' (Field, id, datasetId, name)

  , all
  , byDatasetId
  , getByDatasetId
  , insert
  , deleteAll
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
import Opaleye hiding (table, Field)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg

import qualified Dayta.Db.DataSet.Id as Dataset

newtype Id' a = Id { unId :: a } deriving (Show, Eq, Ord)
type Id = Id' Int64

makeAdaptorAndInstance "pId" ''Id'

instance Default ToFields Id (Column Id) where
  def = dimap unId unsafeCoerceColumn (def :: ToFields Int64 (Column PGInt8))

instance Pg.FromField Id where
  fromField fName mData = Id <$> Pg.fromField fName mData

instance DefaultFromField Id Id where
  defaultFromField = fromPGSFromField

newtype FieldName' a = FieldName { unFieldName :: a } deriving (Show, Eq, Ord)
type FieldName = FieldName' Text

makeAdaptorAndInstance "pFieldName" ''FieldName'

instance Default ToFields FieldName (Column FieldName) where
  def = dimap unFieldName unsafeCoerceColumn (def :: ToFields Text (Column PGText))

instance Pg.FromField FieldName where
  fromField fName mData = FieldName <$> Pg.fromField fName mData

instance DefaultFromField FieldName FieldName where
  defaultFromField = fromPGSFromField

data Field' a b c = Field
  { id        :: a
  , datasetId :: b
  , name      :: c
  } deriving Show

makeAdaptorAndInstance "pField" ''Field'

type Field = Field' Id Dataset.Id FieldName
type FieldColumn = Field' (Column Id) (Column Dataset.Id) (Column FieldName)
type FieldColumnW = Field' (Maybe (Column Id)) (Column Dataset.Id) (Column FieldName)

table :: Table FieldColumnW FieldColumn
table = Table "dataset_field" $ pField Field
  { id        = optionalTableField "id"
  , datasetId = requiredTableField "dataset_id"
  , name      = requiredTableField "field_name"
  }

all :: Query FieldColumn
all = selectTable table

byDatasetId :: Dataset.Id -> Query FieldColumn
byDatasetId ds = keepWhen (\t -> datasetId t .== toFields ds) . all

getByDatasetId :: MonadIO m => Dataset.Id -> Pg.Connection -> m [Field]
getByDatasetId dsId conn = liftIO $ runSelect conn (byDatasetId dsId)

insert :: MonadIO m => [FieldColumnW] -> Pg.Connection -> m ()
insert fs conn = void $ liftIO (runInsert_ conn (Insert table fs rCount Nothing))

deleteAll :: MonadIO m => Dataset.Id -> Pg.Connection -> m ()
deleteAll dsId conn = void $ liftIO $
    runDelete_ conn Delete
      { dTable = table
      , dWhere = \fs -> datasetId fs .== toFields dsId
      , dReturning = rCount
      }
