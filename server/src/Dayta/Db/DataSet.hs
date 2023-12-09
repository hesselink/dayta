{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Dayta.Db.DataSet
  ( Id
  , Dataset
  , DatasetColumnW
  , Dataset' (Dataset, id, name, username)
  , DatasetName
  , DatasetName' (DatasetName, unDatasetName)

  , by

  , list
  , get
  , getWithFields
  , insert
  , update
  ) where

import Prelude hiding (id, all, (.))

import Control.Category ((.))
import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (listToMaybe)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Opaleye hiding (table, Field, keepWhen)
import qualified Opaleye as Db
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg

import Dayta.Db.DataSet.Id
import Dayta.Db.Username (Username)
import Dayta.Db.Util
import Dayta.Db.Field (Field)
import qualified Dayta.Db.Field as Field

newtype DatasetName' a = DatasetName { unDatasetName :: a } deriving (Show, Eq, Ord)
type DatasetName = DatasetName' Text

makeAdaptorAndInstance "pDatasetName" ''DatasetName'

instance Default ToFields DatasetName (Db.Field DatasetName) where
  def = dimap unDatasetName unsafeCoerceField (def :: ToFields Text (Db.Field SqlText))

instance Pg.FromField DatasetName where
  fromField fName mData = DatasetName <$> Pg.fromField fName mData

instance DefaultFromField DatasetName DatasetName where
  defaultFromField = fromPGSFromField

data Dataset' a b c = Dataset
  { id       :: a
  , name     :: b
  , username :: c
  } deriving Show

makeAdaptorAndInstance "pDataset" ''Dataset'

type Dataset = Dataset' Id DatasetName Username
type DatasetColumn = Dataset' (Db.Field Id) (Db.Field DatasetName) (Db.Field Username)
type DatasetColumnW = Dataset' (Maybe (Db.Field Id)) (Db.Field DatasetName) (Db.Field Username)

table :: Table DatasetColumnW DatasetColumn
table = Db.table "dataset" $ pDataset Dataset
  { id       = optionalTableField "id"
  , name     = requiredTableField "name"
  , username = requiredTableField "username"
  }

all :: Select DatasetColumn
all = selectTable table

byUser :: Username -> Select DatasetColumn
byUser un = keepWhen (\t -> username t .== toFields un) . all

by :: Username -> DatasetName -> Select DatasetColumn
by un dn = keepWhen (\t -> name t .== toFields dn) . byUser un


list :: MonadIO m => Username -> Pg.Connection -> m [DatasetName]
list un conn = liftIO $ runSelect conn $ fmap name (byUser un)

get :: MonadIO m => Username -> DatasetName -> Pg.Connection -> m (Maybe Dataset)
get un dn conn = listToMaybe <$> liftIO (runSelect conn $ by un dn)

getWithFields :: MonadIO m => Username -> DatasetName -> Pg.Connection -> m (Maybe (Dataset, [Field]))
getWithFields un dn conn = liftIO $ Pg.withTransaction conn $ do
  mDs <- get un dn conn
  forM mDs $ \ds -> (ds,) <$> Field.getByDatasetId (id ds) conn

insert :: MonadIO m => DatasetColumnW -> Pg.Connection ->  m Id
insert ds conn = head <$> liftIO (runInsert_ conn (Insert table [ds] (rReturning id) Nothing))

update :: MonadIO m => Id -> DatasetColumnW -> Pg.Connection -> m ()
update dsId ds conn = void $ liftIO (runUpdate_ conn (Update table (`merge` ds) (\t -> id t .== toFields dsId) rCount))
  where
    merge defs writes = writes { id = Just (id defs) } -- TODO generalize?
