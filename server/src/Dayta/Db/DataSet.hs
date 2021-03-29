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
import Opaleye hiding (table, Field)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg

import Dayta.Db.DataSet.Id
import Dayta.Db.Username (Username)
import Dayta.Db.Field (Field)
import qualified Dayta.Db.Field as Field

newtype DatasetName' a = DatasetName { unDatasetName :: a } deriving (Show, Eq, Ord)
type DatasetName = DatasetName' Text

makeAdaptorAndInstance "pDatasetName" ''DatasetName'

instance Default Constant DatasetName (Column DatasetName) where
  def = dimap unDatasetName unsafeCoerceColumn (def :: Constant Text (Column PGText))

instance Pg.FromField DatasetName where
  fromField fName mData = DatasetName <$> Pg.fromField fName mData

instance QueryRunnerColumnDefault DatasetName DatasetName where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

data Dataset' a b c = Dataset
  { id       :: a
  , name     :: b
  , username :: c
  } deriving Show

makeAdaptorAndInstance "pDataset" ''Dataset'

type Dataset = Dataset' Id DatasetName Username
type DatasetColumn = Dataset' (Column Id) (Column DatasetName) (Column Username)
type DatasetColumnW = Dataset' (Maybe (Column Id)) (Column DatasetName) (Column Username)

table :: Table DatasetColumnW DatasetColumn
table = Table "dataset" $ pDataset Dataset
  { id       = optional "id"
  , name     = required "name"
  , username = required "username"
  }

all :: Query DatasetColumn
all = queryTable table

byUser :: Username -> Query DatasetColumn
byUser un = keepWhen (\t -> username t .== constant un) . all

by :: Username -> DatasetName -> Query DatasetColumn
by un dn = keepWhen (\t -> name t .== constant dn) . byUser un


list :: MonadIO m => Username -> Pg.Connection -> m [DatasetName]
list un conn = liftIO $ runQuery conn $ fmap name (byUser un)

get :: MonadIO m => Username -> DatasetName -> Pg.Connection -> m (Maybe Dataset)
get un dn conn = listToMaybe <$> liftIO (runQuery conn $ by un dn)

getWithFields :: MonadIO m => Username -> DatasetName -> Pg.Connection -> m (Maybe (Dataset, [Field]))
getWithFields un dn conn = liftIO $ Pg.withTransaction conn $ do
  mDs <- get un dn conn
  forM mDs $ \ds -> (ds,) <$> Field.getByDatasetId (id ds) conn

insert :: MonadIO m => DatasetColumnW -> Pg.Connection ->  m Id
insert ds conn = head <$> liftIO (runInsert_ conn (Insert table [ds] (rReturning id) Nothing))

update :: MonadIO m => Id -> DatasetColumnW -> Pg.Connection -> m ()
update dsId ds conn = void $ liftIO (runUpdate_ conn (Update table (`merge` ds) (\t -> id t .== constant dsId) rCount))
  where
    merge defs writes = writes { id = Just (id defs) } -- TODO generalize?
