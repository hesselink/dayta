{-# LANGUAGE OverloadedStrings #-}
module Dayta.DataItem (create, createMany, list, deleteAll) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as Json
import qualified Data.HashMap.Lazy as Map
import qualified Database.PostgreSQL.Simple as Pg
import qualified Opaleye as O

import Dayta.Types.DataItem (DataItem (DataItem, datetime, value))
import Dayta.Types.Dayta (Dayta, withConnection)
import Dayta.Types.Dataset (DatasetName, unDatasetName)
import Dayta.Types.Username (Username, unUsername)
import qualified Dayta.Db.DataItem as Db
import qualified Dayta.Db.DataSet as Db.Dataset
import qualified Dayta.Db.Username as Db


toDb :: Username -> Db.Dataset.Id -> DataItem -> Db.DataItemColumnW
toDb username datasetId di = Db.DataItem
  { Db.id = Nothing
  , Db.datetime = O.toFields (datetime di)
  , Db.values = O.toFields (Json.object ["value" .= value di])
  , Db.username = O.toFields username
  , Db.datasetId = O.toFields datasetId
  }

fromDb :: Db.DataItem -> DataItem
fromDb di = DataItem
  { datetime = Db.datetime di
  , value = fromValues (Db.values di)
  }
  where
    fromValues (Json.Object o) =
      case Json.fromJSON (fromMaybe (error "No value key found in data item.") (Map.lookup "value" o)) of
        Json.Error str -> error ("Error parsing value field in data item: " ++ str)
        Json.Success v -> v
    fromValues _               = error "No json object found when reading data item."

-- TODO 404 if missing
create :: Username -> DatasetName -> DataItem -> Dayta ()
create username dataset di = withConnection $ \conn -> liftIO $ Pg.withTransaction conn $ do
  mDs <- Db.Dataset.get
           (Db.Username . unUsername $ username)
           (Db.Dataset.DatasetName . unDatasetName $ dataset)
           conn
  forM_ mDs $ \ds -> Db.insert conn [toDb username (Db.Dataset.id ds) di]

-- TODO 404 if missing
createMany :: Username -> DatasetName -> [DataItem] -> Dayta ()
createMany username dataset dis = withConnection $ \conn -> liftIO $ Pg.withTransaction conn $ do
  mDs <- Db.Dataset.get
           (Db.Username . unUsername $ username)
           (Db.Dataset.DatasetName . unDatasetName $ dataset)
           conn
  forM_ mDs $ \ds -> Db.insert conn (map (toDb username (Db.Dataset.id ds)) dis)

list :: Username -> DatasetName -> Dayta [DataItem]
list username dataset = withConnection $ \conn -> do
  fmap fromDb <$>
    Db.queryBy (Db.Username . unUsername $ username) (Db.Dataset.DatasetName . unDatasetName $ dataset) conn

deleteAll :: Username -> DatasetName -> Dayta ()
deleteAll username dataset = withConnection $
  Db.deleteAll (Db.Username . unUsername $ username) (Db.Dataset.DatasetName . unDatasetName $ dataset)
