{-# LANGUAGE OverloadedStrings #-}
module Dayta.DataItem (create, createMany, list, deleteAll) where

import Data.Maybe (fromMaybe)

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.HashMap.Lazy as Map
import qualified Opaleye as O

import Dayta.Types.DataItem (DataItem (DataItem, datetime, value))
import Dayta.Types.Dayta (Dayta, withConnection)
import Dayta.Types.Dataset (DatasetName, unDatasetName)
import Dayta.Types.Username (Username, unUsername)
import qualified Dayta.Db.DataItem as Db


toDb :: Username -> DatasetName -> DataItem -> Db.DataItemColumnW
toDb username dataset di = Db.DataItem
  { Db.id = Nothing
  , Db.datetime = O.constant (datetime di)
  , Db.values = O.constant (Json.object ["value" .= (value di)])
  , Db.username = O.constant username
  , Db.dataset = O.constant dataset
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
    fromValues _               = error ("No json object found when reading data item.")

create :: Username -> DatasetName -> DataItem -> Dayta ()
create username dataset di = withConnection $ \conn -> do
  Db.insert conn [toDb username dataset di]

createMany :: Username -> DatasetName -> [DataItem] -> Dayta ()
createMany username dataset dis = withConnection $ \conn -> do
  Db.insert conn (map (toDb username dataset) dis)

list :: Username -> DatasetName -> Dayta [DataItem]
list username dataset = withConnection $ \conn -> do
  fmap fromDb <$>
    Db.queryBy (Db.Username . unUsername $ username) (Db.Dataset . unDatasetName $ dataset) conn

deleteAll :: Username -> DatasetName -> Dayta ()
deleteAll username dataset = withConnection $ Db.deleteAll (Db.Username . unUsername $ username) (Db.Dataset . unDatasetName $ dataset)
