{-# LANGUAGE OverloadedStrings #-}
module Dayta.DataItem (create, list) where

import Data.Maybe (fromMaybe)

import Data.Aeson ((.=))
import qualified Data.Aeson as Json
import qualified Data.HashMap.Lazy as Map
import qualified Opaleye as O

import Dayta.Types.DataItem (DataItem (DataItem, datetime, value))
import Dayta.Types.Dayta (Dayta, withConnection)
import qualified Dayta.Db.DataItem as Db


toDb :: DataItem -> Db.DataItemColumnW
toDb di = Db.DataItem
  { Db.id = Nothing
  , Db.datetime = O.constant (datetime di)
  , Db.values = O.constant (Json.object ["value" .= (value di)])
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

create :: DataItem -> Dayta ()
create di = withConnection $ \conn -> do
  Db.insert conn (toDb di)

list :: Dayta [DataItem]
list = withConnection $ \conn -> do
  fmap fromDb <$> Db.queryAll conn
