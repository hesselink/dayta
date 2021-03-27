{-# LANGUAGE OverloadedStrings #-}
module Dayta.DataSet (list, get) where

import Dayta.Types.Username (Username, unUsername)
import Dayta.Types.Dataset (Dataset (Dataset, name, fields), DatasetName (DatasetName))
import Dayta.Types.Dayta (Dayta, withConnection)
import Dayta.Types.Field (Field (Field), FieldName (FieldName))
import qualified Dayta.Db.DataItem as Db
import qualified Dayta.Db.DataSet as Db

list :: Username -> Dayta [DatasetName]
list un = map (DatasetName . Db.unDataset) <$> withConnection (Db.list (Db.Username . unUsername $ un))

get :: Username -> DatasetName -> Dayta Dataset
get _un ds = return Dataset
  { name = ds
  , fields = [ Field (FieldName "weight") ]
  }
