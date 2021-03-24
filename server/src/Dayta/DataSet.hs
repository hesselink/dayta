module Dayta.DataSet (list) where

import Dayta.Types.Username (Username, unUsername)
import Dayta.Types.Dataset (Dataset (Dataset))
import Dayta.Types.Dayta (Dayta, withConnection)
import qualified Dayta.Db.DataItem as Db
import qualified Dayta.Db.DataSet as Db

list :: Username -> Dayta [Dataset]
list un = map (Dataset . Db.unDataset) <$> withConnection (Db.list (Db.Username . unUsername $ un))
