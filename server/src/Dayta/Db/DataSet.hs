module Dayta.Db.DataSet (list) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Opaleye
import qualified Database.PostgreSQL.Simple as Pg

import Dayta.Db.DataItem (Username, Dataset)
import qualified Dayta.Db.DataItem as DataItem

list :: MonadIO m => Username -> Pg.Connection -> m [Dataset]
list un conn = liftIO $ runQuery conn $ distinct $ fmap DataItem.dataset (DataItem.byUser un)
