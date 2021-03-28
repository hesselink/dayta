{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Dayta.Handler.DataSet (upload, delete, list, get, createOrUpdate) where

import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import Servant (err400, err404, errBody, throwError)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Csv as Csv
import qualified Data.Vector as Vector

import Dayta.Types.Username (Username)
import Dayta.Types.Dataset (Dataset, DatasetName)
import Dayta.Types.Dayta (Dayta)
import qualified Dayta.DataItem as DataItem
import qualified Dayta.DataSet as Domain

upload :: Username -> DatasetName -> ByteString -> Dayta ()
upload username dataset body = do
  liftIO (print ("xxx", body))
  case Csv.decode Csv.HasHeader (Lazy.fromStrict body) of
    Left err -> throwError $ err400 { errBody = UTF8.fromString $ "Error parsing CSV: " ++ err }
    Right rs -> do
      let dis = Vector.toList $ rs
      DataItem.createMany username dataset dis

delete :: Username -> DatasetName -> Dayta ()
delete = DataItem.deleteAll

list :: Username -> Dayta [DatasetName]
list = Domain.list

get :: Username -> DatasetName -> Dayta Dataset
get un dn = Domain.get un dn >>= \case
  Just ds -> return ds
  Nothing -> throwError $ err404 { errBody = "Dataset not found." }

createOrUpdate :: Username -> DatasetName -> Dataset -> Dayta ()
createOrUpdate = Domain.createOrUpdate
