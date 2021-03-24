module Dayta.Handler.DataSet (upload, delete, list) where

import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import Servant (err400, errBody, throwError)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.Csv as Csv
import qualified Data.Vector as Vector

import Dayta.Types.Username (Username)
import Dayta.Types.Dataset (Dataset)
import Dayta.Types.Dayta (Dayta)
import qualified Dayta.DataItem as DataItem
import qualified Dayta.DataSet as Domain

upload :: Username -> Dataset -> ByteString -> Dayta ()
upload username dataset body = do
  liftIO (print ("xxx", body))
  case Csv.decode Csv.HasHeader (Lazy.fromStrict body) of
    Left err -> throwError $ err400 { errBody = UTF8.fromString $ "Error parsing CSV: " ++ err }
    Right rs -> do
      let dis = Vector.toList $ rs
      DataItem.createMany username dataset dis

delete :: Username -> Dataset -> Dayta ()
delete = DataItem.deleteAll

list :: Username -> Dayta [Dataset]
list = Domain.list
