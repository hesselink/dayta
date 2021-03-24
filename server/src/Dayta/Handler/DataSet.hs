module Dayta.Handler.DataSet (upload) where

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

upload :: Username -> Dataset -> ByteString -> Dayta ()
upload username dataset body = do
  case Csv.decode Csv.NoHeader (Lazy.fromStrict body) of
    Left err -> throwError $ err400 { errBody = UTF8.fromString $ "Error parsing CSV: " ++ err }
    Right rs -> do
      let dis = Vector.toList $ rs
      DataItem.deleteAll username dataset
      DataItem.createMany username dataset dis
