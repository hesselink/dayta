module Dayta.Handler.DataItem (list, create) where

import Control.Monad.Reader (asks, liftIO)
import Control.Concurrent.STM (readTVarIO, modifyTVar, atomically)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Map as Map

import Dayta.Types.Dayta (Dayta)
import Dayta.Types.DataItem (DataItem)
import qualified Dayta.Server.State as State

import Debug.Trace

list :: Text -> Text -> Dayta [DataItem]
list username dataset =
  fromMaybe [] . Map.lookup dataset . traceShowId <$> getDatasets

create :: Text -> Text -> DataItem -> Dayta ()
create username dataset dataitem = traceShow dataitem $
  modifyDatasets (Map.insertWith (++) dataset [dataitem])

getDatasets :: Dayta (Map Text [DataItem])
getDatasets = do
  datasetVar <- asks State.datasets
  liftIO $ readTVarIO datasetVar

modifyDatasets :: (Map Text [DataItem] -> Map Text [DataItem]) -> Dayta ()
modifyDatasets f = do
  datasetVar <- asks State.datasets
  liftIO . atomically . modifyTVar datasetVar $ f
