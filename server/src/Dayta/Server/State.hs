module Dayta.Server.State (State (datasets), emptyState) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Map (Map)
import Data.Text (Text)

import Dayta.Types.DataItem (DataItem)

data State = State
  { datasets :: TVar (Map Text [DataItem])
  }

emptyState :: MonadIO m => m State
emptyState = liftIO $ State <$> newTVarIO mempty
