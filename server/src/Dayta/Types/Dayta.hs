{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Dayta.Types.Dayta (Dayta, daytaToHandler, withConnection) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader, MonadIO, asks)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Control (MonadBaseControl (..), control)
import Servant (Handler, ServerError)
import Data.Pool (withResource)
import qualified Database.PostgreSQL.Simple as Db

import qualified Dayta.Server.State as Dayta

newtype Dayta a = Dayta { unDayta :: ReaderT Dayta.State Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Dayta.State, MonadError ServerError, MonadIO, MonadBase IO)

instance MonadBaseControl IO Dayta where
  type StM Dayta a = StM (ReaderT Dayta.State Handler) a
  liftBaseWith f = Dayta (liftBaseWith (\run -> f (run . unDayta)))
  restoreM = Dayta . restoreM

runDayta :: Dayta.State -> Dayta a -> Handler a
runDayta st d = runReaderT (unDayta d) st

daytaToHandler :: Dayta.State -> (forall x. Dayta x -> Handler x)
daytaToHandler = runDayta

withConnection :: (Db.Connection -> Dayta a) -> Dayta a
withConnection act = do
  pool <- asks Dayta.dbConnectionPool
  control $ \run -> withResource pool $ run . act
