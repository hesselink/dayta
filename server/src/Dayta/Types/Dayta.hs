{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Dayta.Types.Dayta (Dayta, daytaToHandler, withConnection) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader, MonadIO, asks)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Servant (Handler, (:~>)(NT))
import Data.Pool (withResource)
import qualified Database.PostgreSQL.Simple as Db

import qualified Dayta.Server.State as Dayta

newtype Dayta a = Dayta { unDayta :: ReaderT Dayta.State Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Dayta.State, MonadIO, MonadBase IO)

instance MonadBaseControl IO Dayta where
  type StM Dayta a = StM (ReaderT Dayta.State Handler) a
  liftBaseWith f = Dayta (liftBaseWith (\run -> f (run . unDayta)))
  restoreM = Dayta . restoreM

runDayta :: Dayta.State -> Dayta a -> Handler a
runDayta st d = runReaderT (unDayta d) st

daytaToHandler :: Dayta.State -> Dayta :~> Handler
daytaToHandler st = NT $ runDayta st

withConnection :: (Db.Connection -> Dayta a) -> Dayta a
withConnection act = do
  pool <- asks Dayta.dbConnectionPool
  withResource pool act
