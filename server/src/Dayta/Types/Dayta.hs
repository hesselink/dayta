{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators #-}
module Dayta.Types.Dayta (Dayta, daytaToHandler) where

import Control.Monad.Reader (ReaderT, runReaderT, MonadReader, MonadIO)
import Servant (Handler, (:~>)(NT))

import qualified Dayta.Server.State as Dayta

newtype Dayta a = Dayta { unDayta :: ReaderT Dayta.State Handler a }
  deriving (Functor, Applicative, Monad, MonadReader Dayta.State, MonadIO)

runDayta :: Dayta.State -> Dayta a -> Handler a
runDayta st d = runReaderT (unDayta d) st

daytaToHandler :: Dayta.State -> Dayta :~> Handler
daytaToHandler st = NT $ runDayta st
