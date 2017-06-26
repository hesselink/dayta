module Dayta.Server (server) where

import Servant
import Servant.Server (Server)

import Dayta.Types.Dayta (Dayta, daytaToHandler)
import Dayta.Api (Api)
import qualified Dayta.Handler.DataItem as DataItem
import qualified Dayta.Server.State as Dayta

server :: Dayta.State -> Server Api
server st = enter (daytaToHandler st) server'

server' :: ServerT Api Dayta
server' username dataset = DataItem.list username dataset :<|> DataItem.create username dataset
