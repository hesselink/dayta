{-# LANGUAGE OverloadedStrings #-}
module Dayta.Server (server) where

import Data.Text (Text)
import Network.Wai.Application.Static (StaticSettings (ssLookupFile), defaultFileServerSettings)
import WaiAppStatic.Types (unsafeToPiece)
import Servant
import Servant.Server (Server)

import Dayta.Types.Dayta (Dayta, daytaToHandler)
import Dayta.Api (Api, Api')
import qualified Dayta.Handler.DataItem as DataItem
import qualified Dayta.Server.State as Dayta

apiServer :: Dayta.State -> Server Api
apiServer st = enter (daytaToHandler st) apiServer'

apiServer' :: ServerT Api Dayta
apiServer' username dataset
   =  DataItem.list username dataset
 :<|> DataItem.create username dataset

server :: Dayta.State -> Server Api'
server st
   =  apiServer st
 :<|> serveDirectoryFileServer "../client/dist/static"
 :<|> serveSingleFile "../client/dist" "index.html"

serveSingleFile :: FilePath -> Text -> Server Raw
serveSingleFile dir file = serveDirectoryWith defaults
  { ssLookupFile = const (ssLookupFile defaults [unsafeToPiece file])
  }
  where
    defaults = defaultFileServerSettings dir
