module Main where

import Data.Proxy (Proxy (Proxy))

import qualified Servant
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Dayta.Api (Api)
import Dayta.Server (server)
import qualified Dayta.Server.State as Dayta

main :: IO ()
main = do
  st <- Dayta.emptyState
  Warp.run 8080 (app st)

app :: Dayta.State -> Wai.Application
app = Servant.serve (Proxy :: Proxy Api) . server
