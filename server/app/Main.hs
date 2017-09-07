module Main where

import Data.Proxy (Proxy (Proxy))

import Data.Pool (createPool)
import qualified Database.PostgreSQL.Simple as Db
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import qualified Servant

import Dayta.Api (Api')
import Dayta.Server (server)
import qualified Dayta.Server.State as Dayta

import qualified Config as Config

main :: IO ()
main = do
  cfg <- Config.get
  pool <- createPool (Db.connect (Config.dbConnectInfo cfg)) Db.close 1 10 100
  let st = Dayta.State
        { Dayta.dbConnectionPool = pool
        }
  Warp.run 8080 (app st)

app :: Dayta.State -> Wai.Application
app = Servant.serve (Proxy :: Proxy Api') . server
