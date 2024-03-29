module Main where

import Data.Proxy (Proxy (Proxy))

import Data.Pool (newPool, defaultPoolConfig)
import qualified Database.PostgreSQL.Simple as Db
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import qualified Servant

import Dayta.Api (Api')
import Dayta.Server (server)
import Dayta.Db.Migration (migrate)
import qualified Dayta.Server.State as Dayta

import qualified CmdLine as CmdLine
import qualified Config as Config

main :: IO ()
main = do
  cmdOpts <- CmdLine.getOpts
  cfg <- Config.get (CmdLine.configFile cmdOpts)
  pool <- newPool (defaultPoolConfig (Db.connect (Config.dbConnectInfo cfg)) Db.close 10 100)
  migrate pool (Config.migrationVerbose_ cfg)
  let st = Dayta.State
        { Dayta.dbConnectionPool = pool
        , Dayta.staticFileDir = Config.staticFileDir_ cfg
        }
  Warp.run 8080 (app st)

app :: Dayta.State -> Wai.Application
app = Servant.serve (Proxy :: Proxy Api') . server
