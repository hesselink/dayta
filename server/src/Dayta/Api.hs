{-# LANGUAGE DataKinds, TypeOperators #-}
module Dayta.Api (Api, Api') where

import Data.ByteString (ByteString)
import Servant.API

import Dayta.Types.Dataset (Dataset)
import Dayta.Types.DataItem (DataItem)
import Dayta.Types.Username (Username)

-- /user/:username/dataset/:dataset/item POST => add new item to dataset
-- /user/:username/dataset/:dataset/item GET => get all dataset items

type Api = "user" :> Capture "username" Username
             :> "dataset" :> Capture "dataset" Dataset
               :> (  "item" :> (Get '[JSON] [DataItem] :<|> ReqBody '[JSON] DataItem  :> Post '[JSON] ())
                :<|> ReqBody '[OctetStream] ByteString :> Put '[JSON] ()
                  )

type Api' =  "api" :> Api
        :<|> "static" :> Raw
        :<|> Raw
