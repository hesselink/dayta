{-# LANGUAGE DataKinds, TypeOperators #-}
module Dayta.Api (Api, Api') where

import Data.ByteString (ByteString)
import Servant.API

import Dayta.Types.Dataset (Dataset, DatasetName)
import Dayta.Types.DataItem (DataItem)
import Dayta.Types.Username (Username)

-- /user/:username/dataset/:dataset/item POST => add new item to dataset
-- /user/:username/dataset/:dataset/item GET => get all dataset items

type Api = "user" :> Capture "username" Username
             :> "dataset" :>
               ( Get '[JSON] [DatasetName]
             :<|> Capture "dataset" DatasetName
               :> (  "item" :> (Get '[JSON] [DataItem] :<|> ReqBody '[JSON] DataItem  :> Post '[JSON] ())
                :<|> ReqBody '[OctetStream] ByteString :> Post '[JSON] ()
                :<|> Delete '[JSON] ()
                :<|> Get '[JSON] Dataset
                :<|> ReqBody '[JSON] Dataset :> Put '[JSON] ()
                  )
               )

type Api' =  "api" :> Api
        :<|> "static" :> Raw
        :<|> Raw
