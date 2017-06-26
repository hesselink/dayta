{-# LANGUAGE DataKinds, TypeOperators #-}
module Dayta.Api (Api) where

import Data.Text (Text)
import Servant.API

import Dayta.Types.DataItem (DataItem)

-- /user/:username/dataset/:dataset/item POST => add new item to dataset
-- /user/:username/dataset/:dataset/item GET => get all dataset items

type Api = "user" :> Capture "username" Text
             :> "dataset" :> Capture "dataset" Text
               :> "item" :> (Get '[JSON] [DataItem] :<|> ReqBody '[JSON] DataItem  :> Post '[JSON] ())
