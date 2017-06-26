{-# LANGUAGE DeriveGeneric #-}
module Dayta.Types.DataItem (DataItem (DataItem, datetime, value)) where

import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Aeson (ToJSON, FromJSON)

data DataItem = DataItem
  { datetime :: UTCTime
  , value    :: Double
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON DataItem
instance FromJSON DataItem
