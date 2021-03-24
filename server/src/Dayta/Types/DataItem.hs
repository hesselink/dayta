{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
module Dayta.Types.DataItem (DataItem (DataItem, datetime, value)) where

import GHC.Generics (Generic)
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Csv as Csv

data DataItem = DataItem
  { datetime :: UTCTime
  , value    :: Double
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON DataItem
instance FromJSON DataItem

instance Csv.FromRecord DataItem
instance Csv.FromField UTCTime where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . UTF8.toString
