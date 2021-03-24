{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Dayta.Types.Dataset (Dataset (Dataset, unDataset)) where

import Data.Aeson (ToJSON)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Text (Text)
import Opaleye (Constant, Column, PGText, unsafeCoerceColumn)
import Servant.API (FromHttpApiData (..))

import qualified Dayta.Db.DataItem as Db

newtype Dataset = Dataset { unDataset :: Text }
  deriving (Eq, Show, ToJSON)

instance Default Constant Dataset (Column Db.Dataset) where
  def = dimap unDataset unsafeCoerceColumn (def :: Constant Text (Column PGText))

instance FromHttpApiData Dataset where
  parseUrlPiece = Right . Dataset
