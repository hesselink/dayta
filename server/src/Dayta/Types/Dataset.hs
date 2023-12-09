{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Dayta.Types.Dataset
  ( DatasetName (DatasetName, unDatasetName)
  , Dataset (Dataset, name, fields)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Opaleye (ToFields, SqlText, unsafeCoerceField)
import Servant.API (FromHttpApiData (..))
import qualified Opaleye as O

import Dayta.Types.Field (Field)
import qualified Dayta.Db.DataSet as Db

newtype DatasetName = DatasetName { unDatasetName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

instance Default ToFields DatasetName (O.Field Db.DatasetName) where
  def = dimap unDatasetName unsafeCoerceField (def :: ToFields Text (O.Field SqlText))

instance FromHttpApiData DatasetName where
  parseUrlPiece = Right . DatasetName

data Dataset = Dataset
  { name   :: DatasetName
  , fields :: [Field]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
