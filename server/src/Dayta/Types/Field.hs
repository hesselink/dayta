{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Dayta.Types.Field (Field (Field, name), FieldName (FieldName, unFieldName)) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Field = Field
  { name :: FieldName
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

newtype FieldName = FieldName { unFieldName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON)
