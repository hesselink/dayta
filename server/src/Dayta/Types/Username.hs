{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dayta.Types.Username (Username (Username, unUsername)) where

import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Text (Text)
import Opaleye (Field, ToFields, SqlText, unsafeCoerceField)
import Servant.API (FromHttpApiData (..))

import qualified Dayta.Db.DataItem as Db

newtype Username = Username { unUsername :: Text }
  deriving (Eq, Show)

instance Default ToFields Username (Field Db.Username) where
  def = dimap unUsername unsafeCoerceField (def :: ToFields Text (Field SqlText))

instance FromHttpApiData Username where
  parseUrlPiece = Right . Username
