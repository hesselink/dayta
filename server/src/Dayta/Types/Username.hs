{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dayta.Types.Username (Username (Username, unUsername)) where

import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Text (Text)
import Opaleye (Constant, Column, PGText, unsafeCoerceColumn)
import Servant.API (FromHttpApiData (..))

import qualified Dayta.Db.DataItem as Db

newtype Username = Username { unUsername :: Text }

instance Default Constant Username (Column Db.Username) where
  def = dimap unUsername unsafeCoerceColumn (def :: Constant Text (Column PGText))

instance FromHttpApiData Username where
  parseUrlPiece = Right . Username
