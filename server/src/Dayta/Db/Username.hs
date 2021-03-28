{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dayta.Db.Username (Username, Username' (Username, unUsername)) where

import Data.Profunctor (dimap)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Profunctor.Product.Default (Default (..))
import Data.Text (Text)
import Opaleye
import qualified Database.PostgreSQL.Simple.FromField as Pg

newtype Username' a = Username { unUsername :: a } deriving (Show, Eq, Ord)
type Username = Username' Text

makeAdaptorAndInstance "pUsername" ''Username'

instance Default Constant Username (Column Username) where
  def = dimap unUsername unsafeCoerceColumn (def :: Constant Text (Column PGText))

instance Pg.FromField Username where
  fromField fName mData = Username <$> Pg.fromField fName mData

instance QueryRunnerColumnDefault Username Username where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
