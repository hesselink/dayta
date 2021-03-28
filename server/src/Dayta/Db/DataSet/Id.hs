{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Dayta.Db.DataSet.Id
  ( Id
  , Id' (Id, unId)
  ) where

import Data.Int (Int64)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Opaleye
import qualified Database.PostgreSQL.Simple.FromField as Pg

newtype Id' a = Id { unId :: a } deriving (Show, Eq, Ord)
type Id = Id' Int64

makeAdaptorAndInstance "pId" ''Id'

instance Default Constant Id (Column Id) where
  def = dimap unId unsafeCoerceColumn (def :: Constant Int64 (Column PGInt8))

instance Pg.FromField Id where
  fromField fName mData = Id <$> Pg.fromField fName mData

instance QueryRunnerColumnDefault Id Id where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
