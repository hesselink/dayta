{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module Dayta.Db.ColumnInfo
( ColumnInfo' (ColumnInfo, schema, table_, name, nullable, type_)
, ColumnInfo
, ColumnInfoColumn
, ColumnInfoColumnW

, table
, all
, queryAll
, queryByTable
, queryBySchemaAndTable
) where

import Prelude hiding (all, id, (.))

import Control.Category ((.))
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Opaleye ((.==), Column, Constant, PGText, Query,
                QueryRunnerColumnDefault (..), Table (TableWithSchema),
                fieldQueryRunnerColumn, queryTable, required, runQuery,
                unsafeCoerceColumn, keepWhen, constant, ToFields, Constant (..), sqlString, (.&&))

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg

data ColumnInfo' a b c d e = ColumnInfo
  { schema   :: a
  , table_   :: b
  , name     :: c
  , nullable :: d
  , type_    :: e
  } deriving Show

makeAdaptorAndInstance "pColumnInfo" ''ColumnInfo'

newtype YesNo' a = YesNo { unYesNo :: a } deriving (Show, Eq, Ord)
type YesNo = YesNo' String

makeAdaptorAndInstance "pYesNo" ''YesNo'

instance QueryRunnerColumnDefault String String where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant YesNo (Column YesNo) where
  def = dimap unYesNo unsafeCoerceColumn (def :: Constant String (Column PGText))

instance Pg.FromField YesNo where
  fromField fName mData = YesNo <$> Pg.fromField fName mData

instance QueryRunnerColumnDefault YesNo YesNo where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

type ColumnInfo = ColumnInfo' String String String YesNo String
type ColumnInfoColumn = ColumnInfo' (Column String) (Column String) (Column String) (Column YesNo) (Column String)
type ColumnInfoColumnW = ColumnInfoColumn

table :: Table ColumnInfoColumnW ColumnInfoColumn
table = TableWithSchema "information_schema" "columns" $ pColumnInfo ColumnInfo
  { schema   = required "table_schema"
  , table_   = required "table_name"
  , name     = required "column_name"
  , nullable = required "is_nullable"
  , type_    = required "data_type"
  }

all :: Query ColumnInfoColumn
all = queryTable table

byTable :: String -> Query ColumnInfoColumn
byTable tableName = keepWhen (\t -> table_ t .== constant tableName) . all

bySchemaAndTable :: String -> String -> Query ColumnInfoColumn
bySchemaAndTable schemaName tableName = keepWhen (\t -> schema t .== constant schemaName
  .&& table_ t .== constant tableName) . all

queryAll :: MonadIO m => Pg.Connection -> m [ColumnInfo]
queryAll conn = liftIO $ runQuery conn all

queryByTable :: MonadIO m => Pg.Connection -> String -> m [ColumnInfo]
queryByTable conn = liftIO . runQuery conn . byTable

queryBySchemaAndTable :: MonadIO m => Pg.Connection -> String -> String -> m [ColumnInfo]
queryBySchemaAndTable conn s t = liftIO (runQuery conn (bySchemaAndTable s t))

instance Default ToFields String (Column String) where
  def = Constant $ unsafeCoerceColumn . sqlString
