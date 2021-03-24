{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses #-}
module Dayta.Db.Validate (validate) where

import Data.Proxy (Proxy (Proxy))
import Data.List (find)
import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor (Profunctor (..))
import Data.Profunctor.Product (ProductProfunctor (..))
import Opaleye (Table (..), IsSqlType (showSqlType), Column)
import Opaleye.Internal.Column (Column (Column))
import Opaleye.Internal.Table (View (View), tableColumns, tableColumnsView)
import Opaleye.Internal.HaskellDB.PrimQuery (PrimExpr (BaseTableAttrExpr))
import qualified Database.PostgreSQL.Simple as Pg

import Dayta.Db.ColumnInfo (ColumnInfo, queryByTable, queryBySchemaAndTable)
import qualified Dayta.Db.ColumnInfo as ColumnInfo

data AnyTable where
  AnyTable :: Default GetExpectedColumnInfo vCols vCols => Table wCols vCols -> AnyTable

validate :: Pg.Connection -> [AnyTable] -> IO [ValidationError]
validate conn = fmap concat . mapM (validateOne conn)

validateOne :: Pg.Connection -> AnyTable -> IO [ValidationError]
validateOne conn (AnyTable (table :: Table wCols vCols)) = do
  ci <- queryColumnInfo conn table
  let View view = tableColumnsView (tableColumns table)
      GetExpectedColumnInfo expected = def :: GetExpectedColumnInfo vCols vCols
  print ci
  return $ concatMap (validateColumn ci) (expected view)

data ExpectedColumnInfo = ExpectedColumnInfo
  { name  :: String
  , type_ :: String
  } deriving Show

data GetExpectedColumnInfo a b = GetExpectedColumnInfo (a -> [ExpectedColumnInfo])

instance Profunctor GetExpectedColumnInfo where
  dimap f _ (GetExpectedColumnInfo mkCi) = GetExpectedColumnInfo (mkCi . f)

instance ProductProfunctor GetExpectedColumnInfo where
  purePP _ = GetExpectedColumnInfo (\_ -> [])
  GetExpectedColumnInfo mkCi1 **** GetExpectedColumnInfo mkCi2 =
    GetExpectedColumnInfo (\a -> mkCi1 a ++ mkCi2 a)

instance IsSqlType a => Default GetExpectedColumnInfo (Column a) (Column a) where
  def = GetExpectedColumnInfo $ \(Column c) ->
    [ExpectedColumnInfo (getColName c) (showSqlType (Proxy :: Proxy a))]

getColName :: PrimExpr -> String
getColName (BaseTableAttrExpr n) = n
getColName _ = error "Table should contain only BaseTableAttrExpr"

validateColumn :: [ColumnInfo] -> ExpectedColumnInfo -> [ValidationError]
validateColumn ci expected =
  let n = name expected
  in maybe [ColumnMissing n] (const []) $ find (\c -> ColumnInfo.name c == n) ci

data ValidationError = ColumnMissing String
  deriving (Eq, Show)

queryColumnInfo :: Pg.Connection -> Table w v -> IO [ColumnInfo]
queryColumnInfo conn (Table n _) = queryByTable conn n
queryColumnInfo conn (TableWithSchema s n _) = queryBySchemaAndTable conn s n
