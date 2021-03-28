{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Dayta.DataSet (list, get, createOrUpdate) where

import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Opaleye

import Dayta.Types.Username (Username, unUsername)
import Dayta.Types.Dataset (Dataset (Dataset, name, fields), DatasetName (DatasetName, unDatasetName))
import Dayta.Types.Dayta (Dayta, withConnection)
import Dayta.Types.Field (Field (Field), FieldName (FieldName, unFieldName))
import qualified Dayta.Types.Field as Field
import qualified Dayta.Db.DataSet as Db
import qualified Dayta.Db.Field as Db (Field, Field' (Field), FieldColumnW, unFieldName)
import qualified Dayta.Db.Field as Db.Field
import qualified Dayta.Db.Username as Db

list :: Username -> Dayta [DatasetName]
list un = map (DatasetName . Db.unDatasetName) <$> withConnection (Db.list (Db.Username . unUsername $ un))

get :: Username -> DatasetName -> Dayta (Maybe Dataset)
get un ds = fmap fromDb <$> withConnection (Db.getWithFields (Db.Username . unUsername $ un) (Db.DatasetName . unDatasetName $ ds))

-- TODO upsert not supported by opaleye
createOrUpdate :: Username -> DatasetName -> Dataset -> Dayta ()
createOrUpdate un dn ds = withConnection $ \conn -> liftIO $ Pg.withTransaction conn $
  Db.get dbUn dbDn conn >>= \case
    Nothing -> do
      dsId <- Db.insert dbDs conn
      Db.Field.insert (mkDbFs dsId) conn
    Just oldDbDs -> do
      let dsId = Db.id oldDbDs
      Db.update dsId dbDs conn
      Db.Field.deleteAll dsId conn
      Db.Field.insert (mkDbFs dsId) conn
  where
    dbUn = Db.Username . unUsername $ un
    dbDn = Db.DatasetName . unDatasetName $ dn
    (dbDs, mkDbFs) = toDb un ds

fromDb :: (Db.Dataset, [Db.Field]) -> Dataset
fromDb (ds, fs) = Dataset
  { name = DatasetName . Db.unDatasetName . Db.name $ ds
  , fields = map fieldFromDb fs
  }

toDb :: Username -> Dataset -> (Db.DatasetColumnW, Db.Id -> [Db.FieldColumnW])
toDb un ds =
  ( Db.Dataset
      { Db.id = Nothing
      , Db.username = Opaleye.constant . Db.Username . unUsername $ un
      , Db.name = Opaleye.constant . Db.DatasetName . unDatasetName . name $ ds
      }
  , \dsId -> map (\f ->
           Db.Field
             { Db.Field.id = Nothing
             , Db.Field.datasetId = Opaleye.constant dsId
             , Db.Field.name = Opaleye.constant . Db.Field.FieldName . unFieldName . Field.name $ f
             }
        )
      (fields ds)
  )

fieldFromDb :: Db.Field -> Field
fieldFromDb f = Field
  { Field.name = FieldName . Db.unFieldName . Db.Field.name $ f
  }
