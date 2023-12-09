{-# LANGUAGE Arrows #-}
module Dayta.Db.Util (keepWhen) where

import Control.Arrow (returnA)
import Opaleye (Field, SqlBool, SelectArr, restrict)

keepWhen :: (a -> Field SqlBool) -> SelectArr a a
keepWhen p = proc a -> do
  restrict  -< p a
  returnA -< a

