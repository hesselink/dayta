module Dayta.Handler.DataItem (list, create) where

import qualified Dayta.DataItem as Domain
import Dayta.Types.Dayta (Dayta)
import Dayta.Types.DataItem (DataItem)
import Dayta.Types.Dataset (DatasetName)
import Dayta.Types.Username (Username)

import Debug.Trace

list :: Username -> DatasetName -> Dayta [DataItem]
list = Domain.list

create :: Username -> DatasetName -> DataItem -> Dayta ()
create username dataset dataitem = traceShow dataitem $
  Domain.create username dataset dataitem
