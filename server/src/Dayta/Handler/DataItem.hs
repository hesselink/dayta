module Dayta.Handler.DataItem (list, create) where

import qualified Dayta.DataItem as Domain
import Dayta.Types.Dayta (Dayta)
import Dayta.Types.DataItem (DataItem)
import Dayta.Types.Dataset (Dataset)
import Dayta.Types.Username (Username)

import Debug.Trace

list :: Username -> Dataset -> Dayta [DataItem]
list = Domain.list

create :: Username -> Dataset -> DataItem -> Dayta ()
create username dataset dataitem = traceShow dataitem $
  Domain.create username dataset dataitem
