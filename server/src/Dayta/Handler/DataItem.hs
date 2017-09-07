module Dayta.Handler.DataItem (list, create) where

import Data.Text (Text)

import qualified Dayta.DataItem as Domain
import Dayta.Types.Dayta (Dayta)
import Dayta.Types.DataItem (DataItem)

import Debug.Trace

list :: Text -> Text -> Dayta [DataItem]
list _username _dataset =
  Domain.list

create :: Text -> Text -> DataItem -> Dayta ()
create _username _dataset dataitem = traceShow dataitem $
  Domain.create dataitem
