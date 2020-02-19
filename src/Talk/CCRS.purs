module Talk.CCRS where

import Prelude

import Effect (Effect)
import Web.DOM.Node (Node)
import Web.Event.Event (Event)

-- | A JavaScript CCRS metadata value
foreign import data SysJobMetaData :: Type

foreign import data ExecFun :: Type

foreign import data OneShotOptVar :: Type

foreign import data JobId :: Type

foreign import mypyPursMeta :: SysJobMetaData

foreign import mkJobId :: Effect JobId

foreign import makeOneShotCommand :: Node -> Effect OneShotOptVar
-- TODO: need to generalize/refactor this more in CCRS:
foreign import makeCmdHandler ::
     OneShotOptVar
  -> SysJobMetaData
  -> JobId
  -> Effect (Event -> Unit)  -- This should be a keyboard event

-- foreign import mkExecFileJobFun :: JobId -> String -> String -> ExecFun
-- foreign import runExecFun :: ExecFun -> ??




