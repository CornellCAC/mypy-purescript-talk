module Talk.CCRS where

import Prelude

import Data.Tuple (Tuple)
import Effect (Effect)
import Foreign.Object as FO
import Web.DOM.Node (Node)
import Web.Event.Event (Event)

-- | A JavaScript CCRS metadata value
foreign import data SysJobMetaData :: Type

foreign import data ExecFun :: Type

foreign import data OneShotOpt :: Type

foreign import data OneShotOptVar :: Type

foreign import data ExecFileOpt :: Type

foreign import data ExecFileOptVar :: Type

foreign import data JobId :: Type

foreign import data FileContents :: Type

foreign import mypyPursMeta :: SysJobMetaData

foreign import mkJobId :: Effect JobId

foreign import makeOneShotCommandWidg :: Node -> Effect OneShotOptVar

foreign import updateOptCmd ::
     OneShotOptVar
  -> SysJobMetaData
  -> JobId
  -> String
  -> Effect OneShotOpt

foreign import makeExecFileCommandWidg :: Node -> Effect ExecFileOptVar

foreign import updateOptFileCmd ::
     ExecFileOptVar
  -> SysJobMetaData
  -> JobId
  -> FileContents
  -> String
  -> Effect ExecFileOpt

-- TODO: need to generalize/refactor this more in CCRS:
foreign import makeCmdHandler ::
     OneShotOptVar
  -> SysJobMetaData
  -> JobId
  -> Effect (Event -> Unit)  -- This should be a keyboard event
-- foreign import mkExecFileJobFun :: JobId -> String -> String -> ExecFun
-- foreign import runExecFun :: ExecFun -> ??


foreign import makeFileContents :: FO.Object String -> FileContents

fileContentsFromArray :: Array (Tuple String String) -> FileContents
fileContentsFromArray fArray = makeFileContents $ FO.fromFoldable fArray

type ExecFileCmd = {
    files :: Array (Tuple String String)
  , command :: Array String -> String
  , meta :: SysJobMetaData
  }






