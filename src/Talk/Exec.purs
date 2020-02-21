module Talk.Exec where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Talk.CCRS as CCRS

-- | Assumes the first file is the file to be run
runPyFile :: Array String -> String
runPyFile files = case head files of
  Just file -> "python " <> file
  Nothing -> missingFileCmd


missingFileCmd :: String
missingFileCmd = "echo \"Error: no filename supplied to run file command.\""
