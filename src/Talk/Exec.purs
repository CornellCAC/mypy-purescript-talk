module Talk.Exec where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))

-- | Assumes the first file is the file to be run
runPsFile :: Array String -> String
runPsFile files = case head files of
  Just file -> spagoCmd "run" file
  Nothing -> missingFileCmd

compilePsFile :: Array String -> String
compilePsFile files = case head files of
  Just file -> spagoCmd "build" file
  Nothing -> missingFileCmd

spagoCmd :: String -> String -> String
spagoCmd cmd file = "rm ./src/Main.purs && ln -s $PWD/" <> file
  <> " ./src/Main.purs && spago " <> cmd

-- | Assumes the first file is the file to be run
runPyFile :: Array String -> String
runPyFile files = case head files of
  Just file -> "python " <> file
  Nothing -> missingFileCmd

missingFileCmd :: String
missingFileCmd = "echo \"Error: no filename supplied to run file command.\""
