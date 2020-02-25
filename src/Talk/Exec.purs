module Talk.Exec where

import Prelude

import Talk.CCRS as CCRS
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect (Effect)
-- import Effect.Class (liftEffect)
-- import Effect.Console (log)

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
spagoCmd cmd file =
  "export TERM=dumb && rm -f ./src/Main.purs && ln -s $PWD/"
  <> file <> " ./src/Main.purs && " <> spago cmd

preludeEffectImports :: String
preludeEffectImports = """module Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log, logShow)

"""

missingFileCmd :: String
missingFileCmd = "echo \"Error: no filename supplied to run file command.\""

-- | Convenience function
mkSysJobIdWithInits :: Array String -> Effect CCRS.JobId
mkSysJobIdWithInits cmds = do
  jobId <- CCRS.mkJobId
  _ <- CCRS.runSysCommands CCRS.mypyPursMeta jobId cmds
  pure jobId


spago :: String -> String
spago scmd = "spago -C " <> scmd

spagoInit :: String
spagoInit = spago "init --force"

-- -- -- mypy or python related commands below -- -- --

-- | Assumes the first file is the file to be run
runPyFile :: Array String -> String
runPyFile files = case head files of
  Just file -> "python " <> file
  Nothing -> missingFileCmd

runMypyFile :: Array String -> String
runMypyFile files = case head files of
  Just file -> "mypy " <> file
  Nothing -> missingFileCmd
