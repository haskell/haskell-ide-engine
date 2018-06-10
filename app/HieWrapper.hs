{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Data.List
import           Data.Maybe
import           Data.Version                          (showVersion)
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.Plugin.Base
import qualified Language.Haskell.LSP.Core             as Core
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine              as Meta
import           System.Directory
import           System.Environment
import qualified System.Log.Logger as L
import           System.Process

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    let
        numericVersion :: Parser (a -> a)
        numericVersion =
            infoOption
                (showVersion Meta.version)
                (long "numeric-version" <>
                 help "Show only version number")
        compiler :: Parser (a -> a)
        compiler =
            infoOption
                hieCompilerVersion
                (long "compiler" <>
                 help "Show only compiler and version supported")
    -- Parse the options and run
    (global, ()) <-
        simpleOptions
            version
            "hie-wrapper - Launch the appropriate haskell-ide-engine for a given project"
            ""
            (numericVersion <*> compiler <*> globalOptsParser)
            empty

    run global

-- ---------------------------------------------------------------------

run :: GlobalOpts -> IO ()
run opts = do
  let mLogFileName = case optLogFile opts of
        Just f  -> Just f
        Nothing -> Nothing

      logLevel = if optDebugOn opts
                   then L.DEBUG
                   else L.INFO

  Core.setupLogger mLogFileName ["hie"] logLevel

  maybe (pure ()) setCurrentDirectory $ projectRoot opts

  progName <- getProgName
  logm $  "run entered for hie-wrapper(" ++ progName ++ ") " ++ version
  d <- getCurrentDirectory
  logm $ "Current directory:" ++ d

  ghcVersionString <- getProjectGhcVersion
  let
    ghcVersion = crackGhcVersion ghcVersionString
  logm $ "Project GHC version:" ++ ghcVersion

  let
    hieBin = "hie-" ++ ghcVersion
    backupHieBin = "hie-" ++ (reverse $ tail $ dropWhile (/='.') $ reverse ghcVersion)

  putStrLn $ "hie exe candidates :" ++ show (hieBin,backupHieBin)

  me <- findExecutable hieBin
  mbe <- findExecutable backupHieBin
  mfe <- findExecutable "hie"

  case catMaybes [me,mbe,mfe] of
    [] -> logm $ "cannot find any hie exe, looked for:" ++ intercalate ", " [hieBin, backupHieBin, "hie"]
    (e:_) -> do
      logm $ "found hie exe at:" ++ e
      -- putStrLn $ "found hie exe at:" ++ e
      args <- getArgs
      putStrLn $ "args:" ++ show args
      logm "launching ....\n\n\n"
      callProcess e args
  
-- ---------------------------------------------------------------------

getProjectGhcVersion :: IO String
getProjectGhcVersion = do
  isStack <- doesDirectoryExist ".stack-work"
  cmd <- if isStack
    then do
      logm "Using stack GHC version"
      return "stack ghc -- --version"
    else do
      logm "Using plain GHC --version"
      return "ghc version"
  readCreateProcess (shell cmd) ""


-- "The Glorious Glasgow Haskell Compilation System, version 8.4.3\n"
-- "The Glorious Glasgow Haskell Compilation System, version 8.4.2\n"
crackGhcVersion :: String -> String
crackGhcVersion st = reverse $ takeWhile (/=' ') $ tail $ reverse st

-- ---------------------------------------------------------------------

{-
#!/usr/bin/env bash
DEBUG=1
indent=""
function debug {
  if [[ $DEBUG == 1 ]]; then
    echo "$indent$@" >> /tmp/hie-wrapper.log
  fi
}

curDir=`pwd`
debug "Launching HIE for project located at $curDir"
indent="  "

GHCBIN='ghc'
# If a .stack-work exists, assume we are using stack.
if [ -d ".stack-work" ]; then
  debug "Using stack GHC version"
  GHCBIN='stack ghc --'
else
  debug "Using plain GHC version"
fi
versionNumber=`$GHCBIN --version`
debug $versionNumber

HIEBIN='hie'
BACKUP_HIEBIN='hie'
# Match the version number with a HIE version, and provide a fallback without
# the patch number.

# GHC 8.0.*
if [[ $versionNumber = *"8.0.1"* ]]; then
  debug "Project is using GHC 8.0.1"
  HIEBIN='hie-8.0.1'
  BACKUP_HIEBIN='hie-8.0'
elif [[ $versionNumber = *"8.0.2"* ]]; then
  debug "Project is using GHC 8.0.2"
  HIEBIN='hie-8.0.2'
  BACKUP_HIEBIN='hie-8.0'
elif [[ $versionNumber = *"8.0"* ]]; then
  debug "Project is using GHC 8.0.*"
  HIEBIN='hie-8.0'

# GHC 8.2.*
elif [[ $versionNumber = *"8.2.1"* ]]; then
  debug "Project is using GHC 8.2.1"
  HIEBIN='hie-8.2.1'
  BACKUP_HIEBIN='hie-8.2'
elif [[ $versionNumber = *"8.2.2"* ]]; then
  debug "Project is using GHC 8.2.2"
  HIEBIN='hie-8.2.2'
  BACKUP_HIEBIN='hie-8.2'
elif [[ $versionNumber = *"8.2"* ]]; then
  debug "Project is using GHC 8.2.*"
  HIEBIN='hie-8.2'

# GHC 8.4.*
elif [[ $versionNumber = *"8.4.3"* ]]; then
  debug "Project is using GHC 8.4.3"
  HIEBIN='hie-8.4.3'
  BACKUP_HIEBIN='hie-8.4'
elif [[ $versionNumber = *"8.4.2"* ]]; then
  debug "Project is using GHC 8.4.2"
  HIEBIN='hie-8.4.2'
  BACKUP_HIEBIN='hie-8.4'
elif [[ $versionNumber = *"8.4"* ]]; then
  debug "Project is using GHC 8.4.*"
  HIEBIN='hie-8.4'

else
  debug "WARNING: GHC version does not match any of the checked ones."
fi

if [ -x "$(command -v $HIEBIN)" ]; then
  debug "$HIEBIN was found on path"
elif [ -x "$(command -v $BACKUP_HIEBIN)" ]; then
  debug "Backup $BACKUP_HIEBIN was found on path"
  HIEBIN=$BACKUP_HIEBIN
else
  debug "Falling back to plain hie"
  HIEBIN='hie'
fi

debug "Starting HIE"

# Check that HIE is working
export HIE_SERVER_PATH=`which $HIEBIN`

if [ "X" = "X$HIE_SERVER_PATH" ]; then
  echo "Content-Length: 100\r\n\r"
  echo '{"jsonrpc":"2.0","id":1,"error":{"code":-32099,"message":"Cannot find hie in the path"}}'
  exit 1
fi

# Run directly
$HIEBIN --lsp $@
# $HIEBIN --lsp

# Run with a log
# $HIEBIN --lsp -d -l /tmp/hie.log $@
# $HIEBIN --lsp -d -l /tmp/hie.log --ekg $@
# $HIEBIN --lsp -d -l /tmp/hie.log --vomit $@

# Run with a log and a direct dump of the server output
# $HIEBIN --lsp -d -l /tmp/hie.log | tee /tmp/hie-wire.log

-}
