{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
#if __GLASGOW_HASKELL__ < 804
import           Data.Semigroup
#endif
import           Data.List
import           Data.Maybe
import           Data.Version                          (showVersion)
import           Development.GitRev              (gitCommitCount)
import           Distribution.Parsec.Class
import           Distribution.System             (buildArch)
import           Distribution.Text               (display)
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.Plugin.Base
import           Haskell.Ide.Engine.PluginDescriptor
import           Options.Applicative.Simple
import qualified Paths_haskell_ide_engine              as Meta
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import qualified System.Log.Formatter as L
import qualified System.Log.Handler as LH
import qualified System.Log.Handler.Simple as LHS
import           System.Log.Logger
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
            "haskell-ide-engine - Provide a common engine to power any Haskell IDE"
            ""
            (numericVersion <*> compiler <*> globalOptsParser)
            empty

    run global

-- ---------------------------------------------------------------------

data GlobalOpts = GlobalOpts
  { optDebugOn     :: Bool
  , optLogFile     :: Maybe String
  , optLsp         :: Bool
  , projectRoot    :: Maybe String
  , optGhcModVomit :: Bool
  , optEkg         :: Bool
  , optEkgPort     :: Int
  } deriving (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> switch
       ( long "debug"
      <> short 'd'
      <> help "Generate debug output"
       )
  <*> (optional $ strOption
       ( long "logfile"
      <> short 'l'
      <> metavar "LOGFILE"
      <> help "File to log to, defaults to stdout"
       ))
  <*> flag False True
       ( long "lsp"
       <> help "Enable the Language Server Protocol transport on STDIO")
  <*> (optional $ strOption
       ( long "project-root"
      <> short 'r'
      <> metavar "PROJECTROOT"
      <> help "Root directory of project, defaults to cwd"))
  <*> flag False True
       ( long "vomit"
       <> help "enable vomit logging for ghc-mod")
  <*> flag False True
       ( long "ekg"
       <> help "enable ekg collection and display on http://localhost:8000")
  <*> (option auto
       ( long "port"
      <> short 'p'
      <> metavar "PORT"
      <> help "TCP port to use for EKG server. Only used if --ekg is set. Default 8000"
      <> value 8000
       ))

-- ---------------------------------------------------------------------

run :: GlobalOpts -> IO ()
run opts = do
  let mLogFileName = case optLogFile opts of
        Just f  -> Just f
        Nothing -> Nothing

      logLevel = if optDebugOn opts
                   then L.DEBUG
                   else L.INFO

  setupLogger mLogFileName ["hie-wrapper"] logLevel

  origDir <- getCurrentDirectory

  maybe (pure ()) setCurrentDirectory $ projectRoot opts

  progName <- getProgName
  logm $  "run entered for hie-wrapper(" ++ progName ++ ") " ++ version
  d <- getCurrentDirectory
  logm $ "Current directory:" ++ d

  ghcVersionString <- getProjectGhcVersion
  let
    ghcVersion = crackGhcVersion ghcVersionString
  logm $ "Project GHC version:" ++ ghcVersion

  putStrLn $ "Project GHC version:" ++ ghcVersionString ++ ":"
  putStrLn $ "Project GHC version:" ++ ghcVersion ++ ":"

  let
    hieBin = "hie-" ++ ghcVersion
    backupHieBin = "hie-" ++ (reverse $ tail $ dropWhile (/='.') $ reverse ghcVersion)

  putStrLn $ "hie exe candidates :" ++ show (hieBin,backupHieBin)

  me <- findExecutable hieBin
  mbe <- findExecutable backupHieBin
  mfe <- findExecutable "hie"

  case catMaybes [me,mbe,mfe] of
    [] -> do
      logm $ "cannot find any hie exe, looked for:" ++ (intercalate ", " [hieBin, backupHieBin, "hie"])
    (e:_) -> do
      logm $ "found hie exe at:" ++ e
      putStrLn $ "found hie exe at:" ++ e
      args <- getArgs
      putStrLn $ "args:" ++ show args
      callProcess e args
  

-- ---------------------------------------------------------------------

getProjectGhcVersion :: IO String
getProjectGhcVersion = do
 {-

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
 -}
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
crackGhcVersion str = r
  where
    r = reverse $ takeWhile (/=' ') $ tail $ reverse str

-- ---------------------------------------------------------------------

-- copied from LSP Core
setupLogger :: Maybe FilePath -> [String] -> Priority -> IO ()
setupLogger mLogFile extraLogNames level = do

  logStream <- case mLogFile of
    Just logFile -> openFile logFile AppendMode
    Nothing      -> return stderr
  hSetEncoding logStream utf8

  logH <- LHS.streamHandler logStream level

  let logHandle  = logH {LHS.closeFunc = hClose}
      logFormat  = L.tfLogFormatter logFormatDateStr logFormatStr
      logHandler = LH.setFormatter logHandle logFormat

  L.updateGlobalLogger L.rootLoggerName $ L.setHandlers ([] :: [LHS.GenericHandler Handle])
  L.updateGlobalLogger logNameStr $ L.setHandlers [logHandler]
  L.updateGlobalLogger logNameStr $ L.setLevel level

  -- Also route the additional log names to the same log
  forM_ extraLogNames $ \logName -> do
    L.updateGlobalLogger logName $ L.setHandlers [logHandler]
    L.updateGlobalLogger logName $ L.setLevel level

logNameStr :: String
logNameStr = "hie-wrapper"

logFormatStr :: String
logFormatStr = "$time [$tid] - $msg"

logFormatDateStr :: String
logFormatDateStr = "%Y-%m-%d %H:%M:%S%Q"

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
