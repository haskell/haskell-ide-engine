{-# LANGUAGE TemplateHaskell #-}
module HIE.Bios.Cradle (
      findCradle
    , defaultCradle
  ) where

import System.Process
import System.Exit
import HIE.Bios.Types
import System.Directory hiding (findFile)
import Control.Monad.Trans.Maybe
import System.FilePath
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative ((<|>))
import Data.List
import Data.FileEmbed
import System.IO.Temp
import System.IO

import Debug.Trace
import System.Posix.Files

----------------------------------------------------------------

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
findCradle :: FilePath -> IO Cradle
findCradle wfile = do
    let wdir = takeDirectory wfile
    res <- runMaybeT (rulesHaskellCradle wdir <|> biosCradle wdir <|> cabalCradle wdir)
    case res of
      Just c -> return c
      Nothing -> return (defaultCradle wdir)


---------------------------------------------------------------
-- Default cradle has no special options, not very useful for loading
-- modules.

defaultCradle :: FilePath -> Cradle
defaultCradle cur_dir =
  Cradle {
      cradleCurrentDir = cur_dir
    , cradleRootDir = cur_dir
    , cradleOptsProg = CradleAction "default" (const $ return (ExitSuccess, "", []))
    }

---------------------------------------------------------------


-- | Find a cradle by finding an executable `hie-bios` file which will
-- be executed to find the correct GHC options to use.
biosCradle :: FilePath -> MaybeT IO Cradle
biosCradle cur_dir = do
  wdir <- biosDir cur_dir
  traceM "Using bios"
  return Cradle {
      cradleCurrentDir = cur_dir
    , cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "bios" biosAction
  }

biosDir :: FilePath -> MaybeT IO FilePath
biosDir = findFileUpwards ("hie-bios" ==)

biosAction :: FilePath -> IO (ExitCode, String, [String])
biosAction fp = do
  (ex, res, std) <- readProcessWithExitCode (fp </> "hie-bios") [] []
  return (ex, std, words res)

-- Cabal Cradle
-- Works for new-build using the ghc-environment file

cabalCradle :: FilePath -> MaybeT IO Cradle
cabalCradle fp = do
  wdir <- cabalDir fp
  traceM "Using cabal.project"
  return Cradle {
      cradleCurrentDir = fp
    , cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "cabal" (cabalAction wdir)
  }

cabalWrapper :: String
cabalWrapper = $(embedStringFile "wrappers/cabal")

cabalAction :: FilePath -> FilePath -> IO (ExitCode, String, [String])
cabalAction work_dir fp = do
  wrapper_fp <- writeSystemTempFile "wrapper" cabalWrapper
  -- TODO: This isn't portable for windows
  setFileMode wrapper_fp accessModes
  check <- readFile wrapper_fp
  traceM check
  (ex, args, stde) <-
      withCurrentDirectory work_dir (readProcessWithExitCode "cabal" ["v2-repl", "-v0", "-w", wrapper_fp] [])
  return (ex, stde, words args)


cabalDir :: FilePath -> MaybeT IO FilePath
cabalDir = findFileUpwards isCabal
  where
    isCabal name = name == "cabal.project"

----------------------------------------------------------------------------
-- rules_haskell - Thanks for David Smith for helping with this one.

rulesHaskellCradle :: FilePath -> MaybeT IO Cradle
rulesHaskellCradle fp = do
  wdir <- findFileUpwards (== "WORKSPACE") fp
  traceM "Using rules_haskell"
  return Cradle {
    cradleCurrentDir = fp
    , cradleRootDir  = wdir
    , cradleOptsProg   = CradleAction "bazel" (rulesHaskellAction wdir)
    }


bazelCommand = $(embedStringFile "wrappers/bazel")

rulesHaskellAction :: FilePath -> FilePath -> IO (ExitCode, String, [String])
rulesHaskellAction work_dir fp = do
  wrapper_fp <- writeSystemTempFile "wrapper" bazelCommand
  -- TODO: This isn't portable for windows
  setFileMode wrapper_fp accessModes
  check <- readFile wrapper_fp
  traceM check
  let rel_path = makeRelative work_dir fp
  traceM rel_path
  (ex, args, stde) <-
      withCurrentDirectory work_dir (readProcessWithExitCode wrapper_fp [rel_path] [])
  let args'  = filter (/= '\'') args
  let args'' = filter (/= "\"$GHCI_LOCATION\"") (words args')
  return (ex, stde, args'')




-- Looks for the directory with the first cabal.project file
findFileUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwards p dir = do
    cnts <- liftIO $ findFile p dir
    case cnts of
        [] | dir' == dir -> fail "No cabal files"
           | otherwise   -> findFileUpwards p dir'
        _:_          -> return dir
  where
    dir' = takeDirectory dir

findFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFile p dir = getFiles >>= filterM doesPredFileExist
  where
    getFiles = filter p <$> getDirectoryContents dir
    doesPredFileExist file = doesFileExist $ dir </> file



