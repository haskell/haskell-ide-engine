{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module HIE.Bios.Cradle (
      findCradle
    , defaultCradle
  ) where

import System.Process
import System.Exit
import HIE.Bios.Types
import HIE.Bios.Config
import System.Directory hiding (findFile)
import Control.Monad.Trans.Maybe
import System.FilePath
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative ((<|>))
import Data.FileEmbed
import System.IO.Temp

import Debug.Trace
import System.Posix.Files
import System.FilePath.Posix

----------------------------------------------------------------

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
findCradle :: FilePath -> IO Cradle
findCradle wfile = do
    let wdir = takeDirectory wfile
    cfg <- runMaybeT (dhallConfig wdir <|> implicitConfig wdir)
    return $ case cfg of
      Just bc -> getCradle bc
      Nothing -> (defaultCradle wdir)


getCradle :: (CradleConfig, FilePath) -> Cradle
getCradle (cc, wdir) = case cc of
                 Cabal -> cabalCradle wdir
                 Stack -> stackCradle wdir
                 Bazel -> rulesHaskellCradle wdir
                 Obelisk -> obeliskCradle wdir
                 Bios -> biosCradle wdir

implicitConfig :: FilePath -> MaybeT IO (CradleConfig, FilePath)
implicitConfig fp =
    (Obelisk,) <$> obeliskWorkDir fp
     <|> (Bazel,) <$> rulesHaskellWorkDir fp
     <|> (Stack,) <$> stackWorkDir fp
     <|> ((Cabal,) <$> cabalWorkDir fp)

dhallConfig :: FilePath -> MaybeT IO (CradleConfig, FilePath)
dhallConfig fp = do
  wdir <- findFileUpwards ("hie.dhall" ==) fp
  cfg  <- liftIO $ readConfig (wdir </> "hie.dhall")
  return (stringToCC (cradle cfg), wdir)




---------------------------------------------------------------
-- Default cradle has no special options, not very useful for loading
-- modules.

defaultCradle :: FilePath -> Cradle
defaultCradle cur_dir =
  Cradle {
      cradleRootDir = cur_dir
    , cradleOptsProg = CradleAction "default" (const $ return (ExitSuccess, "", []))
    }

-------------------------------------------------------------------------


-- | Find a cradle by finding an executable `hie-bios` file which will
-- be executed to find the correct GHC options to use.
biosCradle :: FilePath -> Cradle
biosCradle wdir = do
  Cradle {
      cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "bios" (biosAction wdir)
  }

biosDir :: FilePath -> MaybeT IO FilePath
biosDir = findFileUpwards (".hie-bios" ==)


biosAction :: FilePath -> FilePath -> IO (ExitCode, String, [String])
biosAction wdir fp = do
  (ex, res, std) <- readProcessWithExitCode (wdir </> ".hie-bios") [fp] []
  return (ex, std, words res)

------------------------------------------------------------------------
-- Cabal Cradle
-- Works for new-build by invoking `v2-repl` does not support components
-- yet.

cabalCradle :: FilePath -> Cradle
cabalCradle wdir = do
  Cradle {
      cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "cabal" (cabalAction wdir)
  }

cabalWrapper :: String
cabalWrapper = $(embedStringFile "wrappers/cabal")

cabalAction :: FilePath -> FilePath -> IO (ExitCode, String, [String])
cabalAction work_dir _fp = do
  wrapper_fp <- writeSystemTempFile "wrapper" cabalWrapper
  -- TODO: This isn't portable for windows
  setFileMode wrapper_fp accessModes
  check <- readFile wrapper_fp
  traceM check
  (ex, args, stde) <-
      withCurrentDirectory work_dir (readProcessWithExitCode "cabal" ["v2-repl", "-v0", "-w", wrapper_fp] [])
  return (ex, stde, words args)


cabalWorkDir :: FilePath -> MaybeT IO FilePath
cabalWorkDir = findFileUpwards isCabal
  where
    isCabal name = name == "cabal.project"

------------------------------------------------------------------------
-- Stack Cradle
-- Works for by invoking `stack repl` with a wrapper script

stackCradle :: FilePath -> Cradle
stackCradle wdir =
  Cradle {
      cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "stack" (stackAction wdir)
  }

-- Same wrapper works as with cabal
stackWrapper :: String
stackWrapper = $(embedStringFile "wrappers/cabal")

stackAction :: FilePath -> FilePath -> IO (ExitCode, String, [String])
stackAction work_dir fp = do
  wrapper_fp <- writeSystemTempFile "wrapper" stackWrapper
  -- TODO: This isn't portable for windows
  setFileMode wrapper_fp accessModes
  check <- readFile wrapper_fp
  traceM check
  (ex, args, stde) <-
      withCurrentDirectory work_dir (readProcessWithExitCode "stack" ["repl", "--silent", "--no-load", "--with-ghc", wrapper_fp, fp ] [])
  (ex, pkg_args, stdr) <-
      withCurrentDirectory work_dir (readProcessWithExitCode "stack" ["path", "--ghc-package-path"] [])
  let split_pkgs = splitSearchPath (init pkg_args)
      pkg_ghc_args = concatMap (\p -> ["-package-db", p] ) split_pkgs
      ghc_args = words args ++ pkg_ghc_args
  return (ex, stde, ghc_args)


stackWorkDir :: FilePath -> MaybeT IO FilePath
stackWorkDir = findFileUpwards isStack
  where
    isStack name = name == "stack.yaml"


----------------------------------------------------------------------------
-- rules_haskell - Thanks for David Smith for helping with this one.
-- Looks for the directory containing a WORKSPACE file
--
rulesHaskellWorkDir :: FilePath -> MaybeT IO FilePath
rulesHaskellWorkDir fp =
  findFileUpwards (== "WORKSPACE") fp

rulesHaskellCradle :: FilePath -> Cradle
rulesHaskellCradle wdir = do
  Cradle {
      cradleRootDir  = wdir
    , cradleOptsProg   = CradleAction "bazel" (rulesHaskellAction wdir)
    }


bazelCommand :: String
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


------------------------------------------------------------------------------
-- Obelisk Cradle
-- Searches for the directory which contains `.obelisk`.

obeliskWorkDir :: FilePath -> MaybeT IO FilePath
obeliskWorkDir fp = do
  -- Find a possible root which will contain the cabal.project
  wdir <- findFileUpwards (== "cabal.project") fp
  -- Check for the ".obelisk" folder in this directory
  check <- liftIO $ doesDirectoryExist (wdir </> ".obelisk")
  unless check (fail "Not obelisk dir")
  return wdir


obeliskCradle :: FilePath -> Cradle
obeliskCradle wdir =
  Cradle {
      cradleRootDir  = wdir
    , cradleOptsProg = CradleAction "obelisk" (obeliskAction wdir)
    }

obeliskAction :: FilePath -> FilePath -> IO (ExitCode, String, [String])
obeliskAction work_dir _fp = do
  (ex, args, stde) <-
      withCurrentDirectory work_dir (readProcessWithExitCode "ob" ["ide-args"] [])
  return (ex, stde, words args)


------------------------------------------------------------------------------
-- Utilities


-- | Searches upwards for the first directory containing a file to match
-- the predicate.
findFileUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwards p dir = do
    cnts <- liftIO $ findFile p dir
    case cnts of
        [] | dir' == dir -> fail "No cabal files"
           | otherwise   -> findFileUpwards p dir'
        _:_          -> return dir
  where
    dir' = takeDirectory dir

-- | Sees if any file in the directory matches the predicate
findFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFile p dir = getFiles >>= filterM doesPredFileExist
  where
    getFiles = filter p <$> getDirectoryContents dir
    doesPredFileExist file = doesFileExist $ dir </> file



