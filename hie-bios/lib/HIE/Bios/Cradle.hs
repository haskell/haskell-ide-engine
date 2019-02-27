{-# LANGUAGE TemplateHaskell #-}
module HIE.Bios.Cradle (
    findCradle
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
    res <- runMaybeT (biosCradle wdir <|> cabalCradle wdir)
    case res of
      Just c -> return c
      Nothing -> error "No cradle found"


-- | Find a cradle by finding an executable `hie-bios` file which will
-- be executed to find the correct GHC options to use.
biosCradle :: FilePath -> MaybeT IO Cradle
biosCradle cur_dir = do
  wdir <- biosDir cur_dir
  liftIO $ print "Using hie-bios"
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
  liftIO $ print "Using cabal.project"
  return Cradle {
      cradleCurrentDir = fp
    , cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "cabal" cabalAction
  }

cabalWrapper :: String
cabalWrapper = $(embedStringFile "wrappers/cabal")

cabalAction :: FilePath -> IO (ExitCode, String, [String])
cabalAction fp = do
  wrapper_fp <- writeSystemTempFile "wrapper" cabalWrapper
  -- TODO: This isn't portable for windows
  setFileMode wrapper_fp accessModes
  check <- readFile wrapper_fp
  traceM check
  (ex, args, stde) <-
      withCurrentDirectory fp (readProcessWithExitCode "cabal" ["v2-repl", "-v0", "-w", wrapper_fp] [])
  return (ex, stde, words args)


cabalDir :: FilePath -> MaybeT IO FilePath
cabalDir = findFileUpwards isCabal
  where
    isCabal name = name == "cabal.project"


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



