{-# LANGUAGE CPP             #-}
-- | This module is based on the hie-wrapper.sh script in
-- https://github.com/alanz/vscode-hie-server
module Main where

import           Data.List
import           Data.Foldable
import           HIE.Bios
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.Cradle (findLocalCradle)
import           Haskell.Ide.Engine.Options
import           Haskell.Ide.Engine.Version
import           System.Directory
import           System.Environment
import           System.Process
import           System.Info
import           System.FilePath

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  _opts <- initApp
    "hie-wrapper - Launch the appropriate haskell-ide-engine for a given project"
  progName <- getProgName
  logm $  "run entered for hie-wrapper(" ++ progName ++ ") " ++ hieVersion
  d <- getCurrentDirectory
  logm $ "Current directory:" ++ d
  logm $ "Operating system:" ++ os
  args <- getArgs
  logm $ "args:" ++ show args

  -- Get the cabal directory from the cradle
  cradle <- findLocalCradle (d </> "File.hs")
  let dir = cradleRootDir cradle
  logm $ "Cradle directory:" ++ dir
  setCurrentDirectory dir

  ghcVersion <- getProjectGhcVersion cradle
  logm $ "Project GHC version:" ++ ghcVersion

  let
    hieBin = "hie-" ++ ghcVersion
    backupHieBin =
      case dropWhileEnd (/='.') ghcVersion of
        [] -> "hie"
        xs -> "hie-" ++ init xs
    candidates' = [hieBin, backupHieBin, "hie"]
    candidates = map (++ exeExtension) candidates'

  logm $ "hie exe candidates :" ++ show candidates

  mexes <- traverse findExecutable candidates

  case asum mexes of
    Nothing -> logm $ "cannot find any hie exe, looked for:" ++ intercalate ", " candidates
    Just e -> do
      logm $ "found hie exe at:" ++ e
      logm $ "args:" ++ show args
      logm "launching ....\n\n\n"
      callProcess e args
      logm "done"

-- ---------------------------------------------------------------------
