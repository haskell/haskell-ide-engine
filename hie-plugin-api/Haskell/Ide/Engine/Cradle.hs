module Haskell.Ide.Engine.Cradle (findLocalCradle) where

import HIE.Bios as BIOS

findLocalCradle :: FilePath -> IO Cradle
findLocalCradle fp = do
  -- Get the cabal directory from the cradle
  cradleConf <- BIOS.findCradle fp
  case cradleConf of
    Just yaml -> BIOS.loadCradle yaml
    Nothing -> BIOS.loadImplicitCradle fp