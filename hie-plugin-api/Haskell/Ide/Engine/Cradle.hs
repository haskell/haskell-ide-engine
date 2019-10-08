module Haskell.Ide.Engine.Cradle (findLocalCradle) where

import HIE.Bios as BIOS

-- | Find the cradle that the given File belongs to.
--
-- First looks for a "hie.yaml" file in the directory of the file
-- or one of its parents. If this file is found, the cradle
-- is read from the config. If this config does not comply to the "hie.yaml"
-- specification, an error is raised.
--
-- If no "hie.yaml" can be found, the implicit config is used.
-- The implicit config uses different heuristics to determine the type
-- of the project that may or may not be accurate.
findLocalCradle :: FilePath -> IO Cradle
findLocalCradle fp = do
  -- Get the cabal directory from the cradle
  cradleConf <- BIOS.findCradle fp
  case cradleConf of
    Just yaml -> BIOS.loadCradle yaml
    Nothing -> BIOS.loadImplicitCradle fp