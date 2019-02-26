module HIE.Bios.Debug (debugInfo, rootInfo) where

import CoreMonad (liftIO)

import Data.Maybe (fromMaybe)

import HIE.Bios.GHCApi
import HIE.Bios.Types

----------------------------------------------------------------

-- | Obtaining debug information.
debugInfo :: Options
          -> Cradle
          -> IO String
debugInfo opt cradle = convert opt <$> do
    (_ex, _sterr, gopts) <- getOptions (cradleOptsProg cradle) (cradleRootDir cradle)
    mglibdir <- liftIO getSystemLibDir
    return [
        "Root directory:      " ++ rootDir
      , "Current directory:   " ++ currentDir
      , "GHC options:         " ++ unwords gopts
      , "System libraries:    " ++ fromMaybe "" mglibdir
      ]
  where
    currentDir = cradleCurrentDir cradle
    rootDir    = cradleRootDir cradle

----------------------------------------------------------------

-- | Obtaining root information.
rootInfo :: Options
          -> Cradle
          -> IO String
rootInfo opt cradle = return $ convert opt $ cradleRootDir cradle
