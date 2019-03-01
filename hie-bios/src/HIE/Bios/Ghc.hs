-- | The Happy Haskell Programming library.
--   API for interactive processes

module HIE.Bios.Ghc (
  -- * Converting the Ghc monad to the IO monad
    withGHC
  , withGHC'
  -- * Initializing DynFlags
  , initializeFlagsWithCradle
  -- * Ghc utilities
  -- * Misc
  , getSystemLibDir
  ) where

import HIE.Bios.Check
import HIE.Bios.GHCApi
