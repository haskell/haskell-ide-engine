-- | The Happy Haskell Programming library in low level.

module HIE.Bios.Internal (
  -- * Types
  CompilerOptions(..)
  -- * IO
  , getDynamicFlags
  -- * Targets
  , setTargetFiles
  -- * Logging
  , withLogger
  , setNoWaringFlags
  , setAllWaringFlags
  ) where

import HIE.Bios.GHCApi
import HIE.Bios.Logger
import HIE.Bios.Types
