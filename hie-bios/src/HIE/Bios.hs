-- | The HIE Bios

module HIE.Bios (
  -- * Initialise a session
    Cradle(..)
  , findCradle
  , defaultCradle
  , initializeFlagsWithCradle
  -- * Eliminate a session to IO
  , withGhcT
  ) where

import HIE.Bios.Cradle
import HIE.Bios.Types
import HIE.Bios.GHCApi
