module Haskell.Ide.Types where

import qualified Language.Haskell.GhcMod as GM
import qualified Language.Haskell.GhcMod.Monad.Types as GM

type IdeM a = GM.GhcModT (GM.GmOutT IO) a

data Plugin = Plugin
    { initializeHook :: Maybe (IO ())
    }

defaultPlugin :: Plugin
defaultPlugin = Plugin
    { initializeHook = Nothing
    }
