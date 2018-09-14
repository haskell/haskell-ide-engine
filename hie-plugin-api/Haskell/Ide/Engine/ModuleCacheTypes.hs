
module Haskell.Ide.Engine.ModuleCacheTypes where

import qualified Data.Map as Map
import Data.Dynamic
import Data.Typeable
import GHC (ParsedModule, TypecheckedModule)
import qualified GhcMod.Cradle as GM
import Haskell.Ide.Engine.ArtifactMap
import Language.Haskell.LSP.Types

-- | The GHC module(s), extra information and custom data for
-- a single module, e.g. Foo.hs.
data UriCache = UriCache
  { cachedInfo   :: !CachedInfo
  , cachedPsMod  :: !ParsedModule
  , cachedTcMod  :: !(Maybe TypecheckedModule)
  , cachedData   :: !(Map.Map TypeRep Dynamic)
  }

instance Show UriCache where
  show (UriCache _ _ (Just _) dat) =
    "UriCache { cachedTcMod, cachedData { " ++ show dat ++ " } }"
  show (UriCache _ _ _ dat) =
    "UriCache { cachedPsMod, cachedData { " ++ show dat ++ " } }"

-- ---------------------------------------------------------------------

-- | Data related to the modules inside 'UriCache'
data CachedInfo = CachedInfo
  { locMap         :: !LocMap
  , typeMap        :: !TypeMap
  , moduleMap      :: !ModuleMap
  , defMap         :: !DefMap
  , revMap         :: !(FilePath -> FilePath)
  , newPosToOld    :: !(Position -> Maybe Position)
  , oldPosToNew    :: !(Position -> Maybe Position)
  }

-- ---------------------------------------------------------------------

-- | A module that can be cached inside a 'UriCache'
class CacheableModule a where
  fromUriCache :: UriCache -> Maybe a

instance CacheableModule TypecheckedModule where
  fromUriCache (UriCache _ _ mtm _) = mtm

instance CacheableModule ParsedModule where
  fromUriCache (UriCache _ pm _ _) = Just pm

-- ---------------------------------------------------------------------
-- The following to move into ghc-mod-core

class (Monad m) => HasIdeCache m where
  getModuleCache :: m IdeCache
  setModuleCache :: IdeCache -> m ()

emptyModuleCache :: IdeCache
emptyModuleCache = IdeCache Map.empty Map.empty

data IdeCache = IdeCache
  { cradleCache :: !(Map.Map FilePath GM.Cradle)
                -- ^ map from dirs to cradles
  , uriCaches   :: !(Map.Map FilePath UriCacheResult)
                -- ^ map from module paths to module caches
  } deriving (Show)

data UriCacheResult = UriCacheSuccess UriCache
                    | UriCacheFailed
  deriving (Show)