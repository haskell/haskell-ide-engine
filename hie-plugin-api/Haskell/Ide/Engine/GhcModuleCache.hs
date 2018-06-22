{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Haskell.Ide.Engine.GhcModuleCache where

import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Dynamic (Dynamic)
import           Data.Typeable (TypeRep)

import qualified GhcMod.Types                      as GM

import           GHC                               (TypecheckedModule)

import Haskell.Ide.Engine.ArtifactMap

import Language.Haskell.LSP.Types

type UriCaches = Map.Map FilePath UriCache

data UriCache = UriCache
  { cachedModule :: !CachedModule
  , cachedData   :: !(Map.Map TypeRep Dynamic)
  , isStale      :: !Bool
  } | UriCacheFailed T.Text deriving Show

data CachedModule = CachedModule
  { tcMod          :: !TypecheckedModule
  , locMap         :: !LocMap
  , typeMap        :: !TypeMap
  , revMap         :: !(FilePath -> FilePath)
  , newPosToOld    :: !(Position -> Maybe Position)
  , oldPosToNew    :: !(Position -> Maybe Position)
  }

instance Show CachedModule where
  show CachedModule{} = "CachedModule { .. }"

-- ---------------------------------------------------------------------
-- The following to move into ghc-mod-core

class (Monad m) => HasGhcModuleCache m where
  getModuleCache :: m GhcModuleCache
  setModuleCache :: GhcModuleCache -> m ()

emptyModuleCache :: GhcModuleCache
emptyModuleCache = GhcModuleCache Map.empty Map.empty

data GhcModuleCache = GhcModuleCache
  { cradleCache :: !(Map.Map FilePath GM.Cradle)
              -- ^ map from dirs to cradles
  , uriCaches  :: !UriCaches
  } deriving (Show)

-- ---------------------------------------------------------------------
