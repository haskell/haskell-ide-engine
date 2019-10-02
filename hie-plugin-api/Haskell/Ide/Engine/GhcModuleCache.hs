{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Haskell.Ide.Engine.GhcModuleCache where

import qualified Data.Map as Map
import           Data.Dynamic (Dynamic)
import           Data.Typeable (TypeRep)

import qualified HIE.Bios as BIOS
import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as BS
import Crypto.Hash.SHA1

import           GHC (TypecheckedModule, ParsedModule, HscEnv)

import Data.List

import Haskell.Ide.Engine.ArtifactMap

import Language.Haskell.LSP.Types
import Debug.Trace

type UriCaches = Map.Map FilePath UriCacheResult

data UriCacheResult = UriCacheSuccess UriCache
                    | UriCacheFailed
  deriving (Show)

uriCacheState :: UriCacheResult -> String
uriCacheState UriCacheFailed    = "UriCacheFailed"
uriCacheState UriCacheSuccess{} = "UriCacheSuccess"

data UriCache = UriCache
  { cachedInfo   :: !CachedInfo
  , cachedPsMod  :: !ParsedModule
  , cachedTcMod  :: !(Maybe TypecheckedModule)
  -- | Data pertaining to the typechecked module,
  -- not the parsed module
  , cachedData   :: !(Map.Map TypeRep Dynamic)
  }

instance Show UriCache where
  show (UriCache _ _ (Just _) dat) =
    "UriCache { cachedTcMod, cachedData { " ++ show dat ++ " } }"
  show (UriCache _ _ _ dat) =
    "UriCache { cachedPsMod, cachedData { " ++ show dat ++ " } }"

data CachedInfo = CachedInfo
  { locMap         :: !LocMap
  , typeMap        :: !TypeMap
  , moduleMap      :: !ModuleMap
  , defMap         :: !DefMap
  , revMap         :: !(FilePath -> FilePath)
  , newPosToOld    :: !(Position -> Maybe Position)
  , oldPosToNew    :: !(Position -> Maybe Position)
  }

class CacheableModule a where
  fromUriCache :: UriCache -> Maybe a

instance CacheableModule TypecheckedModule where
  fromUriCache (UriCache _ _ mtm _) = mtm

instance CacheableModule ParsedModule where
  fromUriCache (UriCache _ pm _ _) = Just pm

-- ---------------------------------------------------------------------

-- | Given a list of things with their start and end position in the
-- file, return the set of them that cross include the given position,
-- after it is updated based on edits since the last compile.
getThingsAtPos :: CachedInfo -> Position -> [(Position,Position,a)] -> [(Range,a)]
getThingsAtPos cm pos ts =
  case newPosToOld cm pos of
    Nothing   -> []
    Just pos' -> getArtifactsAtPos pos' (genIntervalMap ts)

-- ---------------------------------------------------------------------
-- The following to move into ghc-mod-core

class (Monad m) => HasGhcModuleCache m where
  getModuleCache :: m GhcModuleCache
  setModuleCache :: GhcModuleCache -> m ()

emptyModuleCache :: GhcModuleCache
emptyModuleCache = GhcModuleCache T.empty Map.empty Nothing

data LookupCradleResult = ReuseCradle | LoadCradle CachedCradle | NewCradle FilePath

-- The boolean indicates whether we have to reload the cradle or not
lookupCradle :: FilePath -> GhcModuleCache -> LookupCradleResult
lookupCradle fp gmc = traceShow ("lookupCradle", fp, gmc) $
  case currentCradle gmc of
    Just (dirs, _c) | traceShow ("just", fp, dirs) (any (\d -> d `isPrefixOf` fp) dirs) -> ReuseCradle
    _ -> case T.match  (cradleCache gmc) (B.pack fp) of
           Just (k, c, suf) -> traceShow ("matchjust",k, suf) $ LoadCradle c
           Nothing  -> NewCradle fp

data CachedCradle = CachedCradle BIOS.Cradle HscEnv

instance Show CachedCradle where
  show (CachedCradle x _) = show x

data GhcModuleCache = GhcModuleCache
  { cradleCache :: !(T.Trie CachedCradle)
              -- ^ map from dirs to cradles
  , uriCaches  :: !UriCaches
  , currentCradle :: Maybe ([FilePath], BIOS.Cradle)
              -- ^ The current cradle and which directories it is
              -- responsible for
  } deriving (Show)

-- ---------------------------------------------------------------------
