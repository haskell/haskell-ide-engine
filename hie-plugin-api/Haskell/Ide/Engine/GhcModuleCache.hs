{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Haskell.Ide.Engine.GhcModuleCache where

import qualified Data.ByteString.Char8 as B
import           Data.Dynamic (Dynamic)
import           Data.List
import qualified Data.Map as Map
import qualified Data.Trie as T
import           Data.Typeable (TypeRep)

import qualified HIE.Bios as Bios
import           GHC (TypecheckedModule, ParsedModule, HscEnv)

import           Haskell.Ide.Engine.ArtifactMap
import           Haskell.Ide.Engine.Cradle
import           Language.Haskell.LSP.Types

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

class Monad m => HasGhcModuleCache m where
  getModuleCache :: m GhcModuleCache
  modifyModuleCache :: (GhcModuleCache -> GhcModuleCache) -> m ()

emptyModuleCache :: GhcModuleCache
emptyModuleCache = GhcModuleCache T.empty Map.empty Nothing

data LookupCradleResult = ReuseCradle | LoadCradle CachedCradle | NewCradle FilePath

-- | Lookup for the given File if the module cache has a fitting Cradle.
-- Checks if the File belongs to the current Cradle and if it is,
-- the current Cradle can be reused for the given Module/File.
--
-- If the Module is part of another Cradle that has already been loaded,
-- return the Cradle.
-- Otherwise, a new Cradle for the given FilePath needs to be created.
--
-- After loading, the cradle needs to be set as the current Cradle
-- via 'setCurrentCradle' before the Cradle can be cached via 'cacheCradle'.
lookupCradle :: FilePath -> GhcModuleCache -> LookupCradleResult
lookupCradle fp gmc =
  lookupInCache fp gmc (const $ const ReuseCradle) LoadCradle $ NewCradle fp

-- | Find the cradle wide 'ComponentOptions' that apply to a 'FilePath'
lookupComponentOptions
  :: HasGhcModuleCache m => FilePath -> m (Maybe Bios.ComponentOptions)
lookupComponentOptions fp = do
  gmc <- getModuleCache
  return $ lookupInCache fp gmc (const Just) (Just . compOpts) Nothing

lookupInCache
  :: FilePath
  -> GhcModuleCache
  -> (Bios.Cradle CabalHelper -> Bios.ComponentOptions -> a)
  -- ^ Called when file is in the current cradle
  -> (CachedCradle -> a)
  -- ^ Called when file is a member of a cached cradle
  -> a
  -- ^ Default value to return if a cradle is not found
  -> a
lookupInCache fp gmc cur cached def = case currentCradle gmc of
  Just (dirs, c, co) | any (`isPrefixOf` fp) dirs -> cur c co
  _ -> case T.match (cradleCache gmc) (B.pack fp) of
    Just (_k, c, _suf) -> cached c
    Nothing            -> def

-- | A 'Cradle', it's 'HscEnv' and 'ComponentOptions'
data CachedCradle = CachedCradle
  { ccradle :: Bios.Cradle CabalHelper
  , hscEnv :: HscEnv
  , compOpts :: Bios.ComponentOptions
  }

instance Show CachedCradle where
  show (CachedCradle x _ _) = show x

data GhcModuleCache = GhcModuleCache
  { cradleCache :: !(T.Trie CachedCradle)
              -- ^ map from FilePath to cradle and it's config.
              -- May not include currentCradle
  , uriCaches  :: !UriCaches
  , currentCradle :: Maybe ([FilePath], Bios.Cradle CabalHelper, Bios.ComponentOptions)
              -- ^ The current cradle, it's config,
              -- and which FilePath's it is responsible for.
  } deriving (Show)

-- ---------------------------------------------------------------------
