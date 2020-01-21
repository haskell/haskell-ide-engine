{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Haskell.Ide.Engine.GhcModuleCache where

import           Control.Monad.IO.Class ( liftIO , MonadIO(..) )

import qualified Data.Map as Map
import           Data.Dynamic (Dynamic)
import           Data.Typeable (TypeRep)

import qualified HIE.Bios as BIOS
import qualified Data.Trie as T
import qualified Data.ByteString.Char8 as B

import           GHC (TypecheckedModule, ParsedModule, HscEnv)

import Data.List

import Haskell.Ide.Engine.ArtifactMap

import Language.Haskell.LSP.Types

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
  case currentCradle gmc of
    Just (dirs, _c, _) | any (`isPrefixOf` fp) dirs -> ReuseCradle
    _ -> case T.match (cradleCache gmc) (B.pack fp) of
           Just (_k, c, _suf) -> LoadCradle c
           Nothing  -> NewCradle fp

getComponentOptions
  :: (MonadIO m, HasGhcModuleCache m)
  => FilePath
  -> m (Maybe BIOS.ComponentOptions)
getComponentOptions fp = do
  mc <- getModuleCache
  case currentCradle mc of
    Just (dirs, cradle, co) | any (`isPrefixOf` fp) dirs -> case co of
      Just _ -> return co
      _      -> setCo fp cradle setCur
    _ -> case T.match (cradleCache mc) (B.pack fp) of
      Just (_, CachedCradle _ _ (Just co), _) -> return $ Just co
      Just (p, CachedCradle cradle _ Nothing, _) ->
        setCo (B.unpack p) cradle setCac
      _ -> return Nothing
 where
  setCo fp' cradle mod' = do
    res <- liftIO $ BIOS.getCompilerOptions fp' cradle
    case res of
      BIOS.CradleSuccess opts -> do
        modifyModuleCache $ mod' opts fp'
        return $ Just opts
      _ -> return Nothing
  setCur opts _ mc'@(GhcModuleCache _ _ (Just (fps, c', _))) =
    mc' { currentCradle = Just (fps, c', Just opts) }
  setCur _ _ mc' = mc'
  setCac opts fp' mc' = mc'
    { cradleCache = T.adjust (\cc -> cc { mCompOpts = Just opts }) (B.pack fp')
                      $ cradleCache mc'
    }



data CachedCradle = CachedCradle
  { ccradle :: BIOS.Cradle
  , hscEnv :: HscEnv
  , mCompOpts :: Maybe BIOS.ComponentOptions
  }

instance Show CachedCradle where
  show (CachedCradle x _ _) = show x

data GhcModuleCache = GhcModuleCache
  { cradleCache :: !(T.Trie CachedCradle)
              -- ^ map from FilePath to cradles
              -- May not include currentCradle
  , uriCaches  :: !UriCaches
  , currentCradle :: Maybe ([FilePath], BIOS.Cradle, Maybe BIOS.ComponentOptions)
              -- ^ The current cradle and which FilePath's it is
              -- responsible for
  } deriving (Show)

-- ---------------------------------------------------------------------
