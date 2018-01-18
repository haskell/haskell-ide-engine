{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Haskell.Ide.Engine.ModuleCache where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control

import qualified Data.Map as Map
import           Data.Dynamic (Dynamic, toDyn, fromDynamic)
import           Data.Typeable (TypeRep, Typeable)
import           Data.Generics (Proxy(..), typeRep, typeOf)

import           Exception (ExceptionMonad )

import qualified GhcMod.Cradle                     as GM
import qualified GhcMod.Monad                      as GM
import qualified GhcMod.Types                      as GM

import           GHC                               (TypecheckedModule)

import Haskell.Ide.Engine.ArtifactMap
import Haskell.Ide.Engine.PluginTypes

import           System.Directory
import           System.FilePath

type UriCaches = Map.Map FilePath UriCache

data UriCache = UriCache
  { cachedModule :: !CachedModule
  , cachedData   :: !(Map.Map TypeRep Dynamic)
  } deriving Show

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

modifyCache :: (HasGhcModuleCache m) => (GhcModuleCache -> GhcModuleCache) -> m ()
modifyCache f = do
  mc <- getModuleCache
  setModuleCache (f mc)

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
-- | Runs an IdeM action with the given Cradle
withCradle :: (GM.GmEnv m) => GM.Cradle -> m a -> m a
withCradle crdl =
  GM.gmeLocal (\env -> env {GM.gmCradle = crdl})

-- ---------------------------------------------------------------------
-- | Runs an action in a ghc-mod Cradle found from the
-- directory of the given file. If no file is found
-- then runs the action in the default cradle.
-- Sets the current directory to the cradle root dir
-- in either case
runActionWithContext :: (Monad m, GM.GmEnv m, GM.MonadIO m, HasGhcModuleCache m
                        , GM.GmLog m, MonadBaseControl IO m, ExceptionMonad m, GM.GmOut m)
                     => Maybe FilePath -> m a -> m a
runActionWithContext Nothing action = do
  crdl <- GM.cradle
  liftIO $ setCurrentDirectory $ GM.cradleRootDir crdl
  action
runActionWithContext (Just uri) action = do
  crdl <- getCradle uri
  liftIO $ setCurrentDirectory $ GM.cradleRootDir crdl
  withCradle crdl action

-- | Returns all the cached modules in the IdeState
cachedModules :: GhcModuleCache -> Map.Map FilePath CachedModule
cachedModules = fmap cachedModule . uriCaches

-- | Get the Cradle that should be used for a given URI
getCradle :: (GM.GmEnv m, GM.MonadIO m, HasGhcModuleCache m, GM.GmLog m
             , MonadBaseControl IO m, ExceptionMonad m, GM.GmOut m)
          => FilePath -> m GM.Cradle
getCradle fp = do
      dir <- liftIO $ takeDirectory <$> canonicalizePath fp
      mcache <- getModuleCache
      let mcradle = (Map.lookup dir . cradleCache) mcache
      case mcradle of
        Just crdl ->
          return crdl
        Nothing -> do
          opts <- GM.options
          crdl <- GM.findCradle' (GM.optPrograms opts) dir
          -- debugm $ "cradle cache miss for " ++ dir ++ ", generating cradle " ++ show crdl
          modifyCache (\s -> s { cradleCache = Map.insert dir crdl (cradleCache s)})
          return crdl


-- | looks up a CachedModule for a given URI
getCachedModule :: (Monad m, GM.MonadIO m, HasGhcModuleCache m)
                => FilePath -> m (Maybe CachedModule)
getCachedModule uri = do
  uri' <- liftIO $ canonicalizePath uri
  mc <- getModuleCache
  return $ (Map.lookup uri' . cachedModules) mc

-- | Version of `withCachedModuleAndData` that doesn't provide
-- any extra cached data
withCachedModule :: (Monad m, GM.MonadIO m, HasGhcModuleCache m)
                 => FilePath -> m b -> (CachedModule -> m b) -> m b
withCachedModule uri noCache callback = do
  mcm <- getCachedModule uri
  case mcm of
    Nothing -> noCache
    Just cm -> callback cm

-- | Calls its argument with the CachedModule for a given URI
-- along with any data that might be stored in the ModuleCache.
-- The data is associated with the CachedModule and its cache is
-- invalidated when a new CachedModule is loaded.
-- If the data doesn't exist in the cache, new data is generated
-- using by calling the `cacheDataProducer` function
withCachedModuleAndData :: forall a b m.
  (ModuleCache a, Monad m, GM.MonadIO m, HasGhcModuleCache m)
  => FilePath -> m b -> (CachedModule -> a -> m b) -> m b
withCachedModuleAndData uri noCache callback = do
  uri' <- liftIO $ canonicalizePath uri
  mcache <- getModuleCache
  let mc = (Map.lookup uri' . uriCaches) mcache
  case mc of
    Nothing -> noCache
    Just UriCache{cachedModule = cm, cachedData = dat} -> do
      let proxy :: Proxy a
          proxy = Proxy
      a <- case Map.lookup (typeRep proxy) dat of
             Nothing -> do
               val <- cacheDataProducer cm
               let dat' = Map.insert (typeOf val) (toDyn val) dat
               modifyCache (\s -> s {uriCaches = Map.insert uri' (UriCache cm dat')
                                                                 (uriCaches s)})
               return val
             Just x ->
               case fromDynamic x of
                 Just val -> return val
                 Nothing  -> error "impossible"
      callback cm a

-- | Saves a module to the cache
cacheModule :: (Monad m, GM.MonadIO m, HasGhcModuleCache m)
            => FilePath -> CachedModule -> m ()
cacheModule uri cm = do
  uri' <- liftIO $ canonicalizePath uri
  modifyCache (\s -> s { uriCaches = Map.insert uri' (UriCache cm Map.empty)
                                                     (uriCaches s) })

-- | Deletes a module from the cache
deleteCachedModule :: (Monad m, GM.MonadIO m, HasGhcModuleCache m) => FilePath -> m ()
deleteCachedModule uri = do
  uri' <- liftIO $ canonicalizePath uri
  modifyCache (\s -> s { uriCaches = Map.delete uri' (uriCaches s) })

-- ---------------------------------------------------------------------
-- | A ModuleCache is valid for the lifetime of a CachedModule
-- It is generated on need and the cache is invalidated
-- when a new CachedModule is loaded.
-- Allows the caching of arbitary data linked to a particular
-- TypecheckedModule.
-- TODO: this name is confusing, given GhcModuleCache. Change it
class Typeable a => ModuleCache a where
    -- | Defines an initial value for the state extension
    cacheDataProducer :: (MonadIO m) => CachedModule -> m a

instance ModuleCache () where
    cacheDataProducer = const $ return ()
