{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Haskell.Ide.Engine.ModuleCache where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Dynamic (toDyn, fromDynamic)
import           Data.Generics (Proxy(..), typeRep, typeOf)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Typeable (Typeable)
import           Exception (ExceptionMonad)
import           System.Directory
import           System.FilePath

import qualified GhcMod.Cradle as GM
import qualified GhcMod.Monad  as GM
import qualified GhcMod.Types  as GM

import           Haskell.Ide.Engine.MultiThreadState
import           Haskell.Ide.Engine.PluginsIdeMonads
import           Haskell.Ide.Engine.GhcModuleCache


-- ---------------------------------------------------------------------

modifyCache :: (HasGhcModuleCache m) => (GhcModuleCache -> GhcModuleCache) -> m ()
modifyCache f = do
  mc <- getModuleCache
  setModuleCache (f mc)

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
runActionWithContext :: (GM.GmEnv m, GM.MonadIO m, HasGhcModuleCache m
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
getCachedModule :: (GM.MonadIO m, HasGhcModuleCache m)
                => FilePath -> m (Maybe CachedModule)
getCachedModule uri = do
  uri' <- liftIO $ canonicalizePath uri
  mc <- getModuleCache
  return $ (Map.lookup uri' . cachedModules) mc

-- | Returns true if there is a CachedModule for a given URI
isCached :: (GM.MonadIO m, HasGhcModuleCache m)
         => FilePath -> m Bool
isCached uri = do
  mc <- getCachedModule uri
  return (isJust mc)

-- | Version of `withCachedModuleAndData` that doesn't provide
-- any extra cached data
withCachedModule :: (GM.MonadIO m, HasGhcModuleCache m)
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
  (ModuleCache a, GM.MonadIO m, HasGhcModuleCache m, MonadMTState IdeState m)
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
             Just x -> do
               case fromDynamic x of
                 Just val -> return val
                 Nothing  -> error "impossible"
      callback cm a

-- | Saves a module to the cache
cacheModule :: (GM.MonadIO m, HasGhcModuleCache m)
            => FilePath -> CachedModule -> m ()
cacheModule uri cm = do
  uri' <- liftIO $ canonicalizePath uri
  modifyCache (\gmc ->
      gmc { uriCaches = Map.insert
                          uri'
                          (UriCache cm Map.empty)
                          (uriCaches gmc)
          }
    )
  where

-- | Saves a module to the cache without clearing the associated cache data - use only if you are
-- sure that the cached data associated with the module doesn't change
cacheModuleNoClear :: (GM.MonadIO m, HasGhcModuleCache m)
            => FilePath -> CachedModule -> m ()
cacheModuleNoClear uri cm = do
  uri' <- liftIO $ canonicalizePath uri
  modifyCache (\gmc ->
      gmc { uriCaches = Map.insertWith
                          (updateCachedModule cm)
                          uri'
                          (UriCache cm Map.empty)
                          (uriCaches gmc)
          }
    )
  where
    updateCachedModule :: CachedModule -> UriCache -> UriCache -> UriCache
    updateCachedModule cm' _ old = old { cachedModule = cm' }

-- | Deletes a module from the cache
deleteCachedModule :: (GM.MonadIO m, HasGhcModuleCache m) => FilePath -> m ()
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
    cacheDataProducer :: (GM.MonadIO m, MonadMTState IdeState m)
                      => CachedModule -> m a

instance ModuleCache () where
    cacheDataProducer = const $ return ()
