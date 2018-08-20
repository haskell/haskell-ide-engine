{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Haskell.Ide.Engine.ModuleCache where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free
import           Data.Dynamic (toDyn, fromDynamic)
import           Data.Generics (Proxy(..), typeRep, typeOf)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
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


-- | The possible states the cache can be in
-- along with the cache or error if present
data CachedModuleResult = ModuleLoading
                        -- ^ The module has no cache yet and has not failed
                        | ModuleFailed T.Text
                        -- ^ The module has no cache because something went wrong
                        | ModuleCached CachedModule IsStale
                        -- ^ A cache exists for the module
type IsStale = Bool

-- | looks up a CachedModule for a given URI
getCachedModule :: (MonadIO m, HasGhcModuleCache m)
                => FilePath -> m CachedModuleResult
getCachedModule uri = do
  uri' <- liftIO $ canonicalizePath uri
  maybeUriCache <- fmap (Map.lookup uri' . uriCaches) getModuleCache
  return $ case maybeUriCache of
    Nothing -> ModuleLoading
    Just UriCache {cachedModule, isStale} -> ModuleCached cachedModule isStale
    Just (UriCacheFailed err) -> ModuleFailed err

-- | Returns true if there is a CachedModule for a given URI
isCached :: (GM.MonadIO m, HasGhcModuleCache m)
         => FilePath -> m Bool
isCached uri = do
  mc <- getCachedModule uri
  case mc of
    ModuleCached _ _ -> return True
    _                -> return False

-- | Version of `withCachedModuleAndData` that doesn't provide
-- any extra cached data.
withCachedModule :: FilePath -> a -> (CachedModule -> IdeM a) -> IdeM a
withCachedModule fp def callback = FreeT (return (Free (IdeDefer fp go)))
  where go UriCache{cachedModule = cm} = callback cm
        go _ = return def

-- | Calls its argument with the CachedModule for a given URI
-- along with any data that might be stored in the ModuleCache.
-- If the module is not already cached, then the callback will be
-- called as soon as it is available.
-- The data is associated with the CachedModule and its cache is
-- invalidated when a new CachedModule is loaded.
-- If the data doesn't exist in the cache, new data is generated
-- using by calling the `cacheDataProducer` function.
withCachedModuleAndData :: forall a. ModuleCache a
                        => FilePath -> a
                        -> (CachedModule -> a -> IdeM a) -> IdeM a
withCachedModuleAndData fp def callback =
  FreeT (return (Free (IdeDefer fp go)))
  where go (UriCacheFailed _) = return def
        go UriCache{cachedModule = cm, cachedData = dat} = do
          fp' <- liftIO $ canonicalizePath fp
          let proxy :: Proxy a
              proxy = Proxy
          a <- case Map.lookup (typeRep proxy) dat of
                Nothing -> do
                  val <- cacheDataProducer cm
                  let dat' = Map.insert (typeOf val) (toDyn val) dat
                  modifyCache (\s -> s {uriCaches = Map.insert fp' (UriCache cm dat' False)
                                                              (uriCaches s)})
                  return val
                Just x ->
                  case fromDynamic x of
                    Just val -> return val
                    Nothing  -> error "impossible"
          callback cm a

-- | Saves a module to the cache and executes any deferred
-- responses waiting on that module.
cacheModule :: FilePath -> CachedModule -> IdeGhcM ()
cacheModule uri cm = do
  uri' <- liftIO $ canonicalizePath uri

  let uc = UriCache cm Map.empty False

  modifyCache $ \gmc ->
      gmc { uriCaches = Map.insert uri' uc (uriCaches gmc) }

  -- execute any queued actions for the module
  runDeferredActions uri' uc

-- | Marks a module that it failed to load and triggers
-- any deferred responses waiting on it
failModule :: FilePath -> T.Text -> IdeGhcM ()
failModule fp err = do
  fp' <- liftIO $ canonicalizePath fp

  maybeUriCache <- fmap (Map.lookup fp' . uriCaches) getModuleCache

  let uc = UriCacheFailed err

  case maybeUriCache of
    Just _ -> return ()
    Nothing ->
      -- If there's no cache for the module mark it as failed
      modifyCache (\gmc ->
          gmc {
            uriCaches = Map.insert fp' uc (uriCaches gmc)
          }
        )

      -- Fail the queued actions
  runDeferredActions fp' uc


runDeferredActions :: FilePath -> UriCache -> IdeGhcM ()
runDeferredActions uri cached = do
      actions <- fmap (fromMaybe [] . Map.lookup uri) (requestQueue <$> readMTS)
      liftToGhc $ forM_ actions (\a -> a cached)

      -- remove queued actions
      modifyMTS $ \s -> s { requestQueue = Map.delete uri (requestQueue s) }

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
                          (UriCache cm Map.empty False)
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

markCacheStale :: (GM.MonadIO m, HasGhcModuleCache m) => FilePath -> m ()
markCacheStale uri = do
  uri' <- liftIO $ canonicalizePath uri
  modifyCache $ \gmc ->
    let newUriCaches = Map.update (\c -> case c of
                                    (UriCache cm d _) -> Just (UriCache cm d True)
                                    x -> Just x)
                                  uri'
                                  (uriCaches gmc)
      in gmc { uriCaches = newUriCaches }

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
