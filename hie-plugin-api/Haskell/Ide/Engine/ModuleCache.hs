{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Haskell.Ide.Engine.ModuleCache
  ( modifyCache
  , withCradle
  , ifCachedInfo
  , withCachedInfo
  , ifCachedModule
  , ifCachedModuleAndData
  , withCachedModule
  , withCachedModuleAndData
  , deleteCachedModule
  , failModule
  , cacheModule
  , cacheInfoNoClear
  , runActionWithContext
  , ModuleCache(..)
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free
import           Data.Dynamic (toDyn, fromDynamic, Dynamic)
import           Data.Generics (Proxy(..), TypeRep, typeRep, typeOf)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Typeable (Typeable)
import           Exception (ExceptionMonad)
import           System.Directory
import           System.FilePath

import qualified GhcMod.Cradle as GM
import qualified GhcMod.Monad  as GM
import qualified GhcMod.Types  as GM
import qualified GhcMod.Utils  as GM
import qualified GHC           as GHC

import           Haskell.Ide.Engine.ArtifactMap
import           Haskell.Ide.Engine.GhcModuleCache
import           Haskell.Ide.Engine.MultiThreadState
import           Haskell.Ide.Engine.PluginsIdeMonads

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

ifCachedInfo :: (HasGhcModuleCache m, GM.MonadIO m) => FilePath -> a -> (CachedInfo -> m a) -> m a
ifCachedInfo fp def callback = do
  muc <- getUriCache fp
  case muc of
    Just (UriCacheSuccess uc) -> callback (cachedInfo uc)
    _ -> return def

withCachedInfo :: FilePath -> a -> (CachedInfo -> IdeDeferM a) -> IdeDeferM a
withCachedInfo fp def callback = deferIfNotCached fp go
  where go (UriCacheSuccess uc) = callback (cachedInfo uc)
        go UriCacheFailed = return def

-- | Calls the callback with the cached module for the provided path.
-- Otherwise returns the default immediately if there is no cached module
-- available.
-- If you need custom data, see also 'ifCachedModuleAndData'.
-- If you are in IdeDeferM and would like to wait until a cached module is available,
-- see also 'withCachedModule'.
ifCachedModule :: (HasGhcModuleCache m, GM.MonadIO m, CacheableModule b) => FilePath -> a -> (b -> CachedInfo -> m a) -> m a
ifCachedModule fp def callback = do
  muc <- getUriCache fp
  let x = do
        res <- muc
        case res of
          UriCacheSuccess uc -> do
            let ci = cachedInfo uc
            cm <- fromUriCache uc
            return (ci, cm)
          UriCacheFailed -> Nothing
  case x of
    Just (ci, cm) -> callback cm ci
    Nothing -> return def

-- | Calls the callback with the cached module and data for the provided path.
-- Otherwise returns the default immediately if there is no cached module
-- available.
-- If you are in IdeDeferM and would like to wait until a cached module is available,
-- see also 'withCachedModuleAndData'.
ifCachedModuleAndData :: forall a b m. (ModuleCache a, HasGhcModuleCache m, GM.MonadIO m, MonadMTState IdeState m)
                      => FilePath -> b -> (GHC.TypecheckedModule -> CachedInfo -> a -> m b) -> m b
ifCachedModuleAndData fp def callback = do
  muc <- getUriCache fp
  case muc of
    Just (UriCacheSuccess uc@(UriCache info _ (Just tm) dat)) ->
      case fromUriCache uc of
        Just modul -> lookupCachedData fp tm info dat >>= callback modul (cachedInfo uc)
        Nothing -> return def
    _ -> return def

-- | Calls the callback with the cached module for the provided path.
-- If there is no cached module immediately available, it will call the callback once
-- the module has been cached.
-- If that module fails to load, it will then return then default as a last resort.
-- If you need custom data, see also 'withCachedModuleAndData'.
-- If you don't want to wait until a cached module is available,
-- see also 'ifCachedModule'.
withCachedModule :: CacheableModule b => FilePath -> a -> (b -> CachedInfo -> IdeDeferM a) -> IdeDeferM a
withCachedModule fp def callback = deferIfNotCached fp go
  where go (UriCacheSuccess uc@(UriCache _ _ _ _)) =
          case fromUriCache uc of
            Just modul -> callback modul (cachedInfo uc)
            Nothing -> wrap (Defer fp go)
        go UriCacheFailed = return def

-- | Calls its argument with the CachedModule for a given URI
-- along with any data that might be stored in the ModuleCache.
-- If the module is not already cached, then the callback will be
-- called as soon as it is available.
-- The data is associated with the CachedModule and its cache is
-- invalidated when a new CachedModule is loaded.
-- If the data doesn't exist in the cache, new data is generated
-- using by calling the `cacheDataProducer` function.
withCachedModuleAndData :: forall a b. (ModuleCache a)
                        => FilePath -> b
                        -> (GHC.TypecheckedModule -> CachedInfo -> a -> IdeDeferM b) -> IdeDeferM b
withCachedModuleAndData fp def callback = deferIfNotCached fp go
  where go (UriCacheSuccess (uc@(UriCache info _ (Just tm) dat))) =
          lookupCachedData fp tm info dat >>= callback tm (cachedInfo uc)
        go (UriCacheSuccess (UriCache _ _ Nothing _)) = wrap (Defer fp go)
        go UriCacheFailed = return def

getUriCache :: (HasGhcModuleCache m, GM.MonadIO m) => FilePath -> m (Maybe UriCacheResult)
getUriCache fp = do
  uri' <- liftIO $ canonicalizePath fp
  fmap (Map.lookup uri' . uriCaches) getModuleCache

deferIfNotCached :: FilePath -> (UriCacheResult -> IdeDeferM a) -> IdeDeferM a
deferIfNotCached fp cb = do
  muc <- getUriCache fp
  case muc of
    Just res -> cb res
    Nothing -> wrap (Defer fp cb)

lookupCachedData :: forall a m. (HasGhcModuleCache m, MonadMTState IdeState m, GM.MonadIO m, Typeable a, ModuleCache a)
                 => FilePath -> GHC.TypecheckedModule -> CachedInfo -> (Map.Map TypeRep Dynamic) -> m a
lookupCachedData fp tm info dat = do
  fp' <- liftIO $ canonicalizePath fp
  let proxy :: Proxy a
      proxy = Proxy
  case Map.lookup (typeRep proxy) dat of
    Nothing -> do
      val <- cacheDataProducer tm info
      let dat' = Map.insert (typeOf val) (toDyn val) dat
          newUc = UriCache info (GHC.tm_parsed_module tm) (Just tm) dat'
      modifyCache (\s -> s {uriCaches = Map.insert fp' (UriCacheSuccess newUc)
                                                  (uriCaches s)})
      return val

    Just x ->
      case fromDynamic x of
        Just val -> return val
        Nothing  -> error "impossible"

-- | Saves a module to the cache and executes any deferred
-- responses waiting on that module.
cacheModule :: FilePath -> (Either GHC.ParsedModule GHC.TypecheckedModule) -> IdeGhcM ()
cacheModule uri modul = do
  uri' <- liftIO $ canonicalizePath uri
  rfm <- GM.mkRevRedirMapFunc

  newUc <-
    case modul of
      Left pm -> do
        muc <- getUriCache uri'
        let defInfo = CachedInfo mempty mempty mempty mempty rfm return return
        return $ case muc of
          Just (UriCacheSuccess uc) ->
            let newCI = (cachedInfo uc) { revMap = rfm }
              in uc { cachedPsMod = pm, cachedInfo = newCI }
          _ -> UriCache defInfo pm Nothing mempty

      Right tm -> do
        typm <- GM.unGmlT $ genTypeMap tm
        let info = CachedInfo (genLocMap tm) typm (genImportMap tm) (genDefMap tm) rfm return return
            pm = GHC.tm_parsed_module tm
        return $ UriCache info pm (Just tm) mempty

  let res = UriCacheSuccess newUc
  modifyCache $ \gmc ->
      gmc { uriCaches = Map.insert uri' res (uriCaches gmc) }

  -- execute any queued actions for the module
  runDeferredActions uri' res

-- | Marks a module that it failed to load and triggers
-- any deferred responses waiting on it
failModule :: FilePath -> IdeGhcM ()
failModule fp = do
  fp' <- liftIO $ canonicalizePath fp

  maybeUriCache <- fmap (Map.lookup fp' . uriCaches) getModuleCache

  let res = UriCacheFailed

  case maybeUriCache of
    Just _ -> return ()
    Nothing ->
      -- If there's no cache for the module mark it as failed
      modifyCache (\gmc ->
          gmc {
            uriCaches = Map.insert fp' res (uriCaches gmc)
          }
        )

      -- Fail the queued actions
  runDeferredActions fp' res


runDeferredActions :: FilePath -> UriCacheResult -> IdeGhcM ()
runDeferredActions uri res = do
      actions <- fmap (fromMaybe [] . Map.lookup uri) (requestQueue <$> readMTS)
      -- remove queued actions
      modifyMTS $ \s -> s { requestQueue = Map.delete uri (requestQueue s) }

      liftToGhc $ forM_ actions (\a -> a res)


-- | Saves a module to the cache without clearing the associated cache data - use only if you are
-- sure that the cached data associated with the module doesn't change
cacheInfoNoClear :: (GM.MonadIO m, HasGhcModuleCache m)
                 => FilePath -> CachedInfo -> m ()
cacheInfoNoClear uri ci = do
  uri' <- liftIO $ canonicalizePath uri
  modifyCache (\gmc ->
      gmc { uriCaches = Map.adjust
                          updateCachedInfo
                          uri'
                          (uriCaches gmc)
          }
    )
  where
    updateCachedInfo :: UriCacheResult -> UriCacheResult
    updateCachedInfo (UriCacheSuccess old) = UriCacheSuccess (old { cachedInfo = ci })
    updateCachedInfo UriCacheFailed        = UriCacheFailed

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
                      => GHC.TypecheckedModule -> CachedInfo -> m a

instance ModuleCache () where
    cacheDataProducer = const $ const $ return ()
