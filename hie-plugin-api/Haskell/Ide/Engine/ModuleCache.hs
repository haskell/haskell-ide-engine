{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskell.Ide.Engine.ModuleCache
  ( modifyCache
  , ifCachedInfo
  , withCachedInfo
  , ifCachedModule
  , ifCachedModuleM
  , ifCachedModuleAndData
  , withCachedModule
  , withCachedModuleAndData
  , deleteCachedModule
  , failModule
  , cacheModule
  , cacheModules
  , cacheInfoNoClear
  , runActionWithContext
  , ModuleCache(..)
  , PublishDiagnostics
  ) where


import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as B
import           Data.Dynamic (toDyn, fromDynamic, Dynamic)
import           Data.Generics (Proxy(..), TypeRep, typeRep, typeOf)
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.SortedList as SL
import qualified Data.Trie.Convenience as T
import qualified Data.Trie as T
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import qualified Data.Yaml as Yaml
import           System.Directory


import qualified GHC
import qualified HscMain as GHC
import qualified HIE.Bios as Bios
import qualified HIE.Bios.Ghc.Api as Bios

import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Diagnostics as J
import           Haskell.Ide.Engine.ArtifactMap
import           Haskell.Ide.Engine.Cradle (findLocalCradle, cradleDisplay)
import           Haskell.Ide.Engine.TypeMap
import           Haskell.Ide.Engine.GhcModuleCache
import           Haskell.Ide.Engine.MultiThreadState
import           Haskell.Ide.Engine.PluginsIdeMonads
import           Haskell.Ide.Engine.GhcCompat
import           Haskell.Ide.Engine.GhcUtils
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.MonadFunctions
-- ---------------------------------------------------------------------

modifyCache :: (HasGhcModuleCache m) => (GhcModuleCache -> GhcModuleCache) -> m ()
modifyCache f = modifyModuleCache f

-- ---------------------------------------------------------------------

type PublishDiagnostics = J.NormalizedUri -> J.TextDocumentVersion -> J.DiagnosticsBySource -> IO ()

-- | Run the given action in context and initialise a session with hie-bios.
-- If a context is given, the context is used to initialise a session for GHC.
-- The project "hie-bios" is used to find a Cradle and setup a GHC session
-- for diagnostics.
-- If no context is given, just execute the action.
-- Executing an action without context is useful, if you want to only
-- mutate ModuleCache or something similar without potentially loading
-- the whole GHC session for a component.
--
-- There are three possibilities for loading a cradle
-- 1. Load succeeds and we get a new cradle to execute the action in
-- 2. Load fails, so we report an error using IdeResultFail
-- 3. The bios reports CradleNone, which means we should completely ignore
-- the file.
--
-- In the third case, we
-- 1. Don't execute the action which we told to run, as we should behave as
-- though we know nothing about the file.
-- 2. Return the default value for the specific action.
runActionWithContext :: (MonadIde m, GHC.GhcMonad m, HasGhcModuleCache m, MonadBaseControl IO m)
                     => PublishDiagnostics
                     -> GHC.DynFlags
                     -> Maybe FilePath -- ^ Context for the Action
                     -> a -- ^ Default value for none cradle
                     -> m a -- ^ Action to execute
                     -> m (IdeResult a) -- ^ Result of the action or error in
                                        -- the context initialisation.
runActionWithContext _pub _df Nothing _def action =
  -- Cradle with no additional flags
  -- dir <- liftIO $ getCurrentDirectory
  --This causes problems when loading a later package which sets the
  --packageDb
  -- loadCradle df (Bios.defaultCradle dir)
  fmap IdeResultOk action
runActionWithContext publishDiagnostics df (Just uri) def action = do
  mcradle <- getCradle uri
  loadCradle publishDiagnostics df mcradle def action

-- ---------------------------------------------------------------------

-- | Load the Cradle based on the given DynFlags and Cradle lookup Result.
-- Reuses a Cradle if possible and sets up a GHC session for a new Cradle
-- if needed.
-- This function may take a long time to execute, since it potentially has
-- to set up the Session, including downloading all dependencies of a Cradle.
loadCradle :: forall a m . (MonadIde m, HasGhcModuleCache m, GHC.GhcMonad m
              , MonadBaseControl IO m)
              => PublishDiagnostics
              -> GHC.DynFlags
              -> LookupCradleResult
              -> a
              -> m a
              -> m (IdeResult a)
loadCradle _ _ ReuseCradle _def action = do
  -- Since we expect this message to show up often, only show in debug mode
  debugm "Reusing cradle"
  IdeResultOk <$> action

loadCradle _ _iniDynFlags (LoadCradle (CachedCradle crd env)) _def action = do
  -- Reloading a cradle happens on component switch
  logm $ "Switch to cradle: " ++ show crd
  -- Cache the existing cradle
  maybe (return ()) cacheCradle =<< (currentCradle <$> getModuleCache)
  GHC.setSession env
  setCurrentCradle crd
  IdeResultOk <$> action

loadCradle publishDiagnostics iniDynFlags (NewCradle fp) def action = do
  -- If this message shows up a lot in the logs, it is an indicator for a bug
  logm $ "New cradle: " ++ fp
  -- Cache the existing cradle
  maybe (return ()) cacheCradle =<< (currentCradle <$> getModuleCache)

  -- Now load the new cradle, accounting for hie.yaml parse errors
  let parseErrorHandler = return . Left . Yaml.prettyPrintParseException
  cradleRes <- liftIO $ catch (Right <$> findLocalCradle fp) parseErrorHandler
  case cradleRes of
    Right cradle -> do
      logm $ "Found cradle: " ++ show cradle
      withProgress ("Initializing " <> cradleDisplay cradle) NotCancellable (initialiseCradle cradle)
    Left yamlErr ->
      return $ IdeResultFail $ IdeError
        { ideCode    = OtherError
        , ideMessage = Text.pack $ "Couldn't parse hie.yaml: " <> yamlErr
        , ideInfo    = Aeson.Null
        }

 where
  -- | Initialise the given cradle. This might fail and return an error via `IdeResultFail`.
  -- Reports its progress to the client.
  initialiseCradle :: (MonadIde m, HasGhcModuleCache m, GHC.GhcMonad m)
                  => Bios.Cradle -> (Progress -> IO ()) -> m (IdeResult a)
  initialiseCradle cradle f = do
    res <- Bios.initializeFlagsWithCradleWithMessage (Just (toMessager f)) fp cradle
    case res of
      Bios.CradleNone ->
        -- Note: The action is not run if we are in the none cradle, we
        -- just pretend the file doesn't exist.
        return $ IdeResultOk def
      Bios.CradleFail (Bios.CradleError code msg) -> do
        warningm $ "Fail on cradle initialisation: (" ++ show code ++ ")" ++ show msg

        -- Send a detailed diagnostic to the user.

        let normalizedUri = J.toNormalizedUri (filePathToUri fp)
            sev = Just DsError
            range = Range (Position 0 0) (Position 1 0)
            msgTxt =
              [ "Fail on initialisation for \"" <> Text.pack fp <> "\"."
              ] <> map Text.pack msg
            source = Just "bios"
            diag = Diagnostic range sev Nothing source (Text.unlines msgTxt) Nothing

        liftIO $ publishDiagnostics normalizedUri Nothing
            (Map.singleton source (SL.singleton diag))

        return $ IdeResultFail $ IdeError
            { ideCode    = OtherError
            , ideMessage = Text.unwords  (take 2 msgTxt)
            , ideInfo    = Aeson.Null
            }
      Bios.CradleSuccess init_session -> do
        -- Note that init_session contains a Hook to 'f'.
        -- So, it can still provide Progress Reports.
        -- Therefore, invocation of 'init_session' must happen
        -- while 'f' is still valid.
        liftIO (GHC.newHscEnv iniDynFlags) >>= GHC.setSession
        liftIO $ setCurrentDirectory (Bios.cradleRootDir cradle)

        let onGhcError = return . Left
        let onSourceError srcErr = do
              logm $ "Source error on cradle initialisation: " ++ show srcErr
              return $ Right Bios.Failed
        -- We continue setting the cradle in case the file has source errors
        -- cause they will be reported to user by diagnostics
        init_res <- gcatches
                      (Right <$> init_session)
                      (errorHandlers onGhcError onSourceError)

        case init_res of
          Left err -> do
            logm $ "Ghc error on cradle initialisation: " ++ show err
            return $ IdeResultFail $ IdeError
              { ideCode    = OtherError
              , ideMessage = Text.pack $ show err
              , ideInfo    = Aeson.Null
              }
            -- Note: Don't setCurrentCradle because we want to try to reload
            -- it on a save whilst there are errors. Subsequent loads won't
            -- be that slow, even though the cradle isn't cached because the
            -- `.hi` files will be saved.
          Right Bios.Succeeded -> do
            setCurrentCradle cradle
            logm "Cradle set succesfully"
            IdeResultOk <$> action

          Right Bios.Failed -> do
            setCurrentCradle cradle
            logm "Cradle did not load succesfully"
            IdeResultOk <$> action

-- | Sets the current cradle for caching.
-- Retrieves the current GHC Module Graph, to find all modules
-- that belong to this cradle.
-- If the cradle does not load any module, it is responsible for an empty
-- list of Modules.
setCurrentCradle :: (HasGhcModuleCache m, GHC.GhcMonad m) => Bios.Cradle -> m ()
setCurrentCradle cradle = do
    mg <- GHC.getModuleGraph
    let ps = mapMaybe (GHC.ml_hs_file . GHC.ms_location) (mgModSummaries mg)
    debugm $ "Modules in the cradle: " ++ show ps
    ps' <- liftIO $ mapM canonicalizePath ps
    modifyCache (\s -> s { currentCradle = Just (ps', cradle) })

-- | Cache the given Cradle.
-- Caches the given Cradle together with all Modules this Cradle is responsible
-- for.
-- Via 'lookupCradle' it can be checked if a given FilePath is managed by
-- a any Cradle that has already been loaded.
cacheCradle :: (HasGhcModuleCache m, GHC.GhcMonad m) => ([FilePath], Bios.Cradle) -> m ()
cacheCradle (ds, c) = do
  env <- GHC.getSession
  let cc = CachedCradle c env
      new_map = T.fromList (map (, cc) (map B.pack ds))
  modifyCache (\s -> s { cradleCache = T.unionWith (\a _ -> a) new_map (cradleCache s) })

-- | Get the Cradle that should be used for a given FilePath.
-- Looks up the cradle in the Module Cache and checks if the given
-- FilePath is managed by any already loaded Cradle.
getCradle :: (GHC.GhcMonad m, HasGhcModuleCache m)
         => FilePath -> m LookupCradleResult
getCradle fp = do
  canon_fp <- liftIO $ canonicalizePath fp
  mcache <- getModuleCache
  return $ lookupCradle canon_fp mcache

ifCachedInfo :: (HasGhcModuleCache m, MonadIO m) => FilePath -> a -> (CachedInfo -> m a) -> m a
ifCachedInfo fp def callback = do
  muc <- getUriCache fp
  case muc of
    Just (UriCacheSuccess uc) -> callback (cachedInfo uc)
    _ -> return def

withCachedInfo :: FilePath -> a -> (CachedInfo -> IdeDeferM a) -> IdeDeferM a
withCachedInfo fp def callback = deferIfNotCached fp go
  where go (UriCacheSuccess uc) = callback (cachedInfo uc)
        go UriCacheFailed = return def

ifCachedModule :: (HasGhcModuleCache m, MonadIO m, CacheableModule b) => FilePath -> a -> (b -> CachedInfo -> m a) -> m a
ifCachedModule fp def callback = ifCachedModuleM fp (return def) callback

-- | Calls the callback with the cached module for the provided path.
-- Otherwise returns the default immediately if there is no cached module
-- available.
-- If you need custom data, see also 'ifCachedModuleAndData'.
-- If you are in IdeDeferM and would like to wait until a cached module is available,
-- see also 'withCachedModule'.
ifCachedModuleM :: (HasGhcModuleCache m, MonadIO m, CacheableModule b)
                => FilePath -> m a -> (b -> CachedInfo -> m a) -> m a
ifCachedModuleM fp k callback = do
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
    Nothing -> k

-- | Calls the callback with the cached module and data for the provided path.
-- Otherwise returns the default immediately if there is no cached module
-- available.
-- If you are in IdeDeferM and would like to wait until a cached module is available,
-- see also 'withCachedModuleAndData'.
ifCachedModuleAndData :: forall a b m. (ModuleCache a, HasGhcModuleCache m, MonadIO m, MonadMTState IdeState m)
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
        go (UriCacheSuccess (UriCache { cachedTcMod = Nothing })) = wrap (Defer fp go)
        go UriCacheFailed = return def

getUriCache :: (HasGhcModuleCache m, MonadIO m) => FilePath -> m (Maybe UriCacheResult)
getUriCache fp = do
  canonical_fp <- liftIO $ canonicalizePath fp
  fmap (Map.lookup canonical_fp . uriCaches) getModuleCache

deferIfNotCached :: FilePath -> (UriCacheResult -> IdeDeferM a) -> IdeDeferM a
deferIfNotCached fp cb = do
  muc <- getUriCache fp
  case muc of
    Just res -> cb res
    Nothing -> wrap (Defer fp cb)

lookupCachedData :: forall a m. (HasGhcModuleCache m, MonadMTState IdeState m, MonadIO m, Typeable a, ModuleCache a)
                 => FilePath -> GHC.TypecheckedModule -> CachedInfo -> (Map.Map TypeRep Dynamic) -> m a
lookupCachedData fp tm info dat = do
  canonical_fp <- liftIO $ canonicalizePath fp
  let proxy :: Proxy a
      proxy = Proxy
  case Map.lookup (typeRep proxy) dat of
    Nothing -> do
      val <- cacheDataProducer tm info
      let dat' = Map.insert (typeOf val) (toDyn val) dat
          newUc = UriCache info (GHC.tm_parsed_module tm) (Just tm) dat'
      modifyCache (\s -> s {uriCaches = Map.insert canonical_fp (UriCacheSuccess newUc)
                                                  (uriCaches s)})
      return val

    Just x ->
      case fromDynamic x of
        Just val -> return val
        Nothing  -> error "impossible"

cacheModules :: (FilePath -> FilePath) -> [GHC.TypecheckedModule] -> IdeGhcM ()
cacheModules rfm ms = mapM_ go_one ms
  where
    go_one m = case get_fp m of
                 Just fp -> cacheModule (rfm fp) (Right m)
                 Nothing -> do
                  logm $ "Reverse File Map failed in cacheModules for FilePath: " ++ show (get_fp m)
                  return ()
    get_fp = GHC.ml_hs_file . GHC.ms_location . GHC.pm_mod_summary . GHC.tm_parsed_module

-- | Saves a module to the cache and executes any deferred
-- responses waiting on that module.
cacheModule :: FilePath -> (Either GHC.ParsedModule GHC.TypecheckedModule) -> IdeGhcM ()
cacheModule fp modul = do
  canonical_fp <- liftIO $ canonicalizePath fp
  rfm <- reverseFileMap
  newUc <-
    case modul of
      Left pm -> do
        muc <- getUriCache canonical_fp
        let defInfo = CachedInfo mempty mempty mempty mempty rfm return return
        return $ case muc of
          Just (UriCacheSuccess uc) ->
            let newCI = oldCI { revMap = rfm . revMap oldCI }
                    --                         ^^^^^^^^^^^^
                    -- We have to retain the old mapping state, since the
                    -- old TypecheckedModule still contains spans relative to that
                oldCI = cachedInfo uc
              in uc { cachedPsMod = pm, cachedInfo = newCI }
          _ -> UriCache defInfo pm Nothing mempty

      Right tm -> do
        typm <- genTypeMap tm
        let info = CachedInfo (genLocMap tm) typm (genImportMap tm) (genDefMap tm) rfm return return
            pm = GHC.tm_parsed_module tm
        return $ UriCache info pm (Just tm) mempty

  let res = UriCacheSuccess newUc
  modifyCache $ \gmc ->
      gmc { uriCaches = Map.insert canonical_fp res (uriCaches gmc) }

  -- execute any queued actions for the module
  runDeferredActions canonical_fp res

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
      -- In general it is unsafe to read and then modify but the modification doesn't
      -- capture the previously read state.
      actions <- fromMaybe [] . Map.lookup uri . requestQueue <$> readMTS
      -- remove queued actions
      modifyMTS $ \s -> s { requestQueue = Map.delete uri (requestQueue s) }

      liftToGhc $ forM_ actions (\a -> a res)


-- | Saves a module to the cache without clearing the associated cache data - use only if you are
-- sure that the cached data associated with the module doesn't change
cacheInfoNoClear :: (MonadIO m, HasGhcModuleCache m)
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
deleteCachedModule :: (MonadIO m, HasGhcModuleCache m) => FilePath -> m ()
deleteCachedModule uri = do
  uri' <- liftIO $ canonicalizePath uri
  modifyCache (\s -> s { uriCaches = Map.delete uri' (uriCaches s) })

-- ---------------------------------------------------------------------
-- | A ModuleCache is valid for the lifetime of a CachedModule
-- It is generated on need and the cache is invalidated
-- when a new CachedModule is loaded.
-- Allows the caching of arbitrary data linked to a particular
-- TypecheckedModule.
-- TODO: this name is confusing, given GhcModuleCache. Change it
class Typeable a => ModuleCache a where
    -- | Defines an initial value for the state extension
    cacheDataProducer :: (MonadIO m, MonadMTState IdeState m)
                      => GHC.TypecheckedModule -> CachedInfo -> m a

instance ModuleCache () where
    cacheDataProducer = const $ const $ return ()
