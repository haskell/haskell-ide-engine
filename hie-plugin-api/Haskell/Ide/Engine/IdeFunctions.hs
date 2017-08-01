{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | Functions that act within the IdeM monad

module Haskell.Ide.Engine.IdeFunctions
  (
    getPlugins
  , cachedModules
  , getCachedModule
  , withCachedModule
  , withCachedModuleAndData
  , cacheModule
  , deleteCachedModule
  , getCradle
  , runActionWithContext
  ) where

import           Control.Monad.State.Strict
import           Data.Dynamic
import qualified Data.Map                          as Map
import qualified GhcMod.Cradle                     as GM
import qualified GhcMod.Monad                      as GM
import qualified GhcMod.Types                      as GM
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginTypes
import           System.Directory
import           System.FilePath

import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils

-- | Runs an IdeM action with the given Cradle
withCradle :: GM.Cradle -> IdeM a -> IdeM a
withCradle crdl =
  GM.gmeLocal (\env -> env {GM.gmCradle = crdl})

-- | Runs an action in a ghc-mod Cradle found from the
-- directory of the given file. If no file is found
-- then runs the action in the default cradle.
-- Sets the current directory to the cradle root dir
-- in either case
runActionWithContext :: Maybe Uri -> IdeM a -> IdeM a
runActionWithContext Nothing action = do
  crdl <- GM.cradle
  liftIO $ setCurrentDirectory $ GM.cradleRootDir crdl
  action
runActionWithContext (Just uri) action = do
  crdl <- getCradle uri
  liftIO $ setCurrentDirectory $ GM.cradleRootDir crdl
  withCradle crdl action

-- | Returns all the cached modules in the IdeState
cachedModules :: IdeState -> Map.Map Uri CachedModule
cachedModules = fmap cachedModule . uriCaches

-- | Get the Cradle that should be used for a given URI
getCradle :: Uri -> IdeM GM.Cradle
getCradle uri =
  case uriToFilePath uri of
    Nothing -> do
      debugm $ "getCradle: malformed uri: " ++ show uri
      GM.cradle
    Just fp -> do
      dir <- liftIO $ takeDirectory <$> canonicalizePath fp
      mcradle <- lift . lift $ gets (Map.lookup dir . cradleCache)
      case mcradle of
        Just crdl -> do
          debugm $ "cradle cache hit for " ++ dir ++ ", using cradle " ++ show crdl
          return crdl
        Nothing -> do
          opts <- GM.options
          crdl <- GM.findCradle' (GM.optPrograms opts) dir
          debugm $ "cradle cache miss for " ++ dir ++ ", generating cradle " ++ show crdl
          lift . lift $ modify' (\s -> s { cradleCache = Map.insert dir crdl (cradleCache s)})
          return crdl


-- | looks up a CachedModule for a given URI
getCachedModule :: Uri -> IdeM (Maybe CachedModule)
getCachedModule uri = do
  uri' <- canonicalizeUri uri
  lift . lift $ gets (Map.lookup uri' . cachedModules)

-- | Version of `withCachedModuleAndData` that doesn't provide
-- any extra cached data
withCachedModule :: Uri -> IdeM b -> (CachedModule -> IdeM b) -> IdeM b
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
withCachedModuleAndData :: forall a b. ModuleCache a
  => Uri -> IdeM b -> (CachedModule -> a -> IdeM b) -> IdeM b
withCachedModuleAndData uri noCache callback = do
  uri' <- canonicalizeUri uri
  mc <- lift . lift $ gets (Map.lookup uri' . uriCaches)
  case mc of
    Nothing -> noCache
    Just UriCache{cachedModule = cm, cachedData = dat} -> do
      a <- case Map.lookup (typeRep $ Proxy @a) dat of
             Nothing -> do
               val <- cacheDataProducer cm
               let typ = typeOf val
               debugm $ "withCachedModuleAndData: Cache miss - " ++ show typ
               let dat' = Map.insert (typeOf val) (toDyn val) dat
               lift . lift $ modify' (\s -> s {uriCaches = Map.insert uri' (UriCache cm dat')
                                                                           (uriCaches s)})
               return val
             Just x -> do
               debugm $ "withCachedModuleAndData: Cache hit - " ++ show (typeRep $ Proxy @a)
               case fromDynamic x of
                 Just val -> return val
                 Nothing  -> error "impossible"
      callback cm a

-- | Saves a module to the cache
cacheModule :: Uri -> CachedModule -> IdeM ()
cacheModule uri cm = do
  uri' <- canonicalizeUri uri
  lift . lift $ modify' (\s -> s { uriCaches = Map.insert uri' (UriCache cm Map.empty)
                                                               (uriCaches s) })

-- | Deletes a module from the cache
deleteCachedModule :: Uri -> IdeM ()
deleteCachedModule uri = do
  uri' <- canonicalizeUri uri
  lift . lift $ modify' (\s -> s { uriCaches = Map.delete uri' (uriCaches s) })

getPlugins :: IdeM IdePlugins
getPlugins = lift $ lift $ idePlugins <$> get
