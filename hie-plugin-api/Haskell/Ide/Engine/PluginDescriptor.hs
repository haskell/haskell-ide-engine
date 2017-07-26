{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Experimenting with a data structure to define a plugin.
--
-- The general idea is that a given plugin returns this structure during the
-- initial load/registration process, when- or however this eventually happens.
--
-- It should define the following things
--  1. What features the plugin should expose into the IDE
--       (this one may not be needed initially)
--
--       This may include a requirement to store private data of a particular
--       form.
--
--       It may be interesting to look at the Android model wrt Intents and
--       shared resource management, e.g. default Calendar app, default SMS app,
--       all making use of Contacts service.

module Haskell.Ide.Engine.PluginDescriptor
  (
    PluginDescriptor(..)
  , Service(..)

  -- * Commands
  , Command(..)
  , TaggedCommand
  , UntaggedCommand
  , CommandFunc(..), SyncCommandFunc, AsyncCommandFunc
  , buildCommand
  , Async
  , Callback

  -- * Plugins
  , Plugins

  -- * The IDE monad
  , IdeM
  , IdeState(..)
  , ExtensionClass(..)
  , ModuleCache(..)
  , getPlugins
  , untagPluginDescriptor
  , TaggedPluginDescriptor
  , UntaggedPluginDescriptor
  , NamedCommand(..)
  , CommandType(..)
  , Rec(..)
  , Proxy(..)
  , recordToList'
  , ValidResponse
  , CachedModule(..)
  , getCachedModule
  , withCachedModuleAndData
  , cacheModule
  , deleteCachedModule
  , oldRangeToNew
  , newRangeToOld
  , canonicalizeUri
  , getCradle
  , getTypecheckedModuleGhc
  -- * All the good types
  , module Haskell.Ide.Engine.PluginTypes
  ) where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Dynamic
-- import           Data.List
import qualified Data.Map as Map
import           Data.Singletons
import qualified Data.Text as T
import           Data.Vinyl
import qualified Data.Vinyl.Functor as Vinyl
import           GHC.Generics
import           GHC.TypeLits
import           Haskell.Ide.Engine.PluginTypes
import           Haskell.Ide.Engine.MonadFunctions
import qualified GhcMod.Monad as GM
import qualified GhcMod.Types as GM
import qualified GhcMod.Cradle as GM
import qualified GhcMod.Target as GM
import           GHC(TypecheckedModule)
import           System.Directory
import           System.FilePath

import qualified GHC           as GHC
import qualified Hooks         as GHC
import qualified HscMain       as GHC
import qualified HscTypes      as GHC
import qualified TcRnMonad     as GHC
import qualified DynFlags      as GHC
import qualified Exception     as Exception

-- import StringBuffer

import Data.IORef

-- ---------------------------------------------------------------------
type ValidResponse a = (FromJSON a, ToJSON a, Typeable a)

data PluginDescriptor cmds = PluginDescriptor
  { pdUIShortName     :: !T.Text
  , pdUIOverview     :: !T.Text
  , pdCommands        :: cmds
  , pdExposedServices :: [Service]
  , pdUsedServices    :: [Service]
  }

type TaggedPluginDescriptor cmds = PluginDescriptor (Rec NamedCommand cmds)

type UntaggedPluginDescriptor = PluginDescriptor [UntaggedCommand]

instance Show UntaggedPluginDescriptor where
  showsPrec p (PluginDescriptor name oview cmds svcs used) = showParen (p > 10) $
    showString "PluginDescriptor " .
    showString (T.unpack name) .
    showString (T.unpack oview) .
    showList cmds .
    showString " " .
    showList svcs .
    showString " " .
    showList used

data Service = Service
  { svcName :: T.Text
  -- , svcXXX :: undefined
  } deriving (Show,Eq,Ord,Generic)

recordToList' :: (forall a. f a -> b) -> Rec f as -> [b]
recordToList' f = recordToList . rmap (Vinyl.Const . f)

untagPluginDescriptor :: TaggedPluginDescriptor cmds -> UntaggedPluginDescriptor
untagPluginDescriptor pluginDescriptor =
  pluginDescriptor {pdCommands =
                      recordToList' untagCommand
                                    (pdCommands pluginDescriptor)}

type Plugins = Map.Map PluginId UntaggedPluginDescriptor

untagCommand :: NamedCommand t -> UntaggedCommand
untagCommand (NamedCommand _ (Command desc func)) =
  Command (desc {cmdContexts =
                   recordToList' fromSing
                                 (cmdContexts desc)
                ,cmdAdditionalParams =
                   recordToList' untagParamDesc
                                 (cmdAdditionalParams desc)})
          func

-- | Ideally a Command is defined in such a way that its CommandDescriptor
-- can be exposed via the native CLI for the tool being exposed as well.
-- Perhaps use Options.Applicative for this in some way.
data Command desc = forall a. (ValidResponse a) => Command
  { cmdDesc :: !desc
  , cmdFunc :: !(CommandFunc a)
  }

type TaggedCommand cxts tags
  = Command (TaggedCommandDescriptor cxts tags)
type UntaggedCommand = Command UntaggedCommandDescriptor

instance Show desc => Show (Command desc) where
  show (Command desc _func) = "(Command " ++ show desc ++ ")"

data NamedCommand (t :: CommandType) where
        NamedCommand ::
            KnownSymbol s =>
            Proxy s ->
            TaggedCommand cxts tags ->
            NamedCommand ('CommandType s cxts tags)

data CommandType = CommandType Symbol [AcceptedContext] [ParamDescType]

-- | Build a command, ensuring the command response type name and the command
-- function match
buildCommand :: forall a s cxts tags. (ValidResponse a, KnownSymbol s)
  => CommandFunc a
  -> Proxy s
  -> T.Text
  -> [T.Text]
  -> Rec SAcceptedContext cxts
  -> Rec SParamDescription tags
  -> Save
  -> NamedCommand ( 'CommandType s cxts tags )
buildCommand fun n d exts ctxs parm save =
  NamedCommand n $
  Command {cmdDesc =
            CommandDesc {cmdName = T.pack $ symbolVal n
                        ,cmdUiDescription = d
                        ,cmdFileExtensions = exts
                        ,cmdContexts = ctxs
                        ,cmdAdditionalParams = parm
                        ,cmdReturnType =
                           T.pack $ show $ typeOf (undefined :: a)
                        ,cmdSave = save}
          ,cmdFunc = fun}

-- ---------------------------------------------------------------------

-- | The 'CommandFunc' is called once the dispatcher has checked that it
-- satisfies at least one of the `AcceptedContext` values for the command
-- descriptor, and has all the required parameters. Where a command has only one
-- allowed context the supplied context list does not add much value, but allows
-- easy case checking when multiple contexts are supported.
data CommandFunc resp = CmdSync (SyncCommandFunc resp)
                      | CmdAsync (AsyncCommandFunc resp)
                        -- ^ Note: does not forkIO, the command must decide when
                        -- to do this.

type SyncCommandFunc resp
                = [AcceptedContext] -> IdeRequest -> IdeM (IdeResponse resp)

type AsyncCommandFunc resp = (IdeResponse resp -> IO ())
               -> [AcceptedContext] -> IdeRequest -> IdeM ()

type Callback a = IdeResponse a -> IO ()
type Async a = Callback a -> IO ()

-- -------------------------------------
-- JSON instances


instance ToJSON Service where
  toJSON service = object [ "name" .= svcName service ]


instance FromJSON Service where
  parseJSON (Object v) =
    Service <$> v .: "name"
  parseJSON _ = empty


-- ---------------------------------------------------------------------

type IdeM = IdeT IO
type IdeT m = GM.GhcModT (StateT IdeState m)

data IdeState = IdeState
  {
    idePlugins :: Plugins
  , extensibleState :: !(Map.Map TypeRep Dynamic)
              -- ^ stores custom state information.
  , cradleCache :: !(Map.Map FilePath GM.Cradle)
              -- ^ map from dirs to cradles
  , uriCaches  :: !UriCaches
  } deriving (Show)

type UriCaches = Map.Map Uri UriCache

data UriCache = UriCache
  { cachedModule :: !CachedModule
  , cachedData   :: !(Map.Map TypeRep Dynamic)
  } deriving Show

data CachedModule = CachedModule
  { tcMod       :: !TypecheckedModule
  , revMap      :: FilePath -> FilePath
  , newPosToOld :: Position -> Maybe Position
  , oldPosToNew :: Position -> Maybe Position
  }

instance Show CachedModule where
  show CachedModule{} = "CachedModule { .. }"

cachedModules :: IdeState -> Map.Map Uri CachedModule
cachedModules = fmap cachedModule . uriCaches

canonicalizeUri :: MonadIO m => Uri -> m Uri
canonicalizeUri uri =
  case uriToFilePath uri of
    Nothing -> return uri
    Just fp -> do
      fp' <- liftIO $ canonicalizePath fp
      return $ filePathToUri fp'

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

getCachedModule :: Uri -> IdeM (Maybe CachedModule)
getCachedModule uri = do
  uri' <- canonicalizeUri uri
  lift . lift $ gets (Map.lookup uri' . cachedModules)

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
                 Nothing -> error "impossible"
      callback cm a


cacheModule :: Uri -> CachedModule -> IdeM ()
cacheModule uri cm = do
  uri' <- canonicalizeUri uri
  lift . lift $ modify' (\s -> s { uriCaches = Map.insert uri' (UriCache cm Map.empty)
                                                               (uriCaches s) })

deleteCachedModule :: Uri -> IdeM ()
deleteCachedModule uri = do
  uri' <- canonicalizeUri uri
  lift . lift $ modify' (\s -> s { uriCaches = Map.delete uri' (uriCaches s) })

newRangeToOld :: CachedModule -> Range -> Maybe Range
newRangeToOld cm (Range start end) = do
  start' <- newPosToOld cm start
  end'   <- newPosToOld cm end
  return (Range start' end')

oldRangeToNew :: CachedModule -> Range -> Maybe Range
oldRangeToNew cm (Range start end) = do
  start' <- oldPosToNew cm start
  end'   <- oldPosToNew cm end
  return (Range start' end')

getPlugins :: IdeM Plugins
getPlugins = lift $ lift $ idePlugins <$> get

-- ---------------------------------------------------------------------
-- Module Loading
-- ---------------------------------------------------------------------

type HookIORefData = Maybe TypecheckedModule

getMappedFileName :: (GM.IOish m) => FilePath -> GM.GhcModT m FilePath
getMappedFileName fname = do
      maybeMapped <- GM.lookupMMappedFile fname
      let
        mFileName = case maybeMapped of
          Just (GM.FileMapping mfname _isTemp) -> mfname
          Nothing -> fname
      return mFileName

canonicalizeModSummary :: (MonadIO m) =>
  GHC.ModSummary -> m (Maybe FilePath)
canonicalizeModSummary =
  traverse (liftIO . canonicalizePath) . GHC.ml_hs_file . GHC.ms_location

tweakModSummaryDynFlags :: GHC.ModSummary -> GHC.ModSummary
tweakModSummaryDynFlags ms =
  let df = GHC.ms_hspp_opts ms
  in ms { GHC.ms_hspp_opts = GHC.gopt_set df GHC.Opt_KeepRawTokenStream }

getTypecheckedModuleGhc :: GM.IOish m
  => (GM.GmlT m () -> GM.GmlT m a) -> FilePath -> GM.GhcModT m (a, Maybe TypecheckedModule)
getTypecheckedModuleGhc wrapper targetFile = do
  cfileName <- liftIO $ canonicalizePath targetFile
  mFileName <- liftIO . canonicalizePath =<< getMappedFileName cfileName
  ref <- liftIO $ newIORef Nothing
  let
    setTarget fileName
      = GM.runGmlTWith' [Left fileName]
                        return
                        (Just $ updateHooks cfileName mFileName ref)
                        wrapper
                        (return ())
  res <- setTarget cfileName
  mtm <- liftIO $ readIORef ref
  return (res, mtm)

updateHooks :: FilePath -> FilePath -> IORef HookIORefData -> GHC.Hooks -> GHC.Hooks
updateHooks _ofp fp ref hooks = hooks {
        GHC.hscFrontendHook   = Just $ fmap GHC.FrontendTypecheck . hscFrontend fp ref
      }

newtype HareHsc a = HH { runHareHsc :: GHC.Hsc a }
  deriving (Functor,Applicative,Monad,MonadIO,GHC.HasDynFlags)

instance Exception.ExceptionMonad HareHsc where
  gcatch (HH (GHC.Hsc ma)) f = HH $ GHC.Hsc $ \e w -> do
    let f' err = g e w
          where (HH (GHC.Hsc g)) = f err
    (a, w') <- ma e w `Exception.catch` f'
    return (a, w')
  gmask f =
    let unHH (HH (GHC.Hsc x)) = x in
    HH $ GHC.Hsc $ \e w -> Exception.mask $ \io_restore ->
      let h_restore (HH (GHC.Hsc m)) = HH $ GHC.Hsc $ \e' w' -> io_restore (m e' w')
           in unHH (f h_restore) e w

instance GHC.GhcMonad HareHsc where
  getSession = HH GHC.getHscEnv
  setSession = error "Set session not defined for HareHsc"

hscFrontend :: FilePath -> IORef HookIORefData -> GHC.ModSummary -> GHC.Hsc GHC.TcGblEnv
hscFrontend fn ref mod_summary = do
    mfn <- canonicalizeModSummary mod_summary
    let
      md = GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod mod_summary
      keepInfo = case mfn of
                   Just fileName -> fn == fileName
                   Nothing -> False
    liftIO $ debugm $ "hscFrontend: got mod,file" ++ show (md, mfn)
    if keepInfo
      then runHareHsc $ do
        let modSumWithRaw = (tweakModSummaryDynFlags mod_summary)

        p' <- GHC.parseModule modSumWithRaw
        let p = p' {GHC.pm_mod_summary = mod_summary}
        tc <- GHC.typecheckModule p
        let tc_gbl_env = fst $ GHC.tm_internals_ tc

        liftIO $ modifyIORef' ref (const $ Just tc)
        return tc_gbl_env
      else do
        hpm <- GHC.hscParse' mod_summary
        hsc_env <- GHC.getHscEnv
        GHC.tcRnModule' hsc_env mod_summary False hpm

-- ---------------------------------------------------------------------
-- Extensible state, based on
-- http://xmonad.org/xmonad-docs/xmonad/XMonad-Core.html#t:ExtensionClass
--

-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a => ExtensionClass a where
    -- | Defines an initial value for the state extension
    initialValue :: a

class Typeable a => ModuleCache a where
    -- | Defines an initial value for the state extension
    cacheDataProducer :: CachedModule -> IdeM a

instance ModuleCache () where
    cacheDataProducer = const $ return ()
