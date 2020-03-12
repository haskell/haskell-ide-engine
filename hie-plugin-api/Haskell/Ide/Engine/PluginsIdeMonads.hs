{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}


-- | IdeGhcM and associated types
module Haskell.Ide.Engine.PluginsIdeMonads
  (
  -- * LSP Commands
    HasPidCache(..)
  , mkLspCommand
  , allLspCmdIds
  , mkLspCmdId
  -- * Plugins
  , PluginId(..)
  , CommandId(..)
  , PluginDescriptor(..)
  , pluginDescToIdePlugins
  , PluginCommand(..)
  , runPluginCommand
  , DynamicJSON
  , dynToJSON
  , fromDynJSON
  , toDynJSON
  , CodeActionProvider
  , DiagnosticProvider(..)
  , DiagnosticProviderFunc(..)
  , DiagnosticProviderFuncSync
  , DiagnosticProviderFuncAsync
  , DiagnosticTrigger(..)
  , HoverProvider
  , SymbolProvider
  , FormattingType(..)
  , FormattingProvider
  , IdePlugins(..)
  , getDiagnosticProvidersConfig
  -- * IDE monads
  , IdeState(..)
  , IdeGhcM
  , runIdeGhcM
  , IdeM
  , runIdeM
  , IdeDeferM
  -- ** MonadIde and functions
  , MonadIde
  , getRootPath
  , getVirtualFile
  , getConfig
  , getClientCapabilities
  , getPlugins
  , withProgress
  , withIndefiniteProgress
  , persistVirtualFile'
  , getPersistedFile
  , reverseFileMap
  , withMappedFile
  , Core.Progress(..)
  , Core.ProgressCancellable(..)
  -- ** Lifting
  , iterT
  , LiftsToGhc(..)
  -- * IdeResult
  , IdeResult(..)
  , IdeResultT(..)
  , Defer(..)
  , IdeError(..)
  , IdeErrorCode(..)
  -- * LSP types
  , Uri(..)
  , uriToFilePath
  , filePathToUri
  , Position(..)
  , Range(..)
  , Location(..)
  , TextDocumentIdentifier(..)
  , TextDocumentPositionParams(..)
  , TextEdit(..)
  , WorkspaceEdit(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , PublishDiagnosticsParams(..)
  , List(..)
  , FormattingOptions(..)
  )
where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Control
import           Control.Monad.Base
import           UnliftIO
import           Control.Applicative

import           Data.Aeson                    hiding (defaultOptions)
import qualified Data.ConstrainedDynamic       as CD
import           Data.Default
import qualified Data.List                     as List
import           Data.Dynamic                   ( Dynamic )
import qualified Data.Map                      as Map
import           Data.Maybe

import qualified Data.Set                      as S
import           Data.String
import qualified Data.Text                     as T
import           Data.Typeable                  ( TypeRep )

#if __GLASGOW_HASKELL__ < 808
import           Data.Monoid                    ( (<>) )
import           Data.Typeable                  ( Typeable )
#endif

import System.Directory
import GhcMonad
import           GHC.Generics
import           GHC                            ( HscEnv, runGhcT )
import Exception

import           Haskell.Ide.Engine.Compat
import           Haskell.Ide.Engine.Config
import           Haskell.Ide.Engine.GhcModuleCache
import           Haskell.Ide.Engine.MultiThreadState

import qualified Language.Haskell.LSP.Core     as Core
import           Language.Haskell.LSP.Types.Capabilities
import           Language.Haskell.LSP.Types     ( Command(..)
                                                , CodeAction(..)
                                                , CodeActionContext(..)
                                                , Diagnostic(..)
                                                , DiagnosticSeverity(..)
                                                , DocumentSymbol(..)
                                                , List(..)
                                                , FormattingOptions(..)
                                                , Hover(..)
                                                , Location(..)
                                                , Position(..)
                                                , PublishDiagnosticsParams(..)
                                                , Range(..)
                                                , TextDocumentIdentifier(..)
                                                , TextDocumentPositionParams(..)
                                                , TextEdit(..)
                                                , Uri(..)
                                                , VersionedTextDocumentIdentifier(..)
                                                , WorkspaceEdit(..)
                                                , filePathToUri
                                                , uriToFilePath
                                                , toNormalizedUri
                                                )

import           Language.Haskell.LSP.VFS       ( VirtualFile(..) )

-- ---------------------------------------------------------------------
-- LSP Commands
-- ---------------------------------------------------------------------

-- | A monad that provides access to the current process ID.
-- Used when generating LSP command IDs
class Monad m => HasPidCache m where
  getPidCache :: m Int

instance HasPidCache IdeM where
  getPidCache = asks ideEnvPidCache

instance HasPidCache IO where
  getPidCache = getProcessID

instance HasPidCache m => HasPidCache (IdeResultT m) where
  getPidCache = lift getPidCache

mkLspCommand :: HasPidCache m => PluginId -> CommandId -> T.Text -> Maybe [Value] -> m Command
mkLspCommand plid cn title args' = do
  cmdId <- mkLspCmdId plid cn
  let args = List <$> args'
  return $ Command title cmdId args

allLspCmdIds :: HasPidCache m => IdePlugins -> m [T.Text]
allLspCmdIds (IdePlugins m) = concat <$> mapM go (Map.toList (pluginCommands <$> m))
  where
    go (plid, cmds) = mapM (mkLspCmdId plid . commandId) cmds

mkLspCmdId :: HasPidCache m => PluginId -> CommandId -> m T.Text
mkLspCmdId (PluginId plid) (CommandId cid) = do
  pid <- T.pack . show <$> getPidCache
  return $ pid <> ":" <> plid <> ":" <> cid

-- ---------------------------------------------------------------------
-- Plugins
-- ---------------------------------------------------------------------

type CodeActionProvider =  PluginId
                        -> VersionedTextDocumentIdentifier
                        -> Range
                        -> CodeActionContext
                        -> IdeM (IdeResult [CodeAction])

type DiagnosticProviderFuncSync
  = DiagnosticTrigger -> Uri
  -> IdeDeferM (IdeResult (Map.Map Uri (S.Set Diagnostic)))

type DiagnosticProviderFuncAsync
  = DiagnosticTrigger -> Uri
  -> (Map.Map Uri (S.Set Diagnostic) -> IO ())
  -> IdeDeferM (IdeResult ())

data DiagnosticProviderFunc
  = DiagnosticProviderSync  DiagnosticProviderFuncSync
  | DiagnosticProviderAsync DiagnosticProviderFuncAsync


data DiagnosticProvider = DiagnosticProvider
     { dpTrigger :: S.Set DiagnosticTrigger -- AZ:should this be a NonEmptyList?
     , dpFunc    :: DiagnosticProviderFunc
     }

data DiagnosticTrigger = DiagnosticOnOpen
                       | DiagnosticOnChange
                       | DiagnosticOnSave
                       deriving (Show,Ord,Eq)

type HoverProvider = Uri -> Position -> IdeM (IdeResult [Hover])

type SymbolProvider = Uri -> IdeDeferM (IdeResult [DocumentSymbol])

-- | Format the given Text as a whole or only a @Range@ of it.
-- Range must be relative to the text to format.
-- To format the whole document, read the Text from the file and use 'FormatText'
-- as the FormattingType.
data FormattingType = FormatText
                    | FormatRange Range

-- | Formats the given Text associated with the given Uri.
-- Should, but might not, honour the provided formatting options (e.g. Floskell does not).
-- A formatting type can be given to either format the whole text or only a Range.
--
-- Text to format, may or may not, originate from the associated Uri.
-- E.g. it is ok, to modify the text and then reformat it through this API.
--
-- The Uri is mainly used to discover formatting configurations in the file's path.
--
-- Fails if the formatter can not parse the source.
-- Failing means here that a IdeResultFail is returned.
-- This can be used to display errors to the user, unless the error is an Internal one.
-- The record 'IdeError' and 'IdeErrorCode' can be used to determine the type of error.
--
--
-- To format a whole document, the 'FormatText' @FormattingType@ can be used.
-- It is required to pass in the whole Document Text for that to happen, an empty text
-- and file uri, does not suffice.
type FormattingProvider = T.Text -- ^ Text to format
        -> Uri -- ^ Uri of the file being formatted
        -> FormattingType  -- ^ How much to format
        -> FormattingOptions -- ^ Options for the formatter
        -> IdeM (IdeResult [TextEdit]) -- ^ Result of the formatting or the unchanged text.

newtype PluginId = PluginId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString PluginId where
  fromString = PluginId . T.pack

data PluginDescriptor =
  PluginDescriptor { pluginId                 :: PluginId
                   , pluginName               :: T.Text
                   , pluginDesc               :: T.Text
                   , pluginCommands           :: [PluginCommand]
                   , pluginCodeActionProvider :: Maybe CodeActionProvider
                   , pluginDiagnosticProvider :: Maybe DiagnosticProvider
                   , pluginHoverProvider      :: Maybe HoverProvider
                   , pluginSymbolProvider     :: Maybe SymbolProvider
                   , pluginFormattingProvider :: Maybe FormattingProvider
                   } deriving (Generic)

instance Show PluginCommand where
  show (PluginCommand i _ _) = "PluginCommand { name = " ++ show i ++ " }"

newtype CommandId = CommandId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString CommandId where
  fromString = CommandId . T.pack

data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
  PluginCommand { commandId   :: CommandId
                , commandDesc :: T.Text
                , commandFunc :: a -> IdeGhcM (IdeResult b)
                }

pluginDescToIdePlugins :: [PluginDescriptor] -> IdePlugins
pluginDescToIdePlugins plugins = IdePlugins $ Map.fromList $ map (\p -> (pluginId p, p)) plugins

type DynamicJSON = CD.ConstrainedDynamic ToJSON

dynToJSON :: DynamicJSON -> Value
dynToJSON x = CD.applyClassFn x toJSON

fromDynJSON :: (Typeable a, ToJSON a) => DynamicJSON -> Maybe a
fromDynJSON = CD.fromDynamic

toDynJSON :: (Typeable a, ToJSON a) => a -> DynamicJSON
toDynJSON = CD.toDyn

-- | Runs a plugin command given a PluginId, CommandId and
-- arguments in the form of a JSON object.
runPluginCommand :: PluginId -> CommandId -> Value
                  -> IdeGhcM (IdeResult DynamicJSON)
runPluginCommand p@(PluginId p') com@(CommandId com') arg = do
  IdePlugins m <- getPlugins
  case Map.lookup p m of
    Nothing -> return $
      IdeResultFail $ IdeError UnknownPlugin ("Plugin " <> p' <> " doesn't exist") Null
    Just PluginDescriptor { pluginCommands = xs } -> case List.find ((com ==) . commandId) xs of
      Nothing -> return $ IdeResultFail $
        IdeError UnknownCommand ("Command " <> com' <> " isn't defined for plugin " <> p' <> ". Legal commands are: " <> T.pack(show $ map commandId xs)) Null
      Just (PluginCommand _ _ f) -> case fromJSON arg of
        Error err -> return $ IdeResultFail $
          IdeError ParameterError ("error while parsing args for " <> com' <> " in plugin " <> p' <> ": " <> T.pack err) Null
        Success a -> do
            res <- f a
            return $ fmap toDynJSON res

-- | a Description of the available commands stored in IdeGhcM
newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId PluginDescriptor
  } deriving (Generic)

-- | For the diagnostic providers in the config, return a map of
-- current enabled state, indexed by the plugin id.
getDiagnosticProvidersConfig :: Config -> Map.Map PluginId Bool
getDiagnosticProvidersConfig c = Map.fromList [("applyrefact",hlintOn c)
                                              ,("liquid",     liquidOn c)
                                              ]

-- ---------------------------------------------------------------------
-- Monads
-- ---------------------------------------------------------------------

-- | IdeM that allows for interaction with the Ghc session
type IdeGhcM = GhcT IdeM

-- | Run an IdeGhcM with Cradle found from the current directory
runIdeGhcM :: Maybe FilePath -> IdePlugins -> Core.LspFuncs Config -> TVar IdeState -> IdeGhcM a -> IO a
runIdeGhcM mlibdir plugins lf stateVar f = do
  env <- IdeEnv <$> pure lf <*> getProcessID <*> pure plugins
  flip runReaderT stateVar $ flip runReaderT env $ runGhcT mlibdir f

-- | A computation that is deferred until the module is cached.
-- Note that the module may not typecheck, in which case 'UriCacheFailed' is passed
data Defer a = Defer FilePath (UriCacheResult -> a) deriving Functor
type IdeDeferM = FreeT Defer IdeM

type IdeM = ReaderT IdeEnv (MultiThreadState IdeState)

-- | Run an IdeM
runIdeM :: IdePlugins -> Core.LspFuncs Config -> TVar IdeState -> IdeM a -> IO a
runIdeM plugins lf stateVar f = do
  env <- IdeEnv <$> pure lf <*> getProcessID <*> pure plugins
  -- TODO: AZ run a single ReaderT, with a composite R.
  flip runReaderT stateVar $ runReaderT f env

data IdeEnv = IdeEnv
  { ideEnvLspFuncs :: Core.LspFuncs Config
  -- | The pid of this instance of hie
  , ideEnvPidCache :: Int
  , idePlugins  :: IdePlugins
  }

-- | The class of monads that support common IDE functions, namely IdeM/IdeGhcM/IdeDeferM
class Monad m => MonadIde m where
  getIdeEnv :: m IdeEnv

instance MonadIde IdeM where
  getIdeEnv = ask

instance MonadIde IdeDeferM where
  getIdeEnv = lift ask

instance MonadIde IdeGhcM where
  getIdeEnv = lift ask

getRootPath :: MonadIde m => m (Maybe FilePath)
getRootPath = Core.rootPath . ideEnvLspFuncs <$> getIdeEnv

getVirtualFile :: (MonadIde m, MonadIO m) => Uri -> m (Maybe VirtualFile)
getVirtualFile uri = do
  lf <- ideEnvLspFuncs <$> getIdeEnv
  liftIO $ Core.getVirtualFileFunc lf (toNormalizedUri uri)

-- | Worker function for persistVirtualFile without monad constraints.
--
-- Persist a virtual file as a temporary file in the filesystem.
-- If the virtual file associated to the given uri does not exist, Nothing
-- is returned.
persistVirtualFile' :: Core.LspFuncs Config -> Uri -> IO (Maybe FilePath)
persistVirtualFile' lf uri = Core.persistVirtualFileFunc lf (toNormalizedUri uri)

reverseFileMap :: (MonadIde m, MonadIO m) => m (FilePath -> FilePath)
reverseFileMap = do
  lf <- ideEnvLspFuncs <$> getIdeEnv
  liftIO $ Core.reverseFileMapFunc lf

-- | Get the location of the virtual file persisted to the file system associated
-- to the given Uri.
getPersistedFile :: (MonadIde m, MonadIO m) => Uri -> m (Maybe FilePath)
getPersistedFile uri = do
  lf <- ideEnvLspFuncs <$> getIdeEnv
  liftIO $ persistVirtualFile' lf uri

-- | Execute an action on the temporary file associated to the given FilePath.
-- If the file is not in the current Virtual File System, the given action is not executed
-- and instead returns the default value.
withMappedFile :: (MonadIde m, MonadIO m) => FilePath -> m a -> (FilePath -> m a) -> m a
withMappedFile fp m k = do
  canon <- liftIO $ canonicalizePath fp
  getPersistedFile (filePathToUri canon) >>= \case
    Just fp' -> k fp'
    Nothing -> m

getConfig :: (MonadIde m, MonadIO m) => m Config
getConfig = do
  lf <- ideEnvLspFuncs <$> getIdeEnv
  fromMaybe def <$> liftIO (Core.config lf)

getClientCapabilities :: MonadIde m => m ClientCapabilities
getClientCapabilities = Core.clientCapabilities . ideEnvLspFuncs <$> getIdeEnv

getPlugins :: MonadIde m => m IdePlugins
getPlugins = idePlugins <$> getIdeEnv

-- | 'withProgress' @title cancellable f@ wraps a progress reporting session for long running tasks.
-- f is passed a reporting function that can be used to give updates on the progress
-- of the task.
withProgress :: (MonadIde m , MonadBaseControl IO m)
             => T.Text -> Core.ProgressCancellable
             -> ((Core.Progress -> IO ()) -> m a) -> m a
withProgress t c f = do
  lf <- ideEnvLspFuncs <$> getIdeEnv
  control $ \run -> Core.withProgress lf t c $ \update -> run (f update)


-- | 'withIndefiniteProgress' @title cancellable f@ is the same as the 'withProgress' but for tasks
-- which do not continuously report their progress.
withIndefiniteProgress :: (MonadIde m, MonadBaseControl IO m)
                       => T.Text -> Core.ProgressCancellable -> m a -> m a
withIndefiniteProgress t c f = do
  lf <- ideEnvLspFuncs <$> getIdeEnv
  control $ \run -> Core.withIndefiniteProgress lf t c (run f)

data IdeState = IdeState
  { moduleCache :: !GhcModuleCache
  -- | A queue of requests to be performed once a module is loaded
  , requestQueue :: !(Map.Map FilePath [UriCacheResult -> IdeM ()])
  , extensibleState :: !(Map.Map TypeRep Dynamic)
  , ghcSession  :: !(Maybe (IORef HscEnv))
  }

instance MonadMTState IdeState IdeGhcM where
  readMTS = lift $ lift readMTS
  modifyMTS = lift . lift . modifyMTS

instance MonadMTState IdeState IdeDeferM where
  readMTS = lift readMTS
  modifyMTS = lift . modifyMTS

instance MonadMTState IdeState IdeM where
  readMTS = lift readMTS
  modifyMTS = lift . modifyMTS

class (Monad m) => LiftsToGhc m where
  liftToGhc :: m a -> IdeGhcM a

instance LiftsToGhc IdeM where
  liftToGhc = lift

instance LiftsToGhc IdeGhcM where
  liftToGhc = id

instance HasGhcModuleCache IdeGhcM where
  getModuleCache = lift getModuleCache
  modifyModuleCache = lift . modifyModuleCache

instance HasGhcModuleCache IdeDeferM where
  getModuleCache = lift getModuleCache
  modifyModuleCache = lift . modifyModuleCache

instance HasGhcModuleCache IdeM where
  getModuleCache = do
    tvar <- lift ask
    state <- readTVarIO tvar
    return (moduleCache state)
  modifyModuleCache f = do
    tvar <- lift ask
    atomically $ modifyTVar' tvar (\st -> st { moduleCache = f (moduleCache st) })

-- ---------------------------------------------------------------------
-- Results
-- ---------------------------------------------------------------------

-- | The result of a plugin action, containing the result and an error if
-- it failed. IdeGhcM usually skips IdeResponse and jumps straight to this.
data IdeResult a = IdeResultOk a
                 | IdeResultFail IdeError
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Functor IdeResult where
  fmap f (IdeResultOk x) = IdeResultOk (f x)
  fmap _ (IdeResultFail err) = IdeResultFail err

instance Applicative IdeResult where
  pure = return
  (IdeResultFail err) <*> _ = IdeResultFail err
  _ <*> (IdeResultFail err) = IdeResultFail err
  (IdeResultOk f) <*> (IdeResultOk x) = IdeResultOk (f x)

instance Monad IdeResult where
  return = IdeResultOk
  IdeResultOk x >>= f = f x
  IdeResultFail err >>= _ = IdeResultFail err

newtype IdeResultT m a = IdeResultT { runIdeResultT :: m (IdeResult a) }

instance Monad m => Functor (IdeResultT m) where
  fmap = liftM

instance Monad m => Applicative (IdeResultT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (IdeResultT m) where
  return = IdeResultT . return . IdeResultOk

  m >>= f = IdeResultT $ do
    v <- runIdeResultT m
    case v of
      IdeResultOk x -> runIdeResultT (f x)
      IdeResultFail err -> return $ IdeResultFail err

instance MonadTrans IdeResultT where
  lift m = IdeResultT (fmap IdeResultOk m)

-- | Error codes. Add as required
data IdeErrorCode
 = ParameterError          -- ^ Wrong parameter type
 | PluginError             -- ^ An error returned by a plugin
 | InternalError           -- ^ Code error (case not handled or deemed
                           --   impossible)
 | UnknownPlugin           -- ^ Plugin is not registered
 | UnknownCommand          -- ^ Command is not registered
 | InvalidContext          -- ^ Context invalid for command
 | OtherError              -- ^ An error for which there's no better code
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic)

instance ToJSON IdeErrorCode
instance FromJSON IdeErrorCode

-- | A more structured error than just a string
data IdeError = IdeError
 { ideCode    :: IdeErrorCode -- ^ The error code
 , ideMessage :: T.Text       -- ^ A human readable message
 , ideInfo    :: Value        -- ^ Additional information
 }
 deriving (Show,Read,Eq,Generic)

instance ToJSON IdeError
instance FromJSON IdeError

instance ExceptionMonad m => ExceptionMonad (ReaderT e m) where
  gcatch (ReaderT m) c = ReaderT $ \r -> m r `gcatch` \e -> runReaderT (c e) r
  gmask a = ReaderT $ \e -> gmask $ \u -> runReaderT (a $ q u) e
    where q :: (m a -> m a) -> ReaderT e m a -> ReaderT e m a
          q u (ReaderT b) = ReaderT (u . b)

instance MonadTrans GhcT where
  lift = liftGhcT


instance MonadUnliftIO Ghc where
    {-# INLINE withRunInIO #-}
    withRunInIO inner =
      Ghc $ \s ->
      withRunInIO $ \run ->
      inner (run . flip unGhc s)

instance MonadUnliftIO (GhcT IdeM) where
    {-# INLINE withRunInIO #-}
    withRunInIO inner =
      GhcT $ \s ->
      withRunInIO $ \run ->
      inner (run . flip unGhcT s)

instance MonadTransControl GhcT where
    type StT GhcT a = a

    {-# INLINABLE liftWith #-}
    liftWith f = GhcT $ \s -> f $ \t -> unGhcT t s

    {-# INLINABLE restoreT #-}
    restoreT = GhcT . const

instance MonadBaseControl IO (GhcT IdeM) where
    type StM (GhcT IdeM) a = ComposeSt GhcT IdeM a;

    {-# INLINABLE liftBaseWith #-}
    liftBaseWith = defaultLiftBaseWith

    {-# INLINABLE restoreM #-}
    restoreM = defaultRestoreM

instance MonadBase IO (GhcT IdeM) where

    {-# INLINABLE liftBase #-}
    liftBase = liftBaseDefault


instance MonadPlus (GhcT IdeM) where
    {-# INLINE mzero #-}
    mzero = lift mzero

    {-# INLINE mplus #-}
    m `mplus` n = GhcT $ \s -> unGhcT m s `mplus` unGhcT n s

instance Alternative (GhcT IdeM) where
    {-# INLINE empty #-}
    empty = lift empty

    {-# INLINE (<|>) #-}
    m <|> n = GhcT $ \s ->  unGhcT m s <|> unGhcT n s

-- ghc-8.6 required
-- {-# LANGUAGE DerivingVia #-}
-- deriving via (ReaderT Session IO) instance MonadUnliftIO Ghc
-- deriving via (ReaderT Session IdeM) instance MonadUnliftIO (GhcT IdeM)
-- deriving via (ReaderT Session IdeM) instance MonadBaseControl IO (GhcT IdeM)
-- deriving via (ReaderT Session IdeM) instance MonadBase IO (GhcT IdeM)
-- deriving via (ReaderT Session IdeM) instance MonadPlus (GhcT IdeM)
-- deriving via (ReaderT Session IdeM) instance Alternative (GhcT IdeM)
