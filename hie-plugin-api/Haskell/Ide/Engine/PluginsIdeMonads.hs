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

-- | IdeGhcM and associated types
module Haskell.Ide.Engine.PluginsIdeMonads
  (
  -- * LSP Commands
    HasPidCache(..)
  , mkLspCommand
  , allLspCmdIds
  , mkLspCmdId
  -- * Plugins
  , PluginId
  , CommandName
  , PluginDescriptor(..)
  , pluginDescToIdePlugins
  , PluginCommand(..)
  , CommandFunc(..)
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
  , MonadIde(..)
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

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Free

import           Data.Aeson
import qualified Data.ConstrainedDynamic       as CD
import           Data.Default
import qualified Data.List                     as List
import           Data.Dynamic                   ( Dynamic )
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Monoid                    ( (<>) )
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Data.Typeable                  ( TypeRep
                                                , Typeable
                                                )

import qualified GhcMod.Monad                  as GM
import qualified GhcMod.Types                  as GM
import           GHC.Generics
import           GHC                            ( HscEnv )

import           Haskell.Ide.Engine.Compat
import           Haskell.Ide.Engine.Config
import           Haskell.Ide.Engine.MultiThreadState
import           Haskell.Ide.Engine.GhcModuleCache

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

mkLspCommand :: HasPidCache m => PluginId -> CommandName -> T.Text -> Maybe [Value] -> m Command
mkLspCommand plid cn title args' = do
  cmdId <- mkLspCmdId plid cn
  let args = List <$> args'
  return $ Command title cmdId args

allLspCmdIds :: HasPidCache m => IdePlugins -> m [T.Text]
allLspCmdIds (IdePlugins m) = concat <$> mapM go (Map.toList (pluginCommands <$> m))
  where
    go (plid, cmds) = mapM (mkLspCmdId plid . commandName) cmds

mkLspCmdId :: HasPidCache m => PluginId -> CommandName -> m T.Text
mkLspCmdId plid cn = do
  pid <- T.pack . show <$> getPidCache
  return $ pid <> ":" <> plid <> ":" <> cn

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

-- | Format the document either as a whole or only a given Range of it.
data FormattingType = FormatDocument
                    | FormatRange Range

-- | Formats the given Text associated with the given Uri.
-- Should, but might not, honor the provided formatting options (e.g. Floskell does not).
-- A formatting type can be given to either format the whole document or only a Range.
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
type FormattingProvider = T.Text -- ^ Text to format
        -> Uri -- ^ Uri of the file being formatted
        -> FormattingType  -- ^ How much to format
        -> FormattingOptions -- ^ Options for the formatter
        -> IdeM (IdeResult [TextEdit]) -- ^ Result of the formatting or the unchanged text.

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
  show (PluginCommand name _ _) = "PluginCommand { name = " ++ T.unpack name ++ " }"

type PluginId = T.Text
type CommandName = T.Text

newtype CommandFunc a b = CmdSync (a -> IdeGhcM (IdeResult b))

data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
  PluginCommand { commandName :: CommandName
                , commandDesc :: T.Text
                , commandFunc :: CommandFunc a b
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

-- | Runs a plugin command given a PluginId, CommandName and
-- arguments in the form of a JSON object.
runPluginCommand :: PluginId -> CommandName -> Value
                  -> IdeGhcM (IdeResult DynamicJSON)
runPluginCommand p com arg = do
  IdePlugins m <- getPlugins
  case Map.lookup p m of
    Nothing -> return $
      IdeResultFail $ IdeError UnknownPlugin ("Plugin " <> p <> " doesn't exist") Null
    Just PluginDescriptor { pluginCommands = xs } -> case List.find ((com ==) . commandName) xs of
      Nothing -> return $ IdeResultFail $
        IdeError UnknownCommand ("Command " <> com <> " isn't defined for plugin " <> p <> ". Legal commands are: " <> T.pack(show $ map commandName xs)) Null
      Just (PluginCommand _ _ (CmdSync f)) -> case fromJSON arg of
        Error err -> return $ IdeResultFail $
          IdeError ParameterError ("error while parsing args for " <> com <> " in plugin " <> p <> ": " <> T.pack err) Null
        Success a -> do
            res <- f a
            return $ fmap toDynJSON res

-- | a Description of the available commands stored in IdeGhcM
newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId PluginDescriptor
  } deriving (Generic)

-- TODO:AZ this is a defective instance, do we actually need it?
-- Perhaps rather make a separate type explicitly for this purpose.
instance ToJSON IdePlugins where
  toJSON (IdePlugins m) = toJSON $ fmap (\x -> (commandName x, commandDesc x)) <$> fmap pluginCommands m

-- | For the diagnostic providers in the config, return a map of
-- current enabled state, indexed by the plugin id.
getDiagnosticProvidersConfig :: Config -> Map.Map PluginId Bool
getDiagnosticProvidersConfig c = Map.fromList [("applyrefact",hlintOn c)
                                              ,("liquid",     liquidOn c)
                                              ]

-- ---------------------------------------------------------------------
-- Monads
-- ---------------------------------------------------------------------

-- | IdeM that allows for interaction with the ghc-mod session
type IdeGhcM = GM.GhcModT IdeM

-- | Run an IdeGhcM with Cradle found from the current directory
runIdeGhcM :: GM.Options -> IdePlugins -> Maybe (Core.LspFuncs Config) -> TVar IdeState -> IdeGhcM a -> IO a
runIdeGhcM ghcModOptions plugins mlf stateVar f = do
  env <- IdeEnv <$> pure mlf <*> getProcessID <*> pure plugins
  (eres, _) <- flip runReaderT stateVar $ flip runReaderT env $ GM.runGhcModT ghcModOptions f
  case eres of
      Left err  -> liftIO $ throwIO err
      Right res -> return res

-- | A computation that is deferred until the module is cached.
-- Note that the module may not typecheck, in which case 'UriCacheFailed' is passed
data Defer a = Defer FilePath (UriCacheResult -> a) deriving Functor
type IdeDeferM = FreeT Defer IdeM

type IdeM = ReaderT IdeEnv (MultiThreadState IdeState)

-- | Run an IdeM
runIdeM :: IdePlugins -> Maybe (Core.LspFuncs Config) -> TVar IdeState -> IdeM a -> IO a
runIdeM plugins mlf stateVar f = do
  env <- IdeEnv <$> pure mlf <*> getProcessID <*> pure plugins
  -- TODO: AZ run a single ReaderT, with a composite R.
  flip runReaderT stateVar $ runReaderT f env

data IdeEnv = IdeEnv
  { ideEnvLspFuncs :: Maybe (Core.LspFuncs Config)
  -- | The pid of this instance of hie
  , ideEnvPidCache :: Int
  , idePlugins  :: IdePlugins
  }

-- | The class of monads that support common IDE functions, namely IdeM/IdeGhcM/IdeDeferM
class Monad m => MonadIde m where
  getRootPath :: m (Maybe FilePath)
  getVirtualFile :: Uri -> m (Maybe VirtualFile)
  getConfig :: m Config
  getClientCapabilities :: m ClientCapabilities
  getPlugins :: m IdePlugins

instance MonadIde IdeM where
  getRootPath = do
    mlf <- asks ideEnvLspFuncs
    case mlf of
      Just lf -> return (Core.rootPath lf)
      Nothing -> return Nothing

  getVirtualFile uri = do
    mlf <- asks ideEnvLspFuncs
    case mlf of
      Just lf -> liftIO $ Core.getVirtualFileFunc lf uri
      Nothing -> return Nothing

  getConfig = do
    mlf <- asks ideEnvLspFuncs
    case mlf of
      Just lf -> fromMaybe def <$> liftIO (Core.config lf)
      Nothing -> return def

  getClientCapabilities = do
    mlf <- asks ideEnvLspFuncs
    case mlf of
      Just lf -> return (Core.clientCapabilities lf)
      Nothing -> return def

  getPlugins = asks idePlugins

instance MonadIde IdeGhcM where
  getRootPath = lift $ lift getRootPath
  getVirtualFile = lift . lift . getVirtualFile
  getConfig = lift $ lift getConfig
  getClientCapabilities = lift $ lift getClientCapabilities
  getPlugins = lift $ lift getPlugins

instance MonadIde IdeDeferM where
  getRootPath = lift getRootPath
  getVirtualFile = lift . getVirtualFile
  getConfig = lift getConfig
  getClientCapabilities = lift getClientCapabilities
  getPlugins = lift getPlugins

data IdeState = IdeState
  { moduleCache :: GhcModuleCache
  -- | A queue of requests to be performed once a module is loaded
  , requestQueue :: Map.Map FilePath [UriCacheResult -> IdeM ()]
  , extensibleState :: !(Map.Map TypeRep Dynamic)
  , ghcSession  :: Maybe (IORef HscEnv)
  }

instance MonadMTState IdeState IdeGhcM where
  readMTS = lift $ lift $ lift readMTS
  modifyMTS = lift . lift . lift . modifyMTS

instance MonadMTState IdeState IdeDeferM where
  readMTS = lift $ lift readMTS
  modifyMTS = lift . lift . modifyMTS

instance MonadMTState IdeState IdeM where
  readMTS = lift readMTS
  modifyMTS = lift . modifyMTS

class (Monad m) => LiftsToGhc m where
  liftToGhc :: m a -> IdeGhcM a

instance GM.MonadIO IdeDeferM where
  liftIO = liftIO

instance LiftsToGhc IdeM where
  liftToGhc = lift . lift

instance LiftsToGhc IdeGhcM where
  liftToGhc = id

instance HasGhcModuleCache IdeGhcM where
  getModuleCache = lift $ lift getModuleCache
  setModuleCache = lift . lift . setModuleCache

instance HasGhcModuleCache IdeDeferM where
  getModuleCache = lift getModuleCache
  setModuleCache = lift . setModuleCache

instance HasGhcModuleCache IdeM where
  getModuleCache = do
    tvar <- lift ask
    state <- liftIO $ readTVarIO tvar
    return (moduleCache state)
  setModuleCache mc = do
    tvar <- lift ask
    liftIO $ atomically $ modifyTVar' tvar (\st -> st { moduleCache = mc })

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
