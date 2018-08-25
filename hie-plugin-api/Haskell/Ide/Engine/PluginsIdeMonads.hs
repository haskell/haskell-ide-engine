{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

-- | IdeGhcM and associated types
module Haskell.Ide.Engine.PluginsIdeMonads
  (
  -- * Plugins
    PluginId
  , CommandName
  , HasPidCache(..)
  , mkLspCommand
  , allLspCmdIds
  , mkLspCmdId
  , CommandFunc(..)
  , PluginDescriptor(..)
  , PluginCommand(..)
  , CodeActionProvider
  , DiagnosticProvider(..)
  , DiagnosticProviderFunc
  , DiagnosticTrigger(..)
  , HoverProvider
  , SymbolProvider
  , IdePlugins(..)
  -- * The IDE monad
  , IdeState(..)
  , IdeGhcM
  , IdeM
  , iterT
  , LiftsToGhc(..)
  -- * IdeResult
  , IdeResult(..)
  , IdeResultT(..)
  , IdeDefer(..)
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
  , WorkspaceEdit(..)
  , Diagnostic(..)
  , DiagnosticSeverity(..)
  , PublishDiagnosticsParams(..)
  , List(..)
  ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Free

import           Data.Aeson
import           Data.Dynamic (Dynamic)
import           Data.IORef
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Typeable (TypeRep, Typeable)

import qualified GhcMod.Monad        as GM
import           GHC.Generics
import           GHC (HscEnv)

import           Haskell.Ide.Engine.Compat
import           Haskell.Ide.Engine.MultiThreadState
import           Haskell.Ide.Engine.GhcModuleCache

import           Language.Haskell.LSP.Types.Capabilities
import           Language.Haskell.LSP.Types (Command (..),
                                             CodeAction (..),
                                             CodeActionContext (..),
                                             Diagnostic (..),
                                             DiagnosticSeverity (..),
                                             DocumentSymbol (..),
                                             List (..),
                                             Hover (..),
                                             Location (..),
                                             Position (..),
                                             PublishDiagnosticsParams (..),
                                             Range (..),
                                             TextDocumentIdentifier (..),
                                             TextDocumentPositionParams (..),
                                             Uri (..),
                                             VersionedTextDocumentIdentifier(..),
                                             WorkspaceEdit (..),
                                             filePathToUri,
                                             uriToFilePath)
import           System.Directory


type PluginId = T.Text
type CommandName = T.Text

newtype CommandFunc a b = CmdSync (a -> IdeGhcM (IdeResult b))

data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
  PluginCommand { commandName :: CommandName
                , commandDesc :: T.Text
                , commandFunc :: CommandFunc a b
                }

-- ---------------------------------------------------------------------

class Monad m => HasPidCache m where
  getPidCache :: m Int

instance HasPidCache IdeM where
  getPidCache = idePidCache <$> readMTS

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

type CodeActionProvider =  PluginId
                        -> VersionedTextDocumentIdentifier
                        -> Maybe FilePath -- ^ Project root directory
                        -> Range
                        -> CodeActionContext
                        -> IdeM (IdeResult [CodeAction])

-- type DiagnosticProviderFunc = DiagnosticTrigger -> Uri -> IdeM (IdeResponse (Map.Map Uri (S.Set Diagnostic)))
type DiagnosticProviderFunc
  = DiagnosticTrigger -> Uri -> IdeM (IdeResult (Map.Map Uri (S.Set Diagnostic)))

data DiagnosticProvider = DiagnosticProvider
     { dpTrigger :: S.Set DiagnosticTrigger -- AZ:should this be a NonEmptyList?
     , dpFunc    :: DiagnosticProviderFunc
     }

data DiagnosticTrigger = DiagnosticOnOpen
                       | DiagnosticOnChange
                       | DiagnosticOnSave
                       deriving (Show,Ord,Eq)

type HoverProvider = Uri -> Position -> IdeM (IdeResult [Hover])

type SymbolProvider = Uri -> IdeM (IdeResult [DocumentSymbol])

data PluginDescriptor =
  PluginDescriptor { pluginId                 :: PluginId
                   , pluginName               :: T.Text
                   , pluginDesc               :: T.Text
                   , pluginCommands           :: [PluginCommand]
                   , pluginCodeActionProvider :: Maybe CodeActionProvider
                   , pluginDiagnosticProvider :: Maybe DiagnosticProvider
                   , pluginHoverProvider      :: Maybe HoverProvider
                   , pluginSymbolProvider     :: Maybe SymbolProvider
                   } deriving (Generic)

instance Show PluginCommand where
  show (PluginCommand name _ _) = "PluginCommand { name = " ++ T.unpack name ++ " }"

-- | a Description of the available commands stored in IdeGhcM
newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId PluginDescriptor
  } deriving (Generic)

-- TODO:AZ this is a defective instance, do we actually need it?
-- Perhaps rather make a separate type explicitly for this purpose.
instance ToJSON IdePlugins where
  toJSON (IdePlugins m) = toJSON $ fmap (\x -> (commandName x, commandDesc x)) <$> fmap pluginCommands m

-- ---------------------------------------------------------------------

type IdeGhcM = GM.GhcModT IdeBase

-- | A computation that is deferred until the module is cached.
-- Note that the module may not typecheck, in which case 'UriCacheFailed' is passed
data IdeDefer a = IdeDefer FilePath (UriCache -> a) deriving Functor
type IdeM = FreeT IdeDefer IdeBase

type IdeBase = ReaderT ClientCapabilities (MultiThreadState IdeState)

data IdeState = IdeState
  { moduleCache :: GhcModuleCache
  -- | A queue of requests to be performed once a module is loaded
  , requestQueue :: Map.Map FilePath [UriCache -> IdeBase ()]
  , idePlugins  :: IdePlugins
  , extensibleState :: !(Map.Map TypeRep Dynamic)
  , ghcSession  :: Maybe (IORef HscEnv)
  -- The pid of this instance of hie
  , idePidCache    :: Int
  }

instance MonadMTState IdeState IdeGhcM where
  readMTS = lift $ lift $ lift readMTS
  modifyMTS = lift . lift . lift . modifyMTS

instance MonadMTState IdeState IdeM where
  readMTS   = lift $ lift readMTS
  modifyMTS = lift . lift . modifyMTS

class (Monad m) => LiftsToGhc m where
  liftToGhc :: m a -> IdeGhcM a

instance GM.MonadIO IdeM where
  liftIO = liftIO

instance LiftsToGhc IdeM where
  liftToGhc (FreeT f) = do
    x <- liftToGhc f
    case x of
      Pure a -> return a
      Free (IdeDefer fp cb) -> do
        fp' <- liftIO $ canonicalizePath fp
        muc <- fmap (Map.lookup fp' . uriCaches) getModuleCache
        liftToGhc $ case muc of
          Just uc -> cb uc
          Nothing -> cb UriCacheFailed

instance LiftsToGhc IdeBase where
  liftToGhc = lift . lift

instance LiftsToGhc IdeGhcM where
  liftToGhc = id

instance HasGhcModuleCache IdeGhcM where
  getModuleCache = lift $ lift getModuleCache
  setModuleCache = lift . lift . setModuleCache

instance HasGhcModuleCache IdeM where
  getModuleCache = lift getModuleCache
  setModuleCache = lift . setModuleCache

instance HasGhcModuleCache IdeBase where
  getModuleCache = do
    tvar <- lift ask
    state <- liftIO $ readTVarIO tvar
    return (moduleCache state)
  setModuleCache mc = do
    tvar <- lift ask
    liftIO $ atomically $ modifyTVar' tvar (\st -> st { moduleCache = mc })

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
