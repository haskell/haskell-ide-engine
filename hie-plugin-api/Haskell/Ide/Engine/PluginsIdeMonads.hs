{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- | IdeGhcM and associated types
module Haskell.Ide.Engine.PluginsIdeMonads
  (
  -- * Plugins
    PluginId
  , CommandName
  , CommandFunc(..)
  , PluginDescriptor(..)
  , PluginCommand(..)
  , CodeActionProvider
  , noCodeActions
  , IdePlugins(..)
  -- * The IDE monad
  , IdeGhcM
  , IdeState(..)
  , IDErring(..)
  , runIDErring
  , MonadIde(..)
  , IdeResponseT
  , ResponseT
--  , IdeResponse
  , IdeDefer(..)
  , IdeM
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
  , ideError
  , defer
  , moduleCache, requestQueue, idePlugins, extensibleState, ghcSession
  ) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Control
import           Control.Monad.Morph
import           Control.Monad.Base
import           Control.Lens
import           Exception
import           Data.Functor.Classes

import           Data.Aeson
import           Data.Dynamic (Dynamic)
import           Data.IORef
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Typeable (TypeRep, Typeable)

import qualified GhcMod.Monad        as GM
import           GHC.Generics
import           GHC (HscEnv)

import           Haskell.Ide.Engine.MultiThreadState
import           Haskell.Ide.Engine.GhcModuleCache

import           Language.Haskell.LSP.Types.Capabilities
import           Language.Haskell.LSP.Types (CodeAction(..),
                                             CodeActionContext(..),
                                             Diagnostic (..),
                                             DiagnosticSeverity (..),
                                             List (..),
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


type PluginId = T.Text
type CommandName = T.Text

newtype CommandFunc a b = CmdSync (a -> IDErring IdeGhcM b)

data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
  PluginCommand { commandName :: CommandName
                , commandDesc :: T.Text
                , commandFunc :: CommandFunc a b
                }

type CodeActionProvider =  VersionedTextDocumentIdentifier
                        -> Maybe FilePath -- ^ Project root directory
                        -> Range
                        -> CodeActionContext
                        -> IdeResponseT [CodeAction]

data PluginDescriptor =
  PluginDescriptor { pluginName :: T.Text
                   , pluginDesc :: T.Text
                   , pluginCommands :: [PluginCommand]
                   , pluginCodeActionProvider :: CodeActionProvider
                   } deriving (Generic)

instance Show PluginCommand where
  show (PluginCommand name _ _) = "PluginCommand { name = " ++ T.unpack name ++ " }"

noCodeActions :: CodeActionProvider
noCodeActions _ _ _ _ = return []

-- | a Description of the available commands and code action providers stored in IdeGhcM
newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId ([PluginCommand], CodeActionProvider)
  } deriving (Generic)

instance ToJSON IdePlugins where
  toJSON (IdePlugins m) = toJSON $ fmap (\x -> (commandName x, commandDesc x)) <$> fmap fst m

-- ---------------------------------------------------------------------

type IdeGhcM = GM.GhcModT IdeM

newtype IDErring m a = IDErring { getIDErring :: ExceptT IdeError m a }
deriving (Functor, Applicative, Monad, MonadReader r, MonadState s
         , MonadIO, MonadTrans, MonadBase b, MFunctor)
instance GM.MonadIO m => GM.MonadIO (IDErring m) where
  liftIO = lift . GM.liftIO
instance GM.GmEnv m => GM.GmEnv (IDErring m) where
  gmeAsk = lift GM.gmeAsk
  gmeLocal f x = liftWith (\run -> GM.gmeLocal f $ run x) >>= restoreT . return
instance GM.GmLog m => GM.GmLog (IDErring m) where
  gmlJournal = lift . GM.gmlJournal
  gmlHistory = lift GM.gmlHistory
  gmlClear = lift GM.gmlClear
instance GM.GmOut m => GM.GmOut (IDErring m) where
  gmoAsk = lift GM.gmoAsk
instance GM.GmState m => GM.GmState (IDErring m) where
  gmsGet = lift GM.gmsGet
  gmsPut = lift . GM.gmsPut
  gmsState = lift . GM.gmsState
instance (Functor f, MonadFree f m) => MonadFree f (IDErring m) where
  wrap x = liftWith (\run -> wrap $ fmap run x) >>= restoreT . return

runIDErring :: IDErring m a -> m (Either IdeError a)
runIDErring = runExceptT . getIDErring

instance MonadTransControl IDErring where
  type StT IDErring a = StT (ExceptT IdeError) a
  liftWith = defaultLiftWith IDErring getIDErring
  restoreT = defaultRestoreT IDErring
instance MonadBaseControl b m => MonadBaseControl b (IDErring m) where
  type StM (IDErring m) a = ComposeSt IDErring m a
  liftBaseWith     = defaultLiftBaseWith
  restoreM         = defaultRestoreM

type IdeM = ReaderT ClientCapabilities (MultiThreadState IdeState)

class Monad m => MonadIde m where liftIde :: IdeM a -> m a
instance MonadIde IdeGhcM where liftIde = lift . lift
instance MonadIde m => MonadIde (IDErring m) where liftIde = lift . liftIde
instance MonadIde (ResponseT IdeM) where liftIde = lift

data IdeState = IdeState
  { _moduleCache :: GhcModuleCache
  -- | A queue of requests to be performed once a module is loaded
  , _requestQueue :: Map.Map FilePath [Either T.Text CachedModule -> IdeM ()]
  , _idePlugins  :: IdePlugins
  , _extensibleState :: !(Map.Map TypeRep Dynamic)
  , _ghcSession  :: Maybe (IORef HscEnv)
  }

-- | The IDE response, which wraps around an (Either IdeError a) that may be deferred.
-- Used mostly in IdeM.
data IdeDefer a = IdeDefer FilePath (CachedModule -> a) deriving Functor
type ResponseT = FreeT IdeDefer
type IdeResponseT = IDErring (ResponseT IdeM) -- Lightens error messages

instance GM.MonadIO m => GM.MonadIO (ResponseT m) where liftIO = lift . GM.liftIO

defer :: MonadFree IdeDefer m => FilePath -> (CachedModule -> m a) -> m a
defer fp f = wrap $ IdeDefer fp f

instance Show1 IdeDefer where liftShowsPrec _ _ _ (IdeDefer fp _) = (++) $ "Deferred response waiting on " ++ fp
instance Show (IdeDefer a) where show (IdeDefer fp _) = "Deferred response waiting on " ++ fp

-- | Error codes. Add as required
data IdeErrorCode
 = ParameterError          -- ^ Wrong parameter type
 | PluginError             -- ^ An error returned by a plugin
 | InternalError           -- ^ Code error (case not handled or deemed
                           --   impossible)
 | NoModuleAvailable       -- ^ No typechecked module available to use
 | UnknownPlugin           -- ^ Plugin is not registered
 | UnknownCommand          -- ^ Command is not registered
 | InvalidContext          -- ^ Context invalid for command
 | RequestCancelled        -- ^ A cancel request fired targeting this one
 | VersionMismatch         -- ^ The request expected another hie version
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

ideError :: Monad m => IdeErrorCode -> T.Text -> Value -> IDErring m a
ideError c m i = IDErring $ throwError $ IdeError c m i

makeLenses ''IdeState

instance HasGhcModuleCache IdeM where
  getModuleCache = do
    tvar <- lift ask
    liftIO $ view moduleCache <$> readTVarIO tvar
  setModuleCache mc = do
    tvar <- lift ask
    liftIO $ atomically $ modifyTVar' tvar $ moduleCache .~ mc

instance HasGhcModuleCache IdeGhcM where
  getModuleCache = lift . lift $ getModuleCache
  setModuleCache = lift . lift . setModuleCache

instance HasGhcModuleCache m => HasGhcModuleCache (IDErring m) where
  getModuleCache = lift getModuleCache
  setModuleCache = lift . setModuleCache

instance HasGhcModuleCache m => HasGhcModuleCache (ResponseT m) where
  getModuleCache = lift getModuleCache
  setModuleCache = lift . setModuleCache

deriving instance (GM.MonadIO m, ExceptionMonad m) => ExceptionMonad (IDErring m)

instance ExceptionMonad m => ExceptionMonad (ResponseT m) where
  gcatch act handler = let levelonecatch act' handler' = FreeT $ runFreeT act' `gcatch` (runFreeT . handler') in
    (`levelonecatch` handler) . FreeT . (fmap . fmap) (`gcatch` handler) . runFreeT $ act -- afaic we previously only did one level!
  gmask = error "ResponseT hasn't defined gmask!"