{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternSynonyms   #-}

-- | IdeGhcM and associated types
module Haskell.Ide.Engine.PluginsIdeMonads
  (
  -- * Plugins
    PluginId
  , CommandName
  , CommandFunc(..)
  , PluginDescriptor(..)
  , PluginCommand(..)
  , IdePlugins(..)
  -- * The IDE monad
  , IdeGhcM
  , IdeState(..)
  , IdeM
  , liftToGhc
  -- * IdeResult and IdeResponse
  , IdeResult(..)
  , IdeResultT(..)
  , pattern IdeResponseOk
  , pattern IdeResponseFail
  , IdeResponse(..)
  , IdeResponseT(..)
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

import           Language.Haskell.LSP.Types (Diagnostic (..),
                                             DiagnosticSeverity (..),
                                             List (..),
                                             Location (..),
                                             Position (..),
                                             PublishDiagnosticsParams (..),
                                             Range (..),
                                             TextDocumentIdentifier (..),
                                             TextDocumentPositionParams (..),
                                             Uri (..),
                                             WorkspaceEdit (..),
                                             filePathToUri,
                                             uriToFilePath)


type PluginId = T.Text
type CommandName = T.Text

newtype CommandFunc a b = CmdSync (a -> IdeGhcM (IdeResult b))

data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
  PluginCommand { commandName :: CommandName
                , commandDesc :: T.Text
                , commandFunc :: CommandFunc a b
                }

data PluginDescriptor =
  PluginDescriptor { pluginName :: T.Text
                   , pluginDesc :: T.Text
                   , pluginCommands :: [PluginCommand]
                   } deriving (Show,Generic)

instance Show PluginCommand where
  show (PluginCommand name _ _) = "PluginCommand { name = " ++ T.unpack name ++ " }"

-- | a Description of the available commands stored in IdeGhcM
newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId [PluginCommand]
  } deriving (Show,Generic)

instance ToJSON IdePlugins where
  toJSON (IdePlugins m) = toJSON $ (fmap . fmap) (\x -> (commandName x, commandDesc x)) m

-- ---------------------------------------------------------------------

type IdeGhcM = GM.GhcModT IdeM

instance MonadMTState IdeState IdeGhcM where
  readMTS = lift $ lift $ readMTS
  modifyMTS f = lift $ lift $ modifyMTS f

type IdeM = MultiThreadState IdeState

liftToGhc :: IdeM a -> IdeGhcM a
liftToGhc = lift . lift

data IdeState = IdeState
  { moduleCache :: GhcModuleCache
  , idePlugins  :: IdePlugins
  , extensibleState :: !(Map.Map TypeRep Dynamic)
  , ghcSession  :: (Maybe (IORef HscEnv))
  }

instance HasGhcModuleCache IdeM where
  getModuleCache = do
    tvar <- ask
    state <- liftIO $ readTVarIO tvar
    return (moduleCache state)
  setModuleCache mc = do
    tvar <- ask
    liftIO $ atomically $ modifyTVar' tvar (\st -> st { moduleCache = mc })

instance HasGhcModuleCache IdeGhcM where
  getModuleCache = lift . lift $ getModuleCache
  setModuleCache = lift . lift . setModuleCache



-- ---------------------------------------------------------------------


-- | The result of a plugin action, containing the result and an error if
-- it failed.
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

-- | The IDE response, which wraps around an IdeResult that may be deferred.
-- Used mostly in IdeM.
data IdeResponse a = IdeResponseResult (IdeResult a)
                   | IdeResponseDeferred FilePath (CachedModule -> IdeGhcM (IdeResponse a))

pattern IdeResponseOk :: a -> IdeResponse a
pattern IdeResponseOk a = IdeResponseResult (IdeResultOk a)
pattern IdeResponseFail :: IdeError -> IdeResponse a
pattern IdeResponseFail err = IdeResponseResult (IdeResultFail err)

instance Functor IdeResponse where
  fmap f (IdeResponseResult (IdeResultOk x)) = IdeResponseOk (f x)
  fmap _ (IdeResponseResult (IdeResultFail err)) = IdeResponseFail err
  fmap f (IdeResponseDeferred fp cb) = IdeResponseDeferred fp $ cb >=> (return . fmap f)

instance Applicative IdeResponse where
  pure = return

  (IdeResponseResult (IdeResultFail err)) <*> _ = IdeResponseFail err
  _ <*> (IdeResponseResult (IdeResultFail err)) = IdeResponseFail err

  (IdeResponseResult (IdeResultOk f)) <*> (IdeResponseResult (IdeResultOk x)) = IdeResponseOk (f x)

  (IdeResponseResult (IdeResultOk f)) <*> (IdeResponseDeferred fp cb) = IdeResponseDeferred fp $ fmap (fmap f) . cb

  (IdeResponseDeferred fp cb) <*> x = IdeResponseDeferred fp $ \cm -> do
    f <- cb cm
    pure (f <*> x)

instance Monad IdeResponse where
  (IdeResponseResult (IdeResultOk x)) >>= f = f x
  (IdeResponseDeferred fp cb) >>= f = IdeResponseDeferred fp $ \cm -> do
    x <- cb cm
    return $ x >>= f
  (IdeResponseResult (IdeResultFail err)) >>= _ = IdeResponseFail err
  return = IdeResponseOk

newtype IdeResponseT m a = IdeResponseT { runIdeResponseT :: m (IdeResponse a) }

instance Monad m => Functor (IdeResponseT m) where
  fmap = liftM

instance Monad m => Applicative (IdeResponseT m) where
  pure = return
  (<*>) = ap

instance (Monad m) => Monad (IdeResponseT m) where
  return = IdeResponseT . return . IdeResponseOk
  
  m >>= f = IdeResponseT $ do
    v <- runIdeResponseT m
    case v of
      IdeResponseResult (IdeResultOk x) -> runIdeResponseT (f x)
      IdeResponseResult (IdeResultFail err) -> return $ IdeResponseFail err
      IdeResponseDeferred _ _ -> error "TODO"

instance MonadTrans IdeResponseT where
  lift m = IdeResponseT (fmap IdeResponseOk m)

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
 | ParseError              -- ^ Input could not be parsed
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
