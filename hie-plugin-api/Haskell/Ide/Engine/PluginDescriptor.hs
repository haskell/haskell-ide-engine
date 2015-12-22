{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
  , contextMapping

  , fileParam
  , startPosParam
  , endPosParam
  , cabalParam


  -- * Commands
  , Command(..)
  , CommandFunc(..), SyncCommandFunc, AsyncCommandFunc
  , buildCommand

  -- * Plugins
  , Plugins

  -- * The IDE monad
  , IdeM
  , IdeState(..)
  , StateExtension(..)
  , ExtensionClass(..)
  , getPlugins

  -- * All the good types
  , module Haskell.Ide.Engine.PluginTypes
  ) where

import           Haskell.Ide.Engine.PluginTypes

import           Control.Applicative
import           Data.Aeson
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Typeable
import           Control.Monad.State.Strict
import           GHC.Generics
import qualified Language.Haskell.GhcMod.Monad as GM

-- ---------------------------------------------------------------------

data PluginDescriptor = PluginDescriptor
  { pdUIShortName     :: !T.Text
  , pdUIOverview     :: !T.Text
  , pdCommands        :: [Command]
  , pdExposedServices :: [Service]
  , pdUsedServices    :: [Service]
  }

instance Show PluginDescriptor where
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



type Plugins = Map.Map PluginId PluginDescriptor

-- | Ideally a Command is defined in such a way that its CommandDescriptor
-- can be exposed via the native CLI for the tool being exposed as well.
-- Perhaps use Options.Applicative for this in some way.
data Command = forall a. (ValidResponse a) => Command
  { cmdDesc :: !CommandDescriptor
  , cmdFunc :: !(CommandFunc a)
  }

instance Show Command where
  show (Command desc _func) = "(Command " ++ show desc ++ ")"

-- | Build a command, ensuring the command response type name and the command
-- function match
buildCommand :: forall a. (ValidResponse a)
  => CommandFunc a
  -> CommandName
  -> T.Text
  -> [T.Text]
  -> [AcceptedContext]
  -> [ParamDescription]
  -> Command
buildCommand fun n d exts ctxs parm =
  Command
  { cmdDesc = CommandDesc
      { cmdName = n
      , cmdUiDescription = d
      , cmdFileExtensions = exts
      , cmdContexts = ctxs
      , cmdAdditionalParams = parm
      , cmdReturnType = T.pack $ show $ typeOf (undefined::a)
      }
  , cmdFunc = fun
  }

-- ---------------------------------------------------------------------

-- | For a given 'AcceptedContext', define the parameters that are required in
-- the corresponding 'IdeRequest'
contextMapping :: AcceptedContext -> [ParamDescription]
contextMapping CtxNone        = []
contextMapping CtxFile        = [fileParam]
contextMapping CtxPoint       = [fileParam,startPosParam]
contextMapping CtxRegion      = [fileParam,startPosParam,endPosParam]
contextMapping CtxCabalTarget = [cabalParam]
contextMapping CtxProject     = [dirParam] -- the root directory of the project

fileParam :: ParamDescription
fileParam = RP "file" "a file name" PtFile

-- | A parameter for a directory
dirParam :: ParamDescription
dirParam = RP "dir" "a directory name" PtFile

startPosParam :: ParamDescription
startPosParam = RP "start_pos" "start line and col" PtPos

endPosParam :: ParamDescription
endPosParam = RP "end_pos" "end line and col" PtPos

cabalParam :: ParamDescription
cabalParam = RP "cabal" "cabal target" PtText

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
  , extensibleState :: !(Map.Map String (Either String StateExtension))
              -- ^ stores custom state information.
  } deriving (Show)

getPlugins :: IdeM Plugins
getPlugins = lift $ lift $ idePlugins <$> get

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
    -- | Specifies whether the state extension should be
    -- persistent. Setting this method to 'PersistentExtension'
    -- will make the stored data survive restarts, but
    -- requires a to be an instance of Read and Show.
    --
    -- It defaults to 'StateExtension', i.e. no persistence.
    extensionType :: a -> StateExtension
    extensionType = StateExtension

-- | Existential type to store a state extension.
data StateExtension =
    forall a. ExtensionClass a => StateExtension a
    -- ^ Non-persistent state extension
  | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a
    -- ^ Persistent extension

instance Show StateExtension where
  show _ = "StateExtension"

-- EOF
