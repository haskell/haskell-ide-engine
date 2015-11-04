{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
-- | Experimenting with a data structure to define a plugin.
--
-- The general idea is that a given plugin returns this structure during the
-- initial load/registration process, when- or however this eventually happens.
--
-- It should define the following things
--  1. What features the plugin should expose into the IDE
--  2. What resources it requires access to in order to do this
--       (this one may not be needed initially)
--
--       This may include a requirement to store private data of a particular
--       form.
--
--       It may be interesting to look at the Android model wrt Intents and
--       shared resource management, e.g. default Calendar app, default SMS app,
--       all making use of Contacts service.

module Haskell.Ide.Engine.PluginDescriptor where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map as Map
import qualified GHC
import           GHC.Generics

-- ---------------------------------------------------------------------

data PluginDescriptor = PluginDescriptor
  { pdUiCommands      :: [UiCommand]
  , pdExposedServices :: [Service]
  , pdUsedServices    :: [Service]
  } deriving (Show)

-- |Ideally a UiCommand is defined in such a way that it can be exposed via the
-- native CLI for the tool being exposed as well. Perhaps use
-- Options.Applicative for this in some way.
data UiCommand = UiCommand
  { uiCmdName  :: !CommandName
  , uiContexts :: ![AcceptedContext] -- TODO: should this be a non empty list? or should empty list imply CtxNone.
  , uiAdditionalParams :: ![RequiredParam]
  , uiFunc :: !Dispatcher
  }

instance Show UiCommand where
  show (UiCommand name ctxs params _func) = "(UiCommand " ++ show name ++ " " ++ show ctxs ++ " " ++ show params ++ ")"

type CommandName = String

data Service = Service
  { svcName :: String
  -- , svcXXX :: undefined
  } deriving (Show)

-- |Define what context will be accepted from the frontend for the specific
-- command. Matches up to corresponding values for CommandContext
data AcceptedContext = CtxNone        -- ^ No context required, global command
                     | CtxPoint       -- ^ A single (Line,Col) in a specific file
                     | CtxRegion      -- ^ A region within a specific file
                     | CtxFile        -- ^ Works on a whole file
                     | CtxCabalTarget -- ^ Works on a specific cabal target
                     deriving (Eq,Show,Generic)

type Pos = (Int,Int)

data Context = Context
                { ctxCabal    :: Maybe CabalSection
                , ctxFile     :: Maybe AbsFilePath
                , ctxStartPos :: Maybe Pos
                , ctxEndPos   :: Maybe Pos
                }
             deriving (Eq,Show,Generic)

emptyContext :: Context
emptyContext = Context Nothing Nothing Nothing Nothing


-- |It will simplify things to always work with an absolute file path
type AbsFilePath = FilePath

data CabalSection = CabalSection String deriving (Show,Eq,Generic)

-- |Initially all params will be returned as strings. This can become a much
-- richer structure in time.
data RequiredParam = RP String -- ^ Prompt
                   deriving (Show)

type PluginId = String

type Plugins = Map.Map PluginId PluginDescriptor

-- ---------------------------------------------------------------------

data IdeRequest = IdeRequest
  { ideCommand :: CommandName
  , ideContext :: Context
  , ideParams  :: Map.Map ParamId ParamVal
  } deriving Show

type ParamId = String
type ParamVal = String

-- TODO: should probably be able to return a plugin-specific type. Not sure how
-- to encode it. Perhaps as an instance of a class which says it can be encoded
-- on the wire.
data IdeResponse = IdeResponseOk    Value -- ^ Command Succeeded
                 | IdeResponseFail  Value -- ^ Command Failed
                 | IdeResponseError Value -- ^ some error in haskell-ide-engine
                                          -- driver. Equivalent to HTTP 500
                                          -- status
                 deriving Show

class (Monad m) => HasIdeState m where
  getPlugins :: m Plugins

-- Not sure if this should be completely generalised to not have GhcMonad in it
type Dispatcher = forall m. (MonadIO m,GHC.GhcMonad m,HasIdeState m) => IdeRequest -> m IdeResponse

-- instance Show Dispatcher where
--   show _ = "Dispatcher"

-- ---------------------------------------------------------------------
-- JSON instances
instance ToJSON Context where
    toJSON = genericToJSON defaultOptions

instance FromJSON Context where
    -- No need to provide a parseJSON implementation.

-- -------------------------------------

instance ToJSON CabalSection where
    toJSON = genericToJSON defaultOptions

instance FromJSON CabalSection where
    -- No need to provide a parseJSON implementation.

-- EOF
