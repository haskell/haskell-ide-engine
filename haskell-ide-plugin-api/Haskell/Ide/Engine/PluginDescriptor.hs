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
import qualified Data.Text as T
import qualified GHC
import           GHC.Generics

-- ---------------------------------------------------------------------

data PluginDescriptor = PluginDescriptor
  { pdCommands        :: [Command]
  , pdExposedServices :: [Service]
  , pdUsedServices    :: [Service]
  } deriving (Show)


-- |Ideally a Command is defined in such a way that it can be exposed via the
-- native CLI for the tool being exposed as well. Perhaps use
-- Options.Applicative for this in some way.
data Command = Command
  { cmdDesc :: !CommandDescriptor
  , cmdFunc :: !Dispatcher
  }

instance Show Command where
  show (Command desc _func) = "(Command " ++ show desc ++ ")"

-- |Descriptor for a command. This is intended to be transferred to the IDE, so
-- the IDE can integrate it into it's UI, and then send requests through to HIE.
data CommandDescriptor = CommandDesc
  { cmdName :: !CommandName -- ^As returned in the 'IdeRequest'
  , cmdUiDescription :: !T.Text -- ^ Can be presented to the IDE user
  , cmdContexts :: ![AcceptedContext] -- TODO: should this be a non empty list? or should empty list imply CtxNone.
  , cmdAdditionalParams :: ![RequiredParam]
  } deriving (Show,Generic)

type CommandName = T.Text

-- |Define what context will be accepted from the frontend for the specific
-- command. Matches up to corresponding values for CommandContext
data AcceptedContext = CtxNone        -- ^ No context required, global command
                     | CtxPoint       -- ^ A single (Line,Col) in a specific file
                     | CtxRegion      -- ^ A region within a specific file
                     | CtxFile        -- ^ Works on a whole file
                     | CtxCabalTarget -- ^ Works on a specific cabal target
                     | CtxProject     -- ^ Works on a the whole project
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

data CabalSection = CabalSection T.Text deriving (Show,Eq,Generic)

-- |Initially all params will be returned as text. This can become a much
-- richer structure in time.
data RequiredParam = RP T.Text -- ^ Prompt
                   deriving (Show,Generic)

data Service = Service
  { svcName :: T.Text
  -- , svcXXX :: undefined
  } deriving (Show)

type PluginId = T.Text

type Plugins = Map.Map PluginId PluginDescriptor

-- ---------------------------------------------------------------------

data IdeRequest = IdeRequest
  { ideCommand :: CommandName
  , ideContext :: Context
  , ideParams  :: Map.Map ParamId ParamVal
  } deriving (Show,Generic)

type ParamId = T.Text
type ParamVal = T.Text

-- TODO: should probably be able to return a plugin-specific type. Not sure how
-- to encode it. Perhaps as an instance of a class which says it can be encoded
-- on the wire.
data IdeResponse = IdeResponseOk    Value -- ^ Command Succeeded
                 | IdeResponseFail  Value -- ^ Command Failed
                 | IdeResponseError Value -- ^ some error in haskell-ide-engine
                                          -- driver. Equivalent to HTTP 500
                                          -- status
                 deriving (Show,Generic)

class (Monad m) => HasIdeState m where
  getPlugins :: m Plugins

type Dispatcher = forall m. (MonadIO m,GHC.GhcMonad m,HasIdeState m)
                => IdeRequest -> m IdeResponse

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

-- -------------------------------------

instance ToJSON AcceptedContext where
    toJSON = genericToJSON defaultOptions

instance FromJSON AcceptedContext where
    -- No need to provide a parseJSON implementation.

-- -------------------------------------

instance ToJSON RequiredParam where
    toJSON = genericToJSON defaultOptions

instance FromJSON RequiredParam where
    -- No need to provide a parseJSON implementation.

-- -------------------------------------

instance ToJSON CommandDescriptor where
    toJSON = genericToJSON defaultOptions

instance FromJSON CommandDescriptor where
    -- No need to provide a parseJSON implementation.

-- -------------------------------------

instance ToJSON IdeRequest where
    toJSON = genericToJSON defaultOptions

instance FromJSON IdeRequest where
    -- No need to provide a parseJSON implementation.

-- -------------------------------------

instance ToJSON IdeResponse where
    toJSON = genericToJSON defaultOptions

instance FromJSON IdeResponse where
    -- No need to provide a parseJSON implementation.

-- EOF
