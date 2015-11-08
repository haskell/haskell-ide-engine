{-# LANGUAGE OverloadedStrings #-}
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

import           Control.Applicative
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
  -- | Set up an underlying GHC session for the specific targets. Should map
  -- down to ghc-mod setTargets.
  setTargets :: [FilePath] -> m ()

type Dispatcher = forall m. (MonadIO m,GHC.GhcMonad m,HasIdeState m)
                => IdeRequest -> m IdeResponse

-- ---------------------------------------------------------------------
-- JSON instances

instance ToJSON Context where
    toJSON ctxt = object [ "cabal" .= toJSON (ctxCabal ctxt)
                         , "file" .= toJSON (ctxFile ctxt)
                         , "start_pos" .= toJSON (ctxStartPos ctxt)
                         , "end_pos" .= toJSON (ctxEndPos ctxt) ]

instance FromJSON Context where
    parseJSON (Object v) =
      Context <$> v .: "cabal"
              <*> v .: "file"
              <*> v .: "start_pos"
              <*> v .: "end_pos"
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON CabalSection where
    toJSON (CabalSection s) = toJSON s

instance FromJSON CabalSection where
    parseJSON (String s) = pure $ CabalSection s
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON AcceptedContext where
    toJSON CtxNone = String "none"
    toJSON CtxPoint = String "point"
    toJSON CtxRegion = String "region"
    toJSON CtxFile = String "file"
    toJSON CtxCabalTarget = String "cabal_target"
    toJSON CtxProject = String "project"

instance FromJSON AcceptedContext where
    parseJSON (String "none") = pure CtxNone
    parseJSON (String "point") = pure CtxPoint
    parseJSON (String "region") = pure CtxRegion
    parseJSON (String "file") = pure CtxFile
    parseJSON (String "cabal_target") = pure CtxCabalTarget
    parseJSON (String "project") = pure CtxProject
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON RequiredParam where
    toJSON (RP s) = toJSON s

instance FromJSON RequiredParam where
    parseJSON (String s) = pure $ RP s
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON CommandDescriptor where
    toJSON cmdDescriptor = object [ "name" .= cmdName cmdDescriptor
                                  , "ui_description" .= cmdUiDescription cmdDescriptor
                                  , "contexts" .= cmdContexts cmdDescriptor
                                  , "additional_params" .= cmdAdditionalParams cmdDescriptor ]

instance FromJSON CommandDescriptor where
    parseJSON (Object v) =
      CommandDesc <$> v .: "name"
                  <*> v .: "ui_description"
                  <*> v .: "contexts"
                  <*> v .: "additional_params"
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON IdeRequest where
  toJSON (IdeRequest{ideCommand = command,ideContext = context,ideParams = params}) =
    object [ "command" .= command
           , "context" .= context
           , "params" .= params]

instance FromJSON IdeRequest where
    parseJSON (Object v) =
      IdeRequest <$> v .: "command"
                 <*> v .: "context"
                 <*> v .: "params"
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON IdeResponse where
    toJSON (IdeResponseOk v) = object [ "tag" .= String "ok"
                                      , "contents" .= toJSON v ]
    toJSON (IdeResponseFail v) = object [ "tag" .= String "fail"
                                        , "contents" .= toJSON v ]
    toJSON (IdeResponseError v) = object [ "tag" .= String "error"
                                         , "contents" .= toJSON v ]

instance FromJSON IdeResponse where
    parseJSON (Object v) = do
      tag <- v .: "tag" :: Parser T.Text
      case tag of
        "ok" -> IdeResponseOk <$> v .: "contents"
        "fail" -> IdeResponseFail <$> v .: "contents"
        "error" -> IdeResponseError <$> v .: "contents"
        _ -> empty
    parseJSON _ = empty

-- EOF
