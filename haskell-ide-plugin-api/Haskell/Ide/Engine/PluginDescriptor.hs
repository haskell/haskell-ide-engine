{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
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

module Haskell.Ide.Engine.PluginDescriptor where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified GHC
import           GHC.Generics

-- ---------------------------------------------------------------------

data PluginDescriptor = PluginDescriptor
  { pdCommands        :: [Command]
  , pdExposedServices :: [Service]
  , pdUsedServices    :: [Service]
  }

instance Show PluginDescriptor where
  showsPrec p (PluginDescriptor cmds svcs used) = showParen (p > 10) $
      showString "PluginDescriptor " .
      showList cmds .
      showString " " .
      showList svcs .
      showString " " .
      showList used

-- | Ideally a Command is defined in such a way that it can be exposed via the
-- native CLI for the tool being exposed as well. Perhaps use
-- Options.Applicative for this in some way.
data Command = forall a .(ValidResponse a) => Command
  { cmdDesc :: !CommandDescriptor
  , cmdFunc :: !(CommandFunc a)
  }

instance Show Command where
  show (Command desc _func) = "(Command " ++ show desc ++ ")"

-- | Descriptor for a command. This is intended to be transferred to the IDE, so
-- the IDE can integrate it into it's UI, and then send requests through to HIE.
data CommandDescriptor = CommandDesc
  { cmdName :: !CommandName -- ^As returned in the 'IdeRequest'
  , cmdUiDescription :: !T.Text -- ^ Can be presented to the IDE user
  , cmdFileExtensions :: ![T.Text] -- ^ File extensions this command can be applied to
  , cmdContexts :: ![AcceptedContext] -- TODO: should this be a non empty list? or should empty list imply CtxNone.
  , cmdAdditionalParams :: ![ParamDescription]
  } deriving (Show,Eq,Generic)

type CommandName = T.Text

-- |Define what context will be accepted from the frontend for the specific
-- command. Matches up to corresponding values for CommandContext
data AcceptedContext = CtxNone        -- ^ No context required, global command
                     | CtxFile        -- ^ Works on a whole file
                     | CtxPoint       -- ^ A single (Line,Col) in a specific file
                     | CtxRegion      -- ^ A region within a specific file
                     | CtxCabalTarget -- ^ Works on a specific cabal target
                     | CtxProject     -- ^ Works on a the whole project
                     deriving (Eq,Show,Generic)

type Pos = (Int,Int)

-- |It will simplify things to always work with an absolute file path
type AbsFilePath = FilePath

data CabalSection = CabalSection T.Text deriving (Show,Eq,Generic)

-- |Initially all params will be returned as text. This can become a much
-- richer structure in time.
-- These should map down to the 'ParamVal' return types
data ParamDescription
  = RP
      { pName :: !ParamName
      , pHelp :: !ParamHelp
      , pType :: !ParamType
      } -- ^ Required parameter
  | OP
      { pName :: !ParamName
      , pHelp :: !ParamHelp
      , pType :: !ParamType
      } -- ^ Optional parameter
  deriving (Show,Eq,Generic)

type ParamHelp = T.Text
type ParamName = T.Text
data ParamType = PtText | PtFile | PtPos
               deriving (Eq,Show)

data Service = Service
  { svcName :: T.Text
  -- , svcXXX :: undefined
  } deriving (Show)

type PluginId = T.Text

type Plugins = Map.Map PluginId PluginDescriptor

-- ---------------------------------------------------------------------

-- |For a given 'AcceptedContext', define the parameters that are required in
-- the corresponding 'IdeRequest'
contextMapping :: AcceptedContext -> [ParamDescription]
contextMapping CtxNone        = []
contextMapping CtxFile        = [fileParam]
contextMapping CtxPoint       = [fileParam,startPosParam]
contextMapping CtxRegion      = [fileParam,startPosParam,endPosParam]
contextMapping CtxCabalTarget = [cabalParam]
contextMapping CtxProject     = []

fileParam :: ParamDescription
fileParam = RP "file" "a file name" PtFile

startPosParam :: ParamDescription
startPosParam = RP "start_pos" "start line and col" PtPos

endPosParam :: ParamDescription
endPosParam = RP "end_pos" "end line and col" PtPos

cabalParam :: ParamDescription
cabalParam = RP "cabal" "cabal target" PtText

-- ---------------------------------------------------------------------

-- |A request from the IDE to HIE. When a context is specified, the following
-- parameter names must be used.
--
--  cabal     - for the cabal target
--  file      - for the file name
--  start_pos - for the first position
--  end_pos   - for the second position
--
-- These will be checked by the dispatcher.
data IdeRequest = IdeRequest
  { ideCommand :: !CommandName
  , ideParams  :: !ParamMap
  } deriving (Show)

deriving instance Show (TaggedParamId t)
deriving instance Show (ParamVal t)
deriving instance Show ParamValP

deriving instance Eq (ParamVal t)
instance Eq ParamValP where
  (ParamTextP t) == (ParamTextP t') = t == t'
  (ParamFileP f) == (ParamFileP f') = f == f'
  (ParamPosP p) == (ParamPosP p') = p == p'
  _ == _ = False

pattern ParamTextP t = ParamValP (ParamText t)
pattern ParamFileP f = ParamValP (ParamFile f)
pattern ParamPosP p = ParamValP (ParamPos p)

type ParamMap = Map.Map ParamId ParamValP

type ParamId = T.Text

data TaggedParamId (t :: ParamType) where
  IdText :: T.Text -> TaggedParamId 'PtText
  IdFile :: T.Text -> TaggedParamId 'PtFile
  IdPos :: T.Text -> TaggedParamId 'PtPos

data ParamValP = forall t. ParamValP { unParamValP ::  ParamVal t }

data ParamVal (t :: ParamType) where
  ParamText :: T.Text -> ParamVal 'PtText
  ParamFile :: T.Text -> ParamVal 'PtFile
  ParamPos :: (Int,Int) -> ParamVal 'PtPos

-- | The typeclass for valid response types
class ValidResponse a where
  jsWrite :: a -> Object -- ^ Serialize to JSON Object
  jsRead  :: Object -> Parser a -- ^ Read from JSON Object


-- | The IDE response, with the type of response it contains
data IdeResponse resp = IdeResponseOk resp    -- ^ Command Succeeded
                      | IdeResponseFail  IdeError -- ^ Command Failed
                      | IdeResponseError IdeError -- ^ Some error in
                                               -- haskell-ide-engine
                                               -- driver. Equivalent to HTTP 500
                                               -- status
                 deriving (Show,Eq,Generic)

-- | Map an IdeResponse content.
instance Functor IdeResponse where
  fmap f (IdeResponseOk a) = IdeResponseOk $ f a
  fmap _ (IdeResponseFail e) = IdeResponseFail e
  fmap _ (IdeResponseError e) = IdeResponseError e

-- | Error codes. Add as required
data IdeErrorCode = IncorrectParameterType  -- ^ Wrong parameter type
                  | UnexpectedParameter     -- ^ A parameter was not expected by
                                            -- the command
                  | MissingParameter        -- ^ A required parameter was not
                                            --  provided
                  | PluginError             -- ^ An error returned by a plugin
                  | InternalError           -- ^ Code error
                                            -- (case not handled or deemed
                                            -- impossible)
                  | UnknownPlugin           -- ^ Plugin is not registered
                  | UnknownCommand          -- ^ Command is not registered
                  | InvalidContext          -- ^ Context invalid for command
                  | OtherError              -- ^ An error for which we didn't
                                            -- have a better code
                  | ParseError              -- ^ Input could not be parsed
                  deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic)

-- | A more structured error than just a string
data IdeError = IdeError
  { ideCode    :: IdeErrorCode -- ^ The error code
  , ideMessage :: T.Text       -- ^ A human readable message
  , ideInfo    :: Maybe Value  -- ^ Additional information
  }
  deriving (Show,Read,Eq,Generic)


class (Monad m) => HasIdeState m where
  getPlugins :: m Plugins
  -- | Set up an underlying GHC session for the specific targets. Should map
  -- down to ghc-mod setTargets.
  setTargets :: [FilePath] -> m ()

-- | The 'CommandFunc' is called once the dispatcher has checked that it
-- satisfies at least one of the `AcceptedContext` values for the command
-- descriptor, and has all the required parameters. Where a command has only one
-- allowed context the supplied context list does not add much value, but allows
-- easy case checking when multiple contexts are supported.
type CommandFunc resp = forall m. (MonadIO m,GHC.GhcMonad m,HasIdeState m)
                => [AcceptedContext] -> IdeRequest -> m (IdeResponse resp)

-- ---------------------------------------------------------------------
-- ValidResponse instances

instance ValidResponse String where
  jsWrite s = H.fromList ["response" .= toJSON s]
  jsRead o = o .: "response"

instance ValidResponse T.Text where
  jsWrite s = H.fromList ["response" .= toJSON s]
  jsRead o = o .: "response"

instance ValidResponse [String] where
  jsWrite ss = H.fromList ["responses" .= toJSON ss]
  jsRead o = o .: "responses"

instance ValidResponse [T.Text] where
  jsWrite ss = H.fromList ["responses" .= toJSON ss]
  jsRead o = o .: "responses"

instance ValidResponse () where
  jsWrite _ = H.fromList ["response" .= String "ok"]
  jsRead o = do
      r <- o .: "response"
      if r == String "ok"
        then pure ()
        else empty

instance ValidResponse Object where
  jsWrite = id
  jsRead = pure

instance ValidResponse CommandDescriptor where
  jsWrite cmdDescriptor = H.fromList [ "name" .= cmdName cmdDescriptor
                                  , "ui_description" .= cmdUiDescription cmdDescriptor
                                  , "file_extensions" .= cmdFileExtensions cmdDescriptor
                                  , "contexts" .= cmdContexts cmdDescriptor
                                  , "additional_params" .= cmdAdditionalParams cmdDescriptor ]
  jsRead v =
        CommandDesc <$> v .: "name"
                    <*> v .: "ui_description"
                    <*> v .: "file_extensions"
                    <*> v .: "contexts"
                    <*> v .: "additional_params"

instance ValidResponse Plugins where
  jsWrite = H.fromList . map (\(k,v)-> k .= toJSON v) . Map.assocs

  jsRead = liftM Map.fromList . mapM (\(k,v) -> do
            p<-parseJSON v
            return (k,p)) . H.toList

-- ---------------------------------------------------------------------
-- JSON instances

instance ToJSON ParamValP  where
    toJSON (ParamTextP v) = object [ "tag" .= String "text"
                                  , "contents" .= toJSON v ]
    toJSON (ParamFileP v) = object [ "tag" .= String "file"
                                  , "contents" .= toJSON v ]
    toJSON (ParamPosP  v) = object [ "tag" .= String "pos"
                                  , "contents" .= toJSON v ]
    toJSON _ = "error"

instance FromJSON ParamValP where
    parseJSON (Object v) = do
      tag <- v .: "tag" :: Parser T.Text
      case tag of
        "text" -> ParamTextP <$> v .: "contents"
        "file" -> ParamFileP <$> v .: "contents"
        "pos"  -> ParamPosP <$> v .: "contents"
        _ -> empty
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

instance ToJSON ParamType where
  toJSON PtText = String "text"
  toJSON PtFile = String "file"
  toJSON PtPos  = String "pos"

instance FromJSON ParamType where
  parseJSON (String "text") = pure PtText
  parseJSON (String "file") = pure PtFile
  parseJSON (String "pos")  = pure PtPos
  parseJSON _               = empty

-- -------------------------------------

instance ToJSON ParamDescription where
    toJSON (RP n h t) = object [ "tag" .= String "rp"
                               , "contents" .= toJSON (n,h,t) ]
    toJSON (OP n h t) = object [ "tag" .= String "op"
                               , "contents" .= toJSON (n,h,t) ]

instance FromJSON ParamDescription where
    parseJSON (Object v) = do
      tag <- v .: "tag" :: Parser T.Text
      (n,h,t) <- v .: "contents"
      case tag of
        "rp" -> return $ RP n h t
        "op" -> return $ OP n h t
        _ -> empty
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON CommandDescriptor where
    toJSON  = Object . jsWrite

instance FromJSON CommandDescriptor where
    parseJSON (Object v) = jsRead v
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON Service where
    toJSON service = object [ "name" .= svcName service ]


instance FromJSON Service where
    parseJSON (Object v) =
      Service <$> v .: "name"
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON PluginDescriptor where
    toJSON pluginDescriptor = object [ "commands" .= map cmdDesc (pdCommands pluginDescriptor)
                                     , "exposed_services" .= pdExposedServices pluginDescriptor
                                     , "used_services" .= pdUsedServices pluginDescriptor
                                     ]

instance FromJSON PluginDescriptor where
    parseJSON (Object v) =
      PluginDescriptor <$> (fmap (fmap (\desc -> Command desc (error "missing"::CommandFunc T.Text))) (v .: "commands"))
                       <*> v .: "exposed_services"
                       <*> v .: "used_services"
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON IdeRequest where
  toJSON (IdeRequest{ideCommand = command, ideParams = params}) =
    object [ "command" .= command
           , "params" .= params]

instance FromJSON IdeRequest where
    parseJSON (Object v) =
      IdeRequest <$> v .: "command"
                 <*> v .: "params"
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON IdeErrorCode where
    toJSON code = String $ T.pack $ show code

instance FromJSON IdeErrorCode where
    parseJSON (String s) = case reads (T.unpack s) of
      ((c,""):_) -> pure c
      _          -> empty
    parseJSON _ = empty

-- -------------------------------------

instance ToJSON IdeError where
    toJSON err = object [ "code" .= toJSON (ideCode err)
                        , "msg"  .= String (ideMessage err)
                        , "info" .= toJSON (ideInfo err)]

instance FromJSON IdeError where
    parseJSON (Object v) = IdeError
        <$> v .: "code"
        <*> v .: "msg"
        <*> v .: "info"
    parseJSON _ = empty

-- -------------------------------------

instance (ValidResponse a) => ToJSON (IdeResponse a) where
    toJSON (IdeResponseOk v) = object [ "tag" .= String "ok"
                                      , "contents" .= toJSON (jsWrite v) ]
    toJSON (IdeResponseFail v) = object [ "tag" .= String "fail"
                                        , "contents" .= toJSON v ]
    toJSON (IdeResponseError v) = object [ "tag" .= String "error"
                                         , "contents" .= toJSON v ]

instance (ValidResponse a) => FromJSON (IdeResponse a) where
    parseJSON (Object v) = do
      tag <- v .: "tag" :: Parser T.Text
      case tag of
        "ok" -> do
          cnts <- v .: "contents"
          case cnts of
            (Object v2) -> IdeResponseOk <$> jsRead v2
            _ -> empty
        "fail" -> IdeResponseFail <$> v .: "contents"
        "error" -> IdeResponseError <$> v .: "contents"
        _ -> empty
    parseJSON _ = empty

-- EOF
