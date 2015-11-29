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
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Data.Maybe
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           Data.Typeable
import qualified GHC
import           GHC.Generics


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

-- | Ideally a Command is defined in such a way that its CommandDescriptor
-- can be exposed via the native CLI for the tool being exposed as well.
-- Perhaps use Options.Applicative for this in some way.
data Command = forall a .(ValidResponse a) => Command
  { cmdDesc :: !CommandDescriptor
  , cmdFunc :: !(CommandFunc a)
  }

instance Show Command where
  show (Command desc _func) = "(Command " ++ show desc ++ ")"

-- | Build a command, ensuring the command response type name and the command function match
buildCommand :: forall a .(ValidResponse a)
  => CommandFunc a
  -> CommandName
  -> T.Text
  -> [T.Text]
  -> [AcceptedContext]
  -> [ParamDescription]
  -> Command
buildCommand fun n d exts ctxs parm = Command
  (CommandDesc n d exts ctxs parm (T.pack $ show $ typeOf (undefined::a)))
  fun


-- | Return type of a function
type ReturnType = T.Text

-- | Descriptor for a command. This is intended to be transferred to the IDE, so
-- the IDE can integrate it into it's UI, and then send requests through to HIE.
data CommandDescriptor = CommandDesc
  { cmdName :: !CommandName -- ^As returned in the 'IdeRequest'
  , cmdUiDescription :: !T.Text -- ^ Can be presented to the IDE user
  , cmdFileExtensions :: ![T.Text] -- ^ File extensions this command can be applied to
  , cmdContexts :: ![AcceptedContext] -- TODO: should this be a non empty list? or should empty list imply CtxNone.
  , cmdAdditionalParams :: ![ParamDescription]
  , cmdReturnType :: !ReturnType
  } deriving (Show,Eq,Generic)

type CommandName = T.Text
type PluginName = T.Text

data ExtendedCommandDescriptor =
  ExtendedCommandDescriptor CommandDescriptor
                            PluginName
         deriving (Show, Eq)

-- | Subset type extracted from 'Plugins' to be sent to the IDE as
-- a description of the available commands
data IdePlugins = IdePlugins {
  ipMap :: Map.Map PluginId [CommandDescriptor]
  } deriving (Show,Eq,Generic)

-- | Define what context will be accepted from the frontend for the specific
-- command. Matches up to corresponding values for CommandContext
data AcceptedContext = CtxNone        -- ^ No context required, global command
                     | CtxFile        -- ^ Works on a whole file
                     | CtxPoint       -- ^ A single (Line,Col) in a specific file
                     | CtxRegion      -- ^ A region within a specific file
                     | CtxCabalTarget -- ^ Works on a specific cabal target
                     | CtxProject     -- ^ Works on a the whole project
                     deriving (Eq,Ord,Show,Read,Bounded,Enum,Generic)

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
  deriving (Show,Eq,Ord,Generic)

type ParamHelp = T.Text
type ParamName = T.Text
data ParamType = PtText | PtFile | PtPos
               deriving (Eq,Ord,Show,Read,Bounded,Enum)

data Service = Service
  { svcName :: T.Text
  -- , svcXXX :: undefined
  } deriving (Show,Eq,Ord,Generic)

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
class (Typeable a) => ValidResponse a where
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
data CommandFunc resp = CmdSync (SyncCommandFunc resp)
                      | CmdAsync (AsyncCommandFunc resp)
                        -- ^ Note: does not forkIO, the command must decide when
                        -- to do this.

type SyncCommandFunc resp = forall m. (MonadIO m,GHC.GhcMonad m,HasIdeState m)
                => [AcceptedContext] -> IdeRequest -> m (IdeResponse resp)

type AsyncCommandFunc resp = forall m. (MonadIO m,GHC.GhcMonad m,HasIdeState m)
                => (IdeResponse resp -> IO ()) -> [AcceptedContext] -> IdeRequest -> m ()

-- ---------------------------------------------------------------------
-- Specific response type

-- | Type Information, from the most precise to the most generic
data TypeInfo = TypeInfo { results :: [TypeResult] }
  deriving (Show,Read,Eq,Ord,Generic)

-- | One type result from ghc-mod
data TypeResult = TypeResult
    { trStart :: (Int,Int) -- ^ start line/column
    , trEnd   :: (Int,Int) -- ^ end line/column
    , trText  :: T.Text -- ^ type text
    } deriving (Show,Read,Eq,Ord,Generic)

-- | Result of refactoring
data RefactorResult = RefactorResult
  { rrPaths :: [FilePath]
  } deriving (Show,Read,Eq,Ord,Generic)

-- ---------------------------------------------------------------------
-- ValidResponse instances

ok :: T.Text
ok = "ok"

instance ValidResponse T.Text where
  jsWrite s = H.fromList [ok .= s]
  jsRead o = o .: ok


instance ValidResponse [T.Text] where
  jsWrite ss = H.fromList [ok .= ss]
  jsRead o = o .: ok

instance ValidResponse () where
  jsWrite _ = H.fromList [ok .= String ok]
  jsRead o = do
      r <- o .: ok
      if r == String ok
        then pure ()
        else empty

instance ValidResponse Object where
  jsWrite = id
  jsRead = pure

instance ValidResponse ExtendedCommandDescriptor where
  jsWrite (ExtendedCommandDescriptor cmdDescriptor name) =
    H.fromList
      ["name" .= cmdName cmdDescriptor
      ,"ui_description" .= cmdUiDescription cmdDescriptor
      ,"file_extensions" .= cmdFileExtensions cmdDescriptor
      ,"contexts" .= cmdContexts cmdDescriptor
      ,"additional_params" .= cmdAdditionalParams cmdDescriptor
      ,"return_type" .= cmdReturnType cmdDescriptor
      ,"plugin_name" .= name]

  jsRead v =
    ExtendedCommandDescriptor <$>
    (CommandDesc <$> v .: "name" <*> v .: "ui_description" <*>
     v .: "file_extensions" <*>
     v .: "contexts" <*>
     v .: "additional_params" <*>
     v .: "return_type") <*>
     v .: "plugin_name"

instance ValidResponse CommandDescriptor where
  jsWrite cmdDescriptor = H.fromList [ "name" .= cmdName cmdDescriptor
                                  , "ui_description" .= cmdUiDescription cmdDescriptor
                                  , "file_extensions" .= cmdFileExtensions cmdDescriptor
                                  , "contexts" .= cmdContexts cmdDescriptor
                                  , "additional_params" .= cmdAdditionalParams cmdDescriptor
                                  , "return_type" .= cmdReturnType cmdDescriptor ]
  jsRead v =
        CommandDesc <$> v .: "name"
                    <*> v .: "ui_description"
                    <*> v .: "file_extensions"
                    <*> v .: "contexts"
                    <*> v .: "additional_params"
                    <*> v .: "return_type"

instance ValidResponse IdePlugins where
  jsWrite (IdePlugins m) = H.fromList ["plugins" .= H.fromList
                ( map (uncurry (.=))
                $ Map.assocs m)]
  jsRead v = do
    ps <- v .: "plugins"
    liftM (IdePlugins . Map.fromList) $ mapM (\(k,vp) -> do
            p<-parseJSON vp
            return (k,p)) $ H.toList ps

instance ValidResponse TypeInfo where
  jsWrite (TypeInfo t) = H.fromList ["type_info" .= t]
  jsRead v = TypeInfo <$> v .: "type_info"

instance ValidResponse RefactorResult where
  jsWrite (RefactorResult t) = H.fromList ["refactor" .= t]
  jsRead v = RefactorResult <$> v .: "refactor"

-- ---------------------------------------------------------------------
-- JSON instances

posToJSON :: (Int,Int) -> Value
posToJSON (l,c) = object [ "line" .= l,"col" .= c ]

jsonToPos :: Value -> Parser (Int,Int)
jsonToPos (Object v) = (,) <$> v .: "line" <*> v.: "col"
jsonToPos _ = empty

instance ToJSON ParamValP  where
    toJSON (ParamTextP v) = object [ "text" .= v ]
    toJSON (ParamFileP v) = object [ "file" .= v ]
    toJSON (ParamPosP  p) = posToJSON p
    toJSON _ = "error"

instance FromJSON ParamValP where
    parseJSON (Object v) = do
      mt <- fmap ParamTextP <$> v .:? "text"
      mf <- fmap ParamFileP <$> v .:? "file"
      mp <- toParamPos <$> v .:? "line" <*> v .:? "col"
      case mt <|> mf <|> mp of
        Just pd -> return pd
        _ -> empty
      where
          toParamPos (Just l) (Just c) = Just $ ParamPosP (l,c)
          toParamPos _ _ = Nothing
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
    toJSON (RP n h t) = object [ "name" .= n ,"help" .= h, "type" .= t, "required" .= True ]
    toJSON (OP n h t) = object [ "name" .= n ,"help" .= h, "type" .= t, "required" .= False ]

instance FromJSON ParamDescription where
    parseJSON (Object v) = do
      req <- v .: "required"
      if req
        then RP <$> v .: "name" <*> v .: "help" <*> v .: "type"
        else OP <$> v .: "name" <*> v .: "help" <*> v .: "type"
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
    toJSON err = object [ "code" .= ideCode err
                        , "msg"  .= ideMessage err
                        , "info" .= ideInfo err]

instance FromJSON IdeError where
    parseJSON (Object v) = IdeError
        <$> v .:  "code"
        <*> v .:  "msg"
        <*> v .:? "info"
    parseJSON _ = empty

instance ToJSON TypeResult where
  toJSON (TypeResult s e t) =
      object [ "start" .= posToJSON s
             , "end" .= posToJSON e
             , "type" .= t
             ]

instance FromJSON TypeResult where
  parseJSON (Object v) = TypeResult
    <$> (jsonToPos =<< (v .: "start"))
    <*> (jsonToPos =<< (v .: "end"))
    <*> v .: "type"
  parseJSON _ = empty

-- -------------------------------------

instance (ValidResponse a) => ToJSON (IdeResponse a) where
    toJSON (IdeResponseOk v) = Object (jsWrite v)
    toJSON (IdeResponseFail v) = object [ "fail" .= v ]
    toJSON (IdeResponseError v) = object [ "error" .= v ]

instance (ValidResponse a) => FromJSON (IdeResponse a) where
    parseJSON (Object v) = do
      mf <- fmap IdeResponseFail <$> v .:? "fail"
      me <- fmap IdeResponseError <$> v .:? "error"
      let mo = IdeResponseOk <$> parseMaybe jsRead v
      return $ fromJust $ mf <|> me <|> mo
    parseJSON _ = empty

-- EOF
