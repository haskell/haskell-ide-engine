{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Haskell.Ide.Engine.PluginTypes
  (
    AcceptedContext(..)
  , CabalSection(..)

  -- * Parameters
  , ParamVal(..)
  , ParamValP(..)
  , ParamMap
  , pattern ParamTextP
  , pattern ParamFileP
  , pattern ParamPosP
  , ParamId
  , TaggedParamId(..)
  , ParamDescription(..)
  , pattern RP
  , pattern OP
  , ParamHelp
  , ParamName
  , ParamType(..)

  -- * Commands
  , CommandDescriptor(..)
  , UntaggedCommandDescriptor
  , ExtendedCommandDescriptor(..)
  , CommandName
  , PluginName
  , ValidResponse(..)
  , ReturnType

  -- * Interface types
  , IdeRequest(..)
  , IdeResponse(..)
  , IdeError(..)
  , IdeErrorCode(..)
  , SParamDescription(..)
  , SParamType
  , SParamRequired
  , ParamRequired(..)
  , ParamDescType(..)
  , untagParamDesc
  , SAcceptedContext
  , Sing(..)
  , Pos
  , posToJSON
  , jsonToPos

  -- * Plugins
  , PluginId
  , IdePlugins(..)
  , contextMapping
  , ContextMapping
  , fileParam
  , startPosParam
  , endPosParam
  , cabalParam
  )where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TH
import qualified Data.Text as T
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits

type PluginId = T.Text

-- | Return type of a function
type ReturnType = T.Text

$(singletons
  [d| data ParamType = PtText | PtFile | PtPos
                  deriving (Eq,Ord,Show,Read,Bounded,Enum)
      data ParamRequired = Required | Optional deriving (Eq,Ord,Show,Read,Bounded,Enum)
      -- | Define what context will be accepted from the frontend for the specific
      -- command. Matches up to corresponding values for CommandContext
      data AcceptedContext
        = CtxNone        -- ^ No context required, global command
        | CtxFile        -- ^ Works on a whole file
        | CtxPoint       -- ^ A single (Line,Col) in a specific file
        | CtxRegion      -- ^ A region within a specific file
        | CtxCabalTarget -- ^ Works on a specific cabal target
        | CtxProject     -- ^ Works on a the whole project
        deriving (Eq,Ord,Show,Read,Bounded,Enum,Generic)
  |])

data ParamDescType = ParamDescType Symbol Symbol ParamType ParamRequired

contextMapping :: AcceptedContext -> [ParamDescription]
contextMapping CtxNone        = []
contextMapping CtxFile        = [fileParam]
contextMapping CtxPoint       = [fileParam,startPosParam]
contextMapping CtxRegion      = [fileParam,startPosParam,endPosParam]
contextMapping CtxCabalTarget = [cabalParam]
contextMapping CtxProject     = [dirParam] -- the root directory of the project

-- The duplication is ugly, but it looks like atm singletons can’t promote functions that operate on strings or text
type family ContextMapping (cxt :: AcceptedContext) :: [ParamDescType] where
  ContextMapping 'CtxNone = '[]
  ContextMapping 'CtxFile = '[ 'ParamDescType "file" "a file name" 'PtFile 'Required ]
  ContextMapping 'CtxPoint = '[ 'ParamDescType "file" "a file name" 'PtFile 'Required
                              , 'ParamDescType "start pos" "start line and col" 'PtPos 'Required ]
  ContextMapping 'CtxRegion = '[ 'ParamDescType "file" "a file name" 'PtFile 'Required
                               , 'ParamDescType "start pos" "start line and col" 'PtPos 'Required
                               , 'ParamDescType "end pos" "end line and col" 'PtPos 'Required]
  ContextMapping 'CtxCabalTarget = '[ 'ParamDescType "file" "a file name" 'PtFile 'Required ]
  ContextMapping 'CtxProject = '[ 'ParamDescType "dir" "a directory name" 'PtFile 'Required ]

-- | For a given 'AcceptedContext', define the parameters that are required in
-- the corresponding 'IdeRequest'
fileParam :: ParamDescription
fileParam = ParamDesc "file" "a file name" PtFile Required

-- | A parameter for a directory
dirParam :: ParamDescription
dirParam = ParamDesc "dir" "a directory name" PtFile Required

startPosParam :: ParamDescription
startPosParam = ParamDesc "start_pos" "start line and col" PtPos Required

endPosParam :: ParamDescription
endPosParam = ParamDesc "end_pos" "end line and col" PtPos Required

cabalParam :: ParamDescription
cabalParam = ParamDesc "cabal" "cabal target" PtText Required
data SParamDescription (t :: ParamDescType) where
        SParamDesc ::
            (KnownSymbol pName, KnownSymbol pHelp) =>
            Proxy pName ->
              Proxy pHelp ->
                SParamType pType ->
                  SParamRequired pRequired ->
                    SParamDescription ('ParamDescType pName pHelp pType pRequired)

untagParamDesc :: SParamDescription t -> ParamDescription
untagParamDesc (SParamDesc pName' pHelp' pType' pRequired') =
  ParamDesc (T.pack $ symbolVal pName')
            (T.pack $ symbolVal pHelp')
            (fromSing pType')
            (fromSing pRequired')

-- | Descriptor for a command. This is intended to be transferred to the IDE, so
-- the IDE can integrate it into it's UI, and then send requests
-- through to HIE.  cxts and descs can be instantiated to simple lists
-- or vinyl 'Data.Vinyl.Rec' depending on what kind of type safety you need
data CommandDescriptor cxts descs = CommandDesc
  { cmdName :: !CommandName -- ^As returned in the 'IdeRequest'
  , cmdUiDescription :: !T.Text -- ^ Can be presented to the IDE user
  , cmdFileExtensions :: ![T.Text] -- ^ File extensions this command can be applied to
  , cmdContexts :: !cxts -- TODO: should this be a non empty list? or should empty list imply CtxNone.
  , cmdAdditionalParams :: !descs
  , cmdReturnType :: !ReturnType
  } deriving (Show,Eq,Generic)

-- | Type synonym for a 'CommandDescriptor' that uses simple lists
type UntaggedCommandDescriptor = CommandDescriptor [AcceptedContext] [ParamDescription]

data ExtendedCommandDescriptor =
  ExtendedCommandDescriptor UntaggedCommandDescriptor
                            PluginName deriving (Show,Eq)

instance ValidResponse ExtendedCommandDescriptor where
  jsWrite (ExtendedCommandDescriptor cmdDescriptor name) =
    H.fromList
      [ "name" .= cmdName cmdDescriptor
      , "ui_description" .= cmdUiDescription cmdDescriptor
      , "file_extensions" .= cmdFileExtensions cmdDescriptor
      , "contexts" .= cmdContexts cmdDescriptor
      , "additional_params" .= cmdAdditionalParams cmdDescriptor
      , "return_type" .= cmdReturnType cmdDescriptor
      , "plugin_name" .= name ]
  jsRead v =
    ExtendedCommandDescriptor
    <$> (CommandDesc
      <$> v .: "name"
      <*> v .: "ui_description"
      <*> v .: "file_extensions"
      <*> v .: "contexts"
      <*> v .: "additional_params"
      <*> v .: "return_type")
    <*> v.: "plugin_name"

type CommandName = T.Text
type PluginName = T.Text

-- | Subset type extracted from 'Plugins' to be sent to the IDE as
-- a description of the available commands
data IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId [UntaggedCommandDescriptor]
  } deriving (Show,Eq,Generic)

type Pos = (Int,Int)

-- |It will simplify things to always work with an absolute file path

-- AZ:TODO: reinstate this
-- type AbsFilePath = FilePath

data CabalSection = CabalSection T.Text deriving (Show,Eq,Generic)

-- |Initially all params will be returned as text. This can become a much
-- richer structure in time.
-- These should map down to the 'ParamVal' return types
data ParamDescription =
  ParamDesc {pName :: !ParamName
            ,pHelp :: !ParamHelp
            ,pType :: !ParamType
            ,pRequired :: !ParamRequired}
  deriving (Show,Eq,Ord,Generic)

pattern RP name help type' <- ParamDesc name help type' Required
  where RP name help type' = ParamDesc name help type' Required

pattern OP name help type' <- ParamDesc name help type' Optional
  where OP name help type' = ParamDesc name help type' Optional

type ParamHelp = T.Text
type ParamName = T.Text

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



-- | The IDE response, with the type of response it contains
data IdeResponse resp
 = IdeResponseOk resp        -- ^ Command Succeeded
 | IdeResponseFail  IdeError -- ^ Command Failed
 | IdeResponseError IdeError -- ^ Some error in haskell-ide-engine driver.
                             -- Equivalent to HTTP 500 status.
 deriving (Show,Eq,Generic)

-- | Map an IdeResponse content.
instance Functor IdeResponse where
 fmap f (IdeResponseOk a) = IdeResponseOk $ f a
 fmap _ (IdeResponseFail e) = IdeResponseFail e
 fmap _ (IdeResponseError e) = IdeResponseError e

-- | Error codes. Add as required
data IdeErrorCode
 = IncorrectParameterType  -- ^ Wrong parameter type
 | UnexpectedParameter     -- ^ A parameter was not expected by the command
 | MissingParameter        -- ^ A required parameter was not provided
 | PluginError             -- ^ An error returned by a plugin
 | InternalError           -- ^ Code error (case not handled or deemed
                           --   impossible)
 | UnknownPlugin           -- ^ Plugin is not registered
 | UnknownCommand          -- ^ Command is not registered
 | InvalidContext          -- ^ Context invalid for command
 | OtherError              -- ^ An error for which there's no better code
 | ParseError              -- ^ Input could not be parsed
 deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic)

-- | A more structured error than just a string
data IdeError = IdeError
 { ideCode    :: IdeErrorCode -- ^ The error code
 , ideMessage :: T.Text       -- ^ A human readable message
 , ideInfo    :: Maybe Value  -- ^ Additional information
 }
 deriving (Show,Read,Eq,Generic)

-- | The typeclass for valid response types
class (Typeable a) => ValidResponse a where
  jsWrite :: a -> Object -- ^ Serialize to JSON Object
  jsRead  :: Object -> Parser a -- ^ Read from JSON Object

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

instance ValidResponse UntaggedCommandDescriptor where
  jsWrite cmdDescriptor =
    H.fromList
      [ "name" .= cmdName cmdDescriptor
      , "ui_description" .= cmdUiDescription cmdDescriptor
      , "file_extensions" .= cmdFileExtensions cmdDescriptor
      , "contexts" .= cmdContexts cmdDescriptor
      , "additional_params" .= cmdAdditionalParams cmdDescriptor
      , "return_type" .= cmdReturnType cmdDescriptor ]
  jsRead v =
    CommandDesc
      <$> v .: "name"
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

-- ---------------------------------------------------------------------
-- JSON instances

posToJSON :: (Int,Int) -> Value
posToJSON (l,c) = object [ "line" .= l,"col" .= c ]

jsonToPos :: Value -> Parser (Int,Int)
jsonToPos (Object v) = (,) <$> v .: "line" <*> v.: "col"
jsonToPos _ = empty

instance (ValidResponse a) => ToJSON (IdeResponse a) where
 toJSON (IdeResponseOk v) = Object (jsWrite v)
 toJSON (IdeResponseFail v) = object [ "fail" .= v ]
 toJSON (IdeResponseError v) = object [ "error" .= v ]

instance (ValidResponse a) => FromJSON (IdeResponse a) where
 parseJSON (Object v) = do
   mf <- fmap IdeResponseFail <$> v .:? "fail"
   me <- fmap IdeResponseError <$> v .:? "error"
   let mo = IdeResponseOk <$> parseMaybe jsRead v
   case (mf <|> me <|> mo) of
     Just r -> return r
     Nothing -> empty
 parseJSON _ = empty


instance ToJSON ParamValP  where
 toJSON (ParamTextP v) = object [ "text" .= v ]
 toJSON (ParamFileP v) = object [ "file" .= v ]
 toJSON (ParamPosP  p) = posToJSON p
 toJSON _ = "error"

instance FromJSON (ParamVal 'PtText) where
  parseJSON (Object v) = ParamText <$> v .: "text"
  parseJSON _ = empty

instance FromJSON (ParamVal 'PtFile) where
  parseJSON (Object v) = ParamFile <$> v.: "file"
  parseJSON _ = empty

instance FromJSON (ParamVal 'PtPos) where
  parseJSON (Object v) = fmap ParamPos $ liftA2 (,) (v .: "line") (v .: "col")
  parseJSON _ = empty

instance FromJSON ParamValP where
 parseJSON val = do
   let mt = ParamValP <$> (parseJSON val :: Parser (ParamVal 'PtText))
       mf = ParamValP <$> (parseJSON val :: Parser (ParamVal 'PtFile))
       mp = ParamValP <$> (parseJSON val :: Parser (ParamVal 'PtPos))
   mt <|> mf <|> mp

-- -------------------------------------

instance ToJSON IdeRequest where
 toJSON (IdeRequest{ideCommand = command, ideParams = params}) =
   object [ "cmd" .= command
          , "params" .= params]

instance FromJSON IdeRequest where
 parseJSON (Object v) =
   IdeRequest <$> v .: "cmd"
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
  toJSON (ParamDesc n h t r) =
    object ["name" .= n,"help" .= h,"type" .= t,"required" .= (r == Required)]

instance FromJSON ParamDescription where
  parseJSON (Object v) = do
    req <- v .: "required"
    if req
      then ParamDesc <$> v .: "name" <*> v .: "help" <*> v .: "type" <*> pure Required
      else ParamDesc <$> v .: "name" <*> v .: "help" <*> v .: "type" <*> pure Optional
  parseJSON _ = empty

-- -------------------------------------

instance ToJSON UntaggedCommandDescriptor where
  toJSON  = Object . jsWrite

instance FromJSON UntaggedCommandDescriptor where
  parseJSON (Object v) = jsRead v
  parseJSON _ = empty
