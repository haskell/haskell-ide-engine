{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Haskell.Ide.Engine.PluginTypes
  ( CabalSection(..)

  -- * Parameters
  , ParamVal(..)
  , ParamValP(..)
  , ParamMap
  , pattern ParamTextP
  , pattern ParamIntP
  , pattern ParamBoolP
  , pattern ParamFileP
  , pattern ParamPosP
  , pattern ParamRangeP
  , pattern ParamLocP
  , pattern ParamTextDocIdP
  , pattern ParamTextDocPosP
  , ParamId
  , TaggedParamId(..)
  , ParamDescription(..)
  , pattern RP
  , pattern OP
  , ParamHelp
  , ParamName

  -- * Commands
  , CommandDescriptor(..)
  , UntaggedCommandDescriptor
  , TaggedCommandDescriptor
  , ExtendedCommandDescriptor(..)
  , CommandName
  , PluginName
  , ValidResponse(..)
  , ReturnType
  , Save(..)

  -- * Interface types
  , IdeRequest(..)
  , IdeResponse(..)
  , IdeError(..)
  , IdeErrorCode(..)
  , untagParamDesc
  , Sing(..)
  , Uri(..)
  , uriToFilePath
  , filePathToUri
  , Position(..)
  , toPos, unPos
  , Range(..)
  , Location(..)
  , TextDocumentIdentifier(..)
  , TextDocumentPositionParams(..)
  -- * Plugins
  , PluginId
  , IdePlugins(..)
  , contextMapping
  , ContextMapping
  , fileParam
  , startPosParam
  , endPosParam
  , cabalParam
  , module Haskell.Ide.Engine.PluginTypes.Singletons
  )where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import           Data.Singletons.Prelude
import qualified Data.Text as T
import           Data.Typeable
import           Data.Vinyl
import           GHC.Generics
import           GHC.TypeLits
import           Haskell.Ide.Engine.PluginTypes.Singletons
import           Language.Haskell.LSP.TH.DataTypesJSON ( Uri(..), uriToFilePath, filePathToUri, Position(..), Range(..), Location(..), TextDocumentIdentifier(..), TextDocumentPositionParams(..))


type PluginId = T.Text

-- | Return type of a function
type ReturnType = T.Text

contextMapping :: AcceptedContext -> [ParamDescription]
contextMapping CtxNone        = []
contextMapping CtxFile        = [fileParam]
contextMapping CtxPoint       = [fileParam,startPosParam]
contextMapping CtxRegion      = [fileParam,startPosParam,endPosParam]
contextMapping CtxCabalTarget = [cabalParam]
contextMapping CtxProject     = [dirParam] -- the root directory of the project

-- The duplication is ugly, but it looks like atm singletons can’t promote functions that operate on strings or text
type family ContextMapping (cxt :: AcceptedContext) :: [ParamDescType] where
  ContextMapping 'CtxNone   = '[]
  ContextMapping 'CtxFile   = '[ 'ParamDescType "file" "a file name" 'PtFile 'Required ]
  ContextMapping 'CtxPoint  = '[ 'ParamDescType "file" "a file name" 'PtFile 'Required
                               , 'ParamDescType "start_pos" "start line and col" 'PtPos 'Required ]
  ContextMapping 'CtxRegion = '[ 'ParamDescType "file" "a file name" 'PtFile 'Required
                               , 'ParamDescType "start_pos" "start line and col" 'PtPos 'Required
                               , 'ParamDescType "end_pos" "end line and col" 'PtPos 'Required]
  ContextMapping 'CtxCabalTarget = '[ 'ParamDescType "file" "a file name" 'PtFile 'Required ]
  ContextMapping 'CtxProject     = '[ 'ParamDescType "dir" "a directory name" 'PtFile 'Required ]

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
  { cmdName             :: !CommandName -- ^As returned in the 'IdeRequest'
  , cmdUiDescription    :: !T.Text -- ^ Can be presented to the IDE user
  , cmdFileExtensions   :: ![T.Text] -- ^ File extensions this command can be applied to
  , cmdContexts         :: !cxts -- TODO: should this be a non empty list? or should empty list imply CtxNone.
  , cmdAdditionalParams :: !descs
  , cmdReturnType       :: !ReturnType
  , cmdSave             :: !Save
  } deriving (Show,Eq,Generic)

data Save = SaveNone | SaveAll deriving (Show, Eq, Generic)

-- | Type synonym for a 'CommandDescriptor' that uses simple lists
type UntaggedCommandDescriptor = CommandDescriptor [AcceptedContext] [ParamDescription]

type TaggedCommandDescriptor cxts tags = CommandDescriptor (Rec SAcceptedContext cxts) (Rec SParamDescription tags)

data ExtendedCommandDescriptor =
  ExtendedCommandDescriptor UntaggedCommandDescriptor
                            PluginName deriving (Show,Eq,Generic)


instance ValidResponse ExtendedCommandDescriptor where
  jsWrite (ExtendedCommandDescriptor cmdDescriptor pname) =
    H.fromList
      [ "name"              .= cmdName cmdDescriptor
      , "ui_description"    .= cmdUiDescription cmdDescriptor
      , "file_extensions"   .= cmdFileExtensions cmdDescriptor
      , "contexts"          .= cmdContexts cmdDescriptor
      , "additional_params" .= cmdAdditionalParams cmdDescriptor
      , "return_type"       .= cmdReturnType cmdDescriptor
      , "plugin_name"       .= pname
      , "save"              .= cmdSave cmdDescriptor ]
  jsRead v =
    ExtendedCommandDescriptor
    <$> (CommandDesc
      <$> v .: "name"
      <*> v .: "ui_description"
      <*> v .: "file_extensions"
      <*> v .: "contexts"
      <*> v .: "additional_params"
      <*> v .: "return_type"
      <*> v .: "save")
    <*> v.: "plugin_name"

type CommandName = T.Text
type PluginName = T.Text

-- | Subset type extracted from 'Plugins' to be sent to the IDE as
-- a description of the available commands
data IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId [UntaggedCommandDescriptor]
  } deriving (Show,Eq,Generic)

-- ---------------------------------------------------------------------

-- Converts to one based tuple
unPos :: Position -> (Int,Int)
unPos (Position l c) = (l+1,c+1)

-- Converts from one based tuple
toPos :: (Int,Int) -> Position
toPos (l,c) = Position (l-1) (c-1)

-- ---------------------------------------------------------------------

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

pattern RP :: ParamName -> ParamHelp -> ParamType -> ParamDescription
pattern RP pname help type' <- ParamDesc pname help type' Required
  where RP pname help type' = ParamDesc pname help type' Required

pattern OP :: ParamName -> ParamHelp -> ParamType -> ParamDescription
pattern OP pname help type' <- ParamDesc pname help type' Optional
  where OP pname help type' = ParamDesc pname help type' Optional

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
 (ParamPosP  p) == (ParamPosP p') = p == p'
 _ == _ = False

pattern ParamTextP :: T.Text -> ParamValP
pattern ParamTextP x = ParamValP (ParamText x)
pattern ParamIntP :: Int -> ParamValP
pattern ParamIntP x = ParamValP (ParamInt x)
pattern ParamBoolP :: Bool -> ParamValP
pattern ParamBoolP x = ParamValP (ParamBool x)
pattern ParamFileP :: Uri -> ParamValP
pattern ParamFileP x = ParamValP (ParamFile x)
pattern ParamPosP :: Position -> ParamValP
pattern ParamPosP x = ParamValP (ParamPos x)
pattern ParamRangeP :: Range -> ParamValP
pattern ParamRangeP x = ParamValP (ParamRange x)
pattern ParamLocP :: Location -> ParamValP
pattern ParamLocP x = ParamValP (ParamLoc x)
pattern ParamTextDocIdP :: TextDocumentIdentifier -> ParamValP
pattern ParamTextDocIdP x = ParamValP (ParamTextDocId x)
pattern ParamTextDocPosP :: TextDocumentPositionParams -> ParamValP
pattern ParamTextDocPosP x = ParamValP (ParamTextDocPos x)

type ParamMap = Map.Map ParamId ParamValP

type ParamId = T.Text

data TaggedParamId (t :: ParamType) where
 IdText :: T.Text -> TaggedParamId 'PtText
 IdInt  :: T.Text  -> TaggedParamId 'PtInt
 IdBool :: T.Text -> TaggedParamId 'PtBool
 IdFile :: T.Text -> TaggedParamId 'PtFile
 IdPos  :: T.Text -> TaggedParamId 'PtPos
 IdRange :: T.Text -> TaggedParamId 'PtRange
 IdLoc  :: T.Text -> TaggedParamId 'PtLoc
 IdTextDocId :: T.Text -> TaggedParamId 'PtTextDocId
 IdTextDocPos ::T.Text -> TaggedParamId 'PtTextDocPos

data ParamValP = forall t. ParamValP { unParamValP ::  ParamVal t }

data ParamVal (t :: ParamType) where
 ParamText :: T.Text -> ParamVal 'PtText
 ParamInt  :: Int  -> ParamVal 'PtInt
 ParamBool :: Bool -> ParamVal 'PtBool
 ParamFile :: Uri -> ParamVal 'PtFile
 ParamPos  :: Position -> ParamVal 'PtPos
 ParamRange :: Range -> ParamVal 'PtRange
 ParamLoc  :: Location -> ParamVal 'PtLoc
 ParamTextDocId :: TextDocumentIdentifier -> ParamVal 'PtTextDocId
 ParamTextDocPos ::TextDocumentPositionParams -> ParamVal 'PtTextDocPos

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
 , ideInfo    :: Value  -- ^ Additional information
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
deriving instance Generic Value

instance ValidResponse UntaggedCommandDescriptor where
  jsWrite cmdDescriptor =
    H.fromList
      [ "name" .= cmdName cmdDescriptor
      , "ui_description" .= cmdUiDescription cmdDescriptor
      , "file_extensions" .= cmdFileExtensions cmdDescriptor
      , "contexts" .= cmdContexts cmdDescriptor
      , "additional_params" .= cmdAdditionalParams cmdDescriptor
      , "return_type" .= cmdReturnType cmdDescriptor
      , "save" .= cmdSave cmdDescriptor ]
  jsRead v =
    CommandDesc
      <$> v .: "name"
      <*> v .: "ui_description"
      <*> v .: "file_extensions"
      <*> v .: "contexts"
      <*> v .: "additional_params"
      <*> v .: "return_type"
      <*> v .: "save"

instance ValidResponse IdePlugins where
  jsWrite (IdePlugins m) = H.fromList ["plugins" .= H.fromList
                ( map (uncurry (.=))
                $ Map.assocs m :: [Pair])]
  jsRead v = do
    ps <- v .: "plugins"
    liftM (IdePlugins . Map.fromList) $ mapM (\(k,vp) -> do
            p<-parseJSON vp
            return (k,p)) $ H.toList ps

-- ---------------------------------------------------------------------
-- JSON instances

instance (ValidResponse a) => ToJSON (IdeResponse a) where
 toJSON (IdeResponseOk v) = Object (jsWrite v)
 toJSON (IdeResponseFail v) = object [ "fail" .= v ]
 toJSON (IdeResponseError v) = object [ "error" .= v ]

instance (ValidResponse a) => FromJSON (IdeResponse a) where
 parseJSON = withObject "IdeResponse" $ \v -> do
   mf <- fmap IdeResponseFail <$> v .:? "fail"
   me <- fmap IdeResponseError <$> v .:? "error"
   let mo = IdeResponseOk <$> parseMaybe jsRead v
   case (mf <|> me <|> mo) of
     Just r -> return r
     Nothing -> empty

-- ---------------------------------------------------------------------

instance FromJSON (ParamVal 'PtText) where
  parseJSON = withObject "text parameter object" $ \v ->
    ParamText <$> v .: "text"

instance FromJSON (ParamVal 'PtFile) where
  parseJSON = withObject "file parameter object" $ \v -> ParamFile <$> v.: "file"

instance FromJSON (ParamVal 'PtPos) where
    parseJSON = withObject "pos parameter object" $ \v -> do
      p <- parseJSON (Object v)
      return (ParamPos p)

instance ToJSON (ParamVal 'PtPos) where
  toJSON (ParamPos p) = toJSON p

-- ---------------------------------------------------------------------

instance ToJSON ParamValP  where
 toJSON (ParamTextP v) = object [ "text" .= v ]
 toJSON (ParamFileP v) = object [ "file" .= v ]
 toJSON (ParamPosP  p) = toJSON p
 toJSON _ = "error"

instance FromJSON ParamValP where
 parseJSON val = do
   let mt = ParamValP <$> (parseJSON val :: Parser (ParamVal 'PtText))
       mf = ParamValP <$> (parseJSON val :: Parser (ParamVal 'PtFile))
       mp = ParamValP <$> (parseJSON val :: Parser (ParamVal 'PtPos))
   mf <|> mp <|> mt <|> typeMismatch "text, file, or position object for ParamValP" val

-- -------------------------------------

instance ToJSON IdeRequest where
 toJSON (IdeRequest{ideCommand = command, ideParams = params}) =
   object [ "cmd" .= command
          , "params" .= params]

instance FromJSON IdeRequest where
 parseJSON = withObject "IdeRequest" $ \v ->
   IdeRequest <$> v .: "cmd"
              <*> v .: "params"

-- -------------------------------------

instance ToJSON IdeErrorCode where
 toJSON code = String $ T.pack $ show code

instance FromJSON IdeErrorCode where
 parseJSON = withText "IdeErrorCode" $ \s ->
   case reads (T.unpack s) of
     ((c,""):_) -> pure c
     _          -> empty

-- -------------------------------------

instance ToJSON IdeError where
 toJSON err = object [ "code" .= ideCode err
                     , "msg"  .= ideMessage err
                     , "info" .= ideInfo err]

instance FromJSON IdeError where
 parseJSON = withObject "IdeError" $ \v -> IdeError
   <$> v .:  "code"
   <*> v .:  "msg"
   <*> v .:  "info"



-- -------------------------------------

instance ToJSON CabalSection where
  toJSON (CabalSection s) = toJSON s

instance FromJSON CabalSection where
  parseJSON = withText "CabalSection" $ pure . CabalSection

-- -------------------------------------

instance ToJSON ParamDescription where
  toJSON (ParamDesc n h t r) =
    object ["name" .= n,"help" .= h,"type" .= t,"required" .= (r == Required)]

instance FromJSON ParamDescription where
  parseJSON = withObject "ParamDescription" $ \v -> do
    req <- v .: "required"
    if req
      then ParamDesc <$> v .: "name" <*> v .: "help" <*> v .: "type" <*> pure Required
      else ParamDesc <$> v .: "name" <*> v .: "help" <*> v .: "type" <*> pure Optional

-- -------------------------------------

instance ToJSON UntaggedCommandDescriptor where
  toJSON  = Object . jsWrite

instance FromJSON UntaggedCommandDescriptor where
  parseJSON = withObject "UntaggedCommandDescriptor" jsRead

-- -------------------------------------

instance ToJSON Save where
  toJSON SaveNone    = "save_none"
  toJSON SaveAll = "save_all"

instance FromJSON Save where
  parseJSON "save_none" = return SaveNone
  parseJSON "save_all"  = return SaveAll
  parseJSON x           = typeMismatch "Save" x
