{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Haskell.Ide.Engine.PluginTypes.Singletons where

import Control.Applicative
import Data.Aeson
import Data.Singletons.Prelude
import Data.Singletons.TH
import GHC.Generics
import GHC.TypeLits

-- | Indicates the type of a parameter. Plugin authors should use the
-- singleton version `SParamType` which requires prefixing the
-- constructors with an /S/
data ParamType = PtText | PtFile | PtPos
            deriving (Eq,Ord,Show,Read,Bounded,Enum,Generic)

-- | Indicates whether a parameter is required or optional. Plugin
-- atuhors should use the singleton version `SParamRequired` which
-- requires prefixing constructors with an /S/
data ParamRequired = Required | Optional deriving (Eq,Ord,Show,Read,Bounded,Enum,Generic)

-- | Define what context will be accepted from the frontend for the
-- specific command. Matches up to corresponding values for
-- CommandContext. Plugin authors should use the singleton version
-- `SAcceptedContext` which requires prefixing the constructors with
-- an /S/
data AcceptedContext
  = CtxNone        -- ^ No context required, global command
  | CtxFile        -- ^ Works on a whole file
  | CtxPoint       -- ^ A single (Line,Col) in a specific file
  | CtxRegion      -- ^ A region within a specific file
  | CtxCabalTarget -- ^ Works on a specific cabal target
  | CtxProject     -- ^ Works on a the whole project
  deriving (Eq,Ord,Show,Read,Bounded,Enum,Generic)

$(genSingletons [''ParamType, ''ParamRequired, ''AcceptedContext])

-- | Only used via DataKinds. The first symbol represents the param
-- name and the second the param help text.
data ParamDescType = ParamDescType Symbol Symbol ParamType ParamRequired

-- | Singleton version of `ParamDescription`
data SParamDescription (t :: ParamDescType) where
        SParamDesc ::
            (KnownSymbol pName, KnownSymbol pHelp) =>
            Proxy pName ->
              Proxy pHelp ->
                SParamType pType ->
                  SParamRequired pRequired ->
                    SParamDescription ('ParamDescType pName pHelp pType pRequired)

instance ToJSON ParamType where
  toJSON PtText = String "text"
  toJSON PtFile = String "file"
  toJSON PtPos  = String "pos"

instance FromJSON ParamType where
  parseJSON (String "text") = pure PtText
  parseJSON (String "file") = pure PtFile
  parseJSON (String "pos")  = pure PtPos
  parseJSON _               = empty

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
