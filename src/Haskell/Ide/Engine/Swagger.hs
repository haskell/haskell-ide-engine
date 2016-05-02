{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-} -- For the File and Text declarations needed by swagger
module Haskell.Ide.Engine.Swagger
  (
    hieSwagger
  ) where

import           Control.Lens
import           Data.List
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Swagger
import           Data.Swagger.Declare
import qualified Data.Text as T
import           GHC.Generics
import           Haskell.Ide.Engine.PluginDescriptor

-- ---------------------------------------------------------------------

hieSwagger :: Plugins -> Swagger
hieSwagger plugins = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare (declareHieSwagger plugins) mempty

-- ---------------------------------------------------------------------

declareHieSwagger :: Plugins -> Declare (Definitions Schema) Swagger
declareHieSwagger plugins = do
  let cmds = concatMap (\(k,pd) -> zip (repeat k) (pdCommands pd)) $ Map.toList plugins
  cmdPaths <- mapM (uncurry commandToPath) cmds

  let h = Just $ Host "localhost" (Just 8001)
  _f <- declareSchema (Proxy :: Proxy File)

  return $ mempty
    & host .~ h
    & paths .~ HM.fromList cmdPaths

-- ---------------------------------------------------------------------

commandToPath :: PluginId -> UntaggedCommand -> Declare (Definitions Schema) (FilePath,PathItem)
commandToPath pluginId c@(Command cd _f) = do
  cmdResponse  <- commandResponse c
  f <- declareSchemaRef (Proxy :: Proxy File)
  t <- declareSchemaRef (Proxy :: Proxy Text) -- NOTE: locally declared Text, not T.Text
  let allParams = nub $ concatMap contextMapping (cmdContexts cd) ++ cmdAdditionalParams cd
  let p = mempty
           & post ?~ (mempty
                     & produces ?~ MimeList ["application/json"]

                     & parameters .~ [
                                       Inline $ mempty
                                       & name .~ "params"
                                       & required ?~ True
                                       & schema .~ ParamBody (Inline (mkParamsSchema f t allParams))
                                     ]

                     & consumes ?~ MimeList ["application/json"]
                     & at 200 ?~ Inline cmdResponse)
  let route =  "/req/" ++ T.unpack pluginId ++ "/" ++ T.unpack (cmdName cd)
  return (route,p)

-- ---------------------------------------------------------------------

mkParamsSchema :: Referenced Schema -> Referenced Schema -> [ParamDescription] -> Schema
mkParamsSchema fileSchemaRef textSchemaRef allParams = s
  where
    pList = map mkParam allParams

    requiredParams = concatMap isRequired allParams

    isRequired :: ParamDescription -> [Data.Swagger.ParamName]
    isRequired pd = if pRequired pd == Required
                       then [pName pd]
                       else []

    mkParam pd = (pName pd,pTypeSchema (pType pd))

    pTypeSchema PtText = textSchemaRef
    pTypeSchema PtFile = fileSchemaRef
    pTypeSchema PtPos  = toSchemaRef (Proxy :: Proxy Pos)
      -- For some reason the Pos reference target does end up in the swagger
      -- definition, so does not need to be passed in

    p = mempty
      & type_ .~ SwaggerObject
      & required .~ requiredParams
      & properties .~ HM.fromList pList

    -- TODO: do we actually need the command and plugin here too?
    s = mempty
      & type_ .~ SwaggerObject
      & properties .~ HM.fromList [("params",Inline p)]

-- ---------------------------------------------------------------------

-- | local type declaration for swagger, so that the generated Schema matches
-- what HIE is expecting
data File = File { file::T.Text} deriving Generic
instance ToSchema File

-- | local type declaration for swagger, so that the generated Schema matches
-- what HIE is expecting
data Text = Text { text::T.Text} deriving Generic
instance ToSchema Text

-- ---------------------------------------------------------------------

-- Keep the type checker happy while extracting the reply schema
commandResponse :: UntaggedCommand -> Declare (Definitions Schema) Response
commandResponse (Command _x f) = declareCmdResponse f

declareCmdResponse :: ToSchema a => CommandFunc a -> Declare (Definitions Schema) Response
declareCmdResponse = declareResponse

-- ---------------------------------------------------------------------

