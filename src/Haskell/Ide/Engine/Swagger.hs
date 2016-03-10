{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.Swagger
  (
    hieSwagger
  ) where

-- Based on https://github.com/GetShopTV/swagger2/blob/master/examples/hackage.hs
-- TODO: Make sure we comply with license conditions.

import           Control.Lens
import           Data.Aeson
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Swagger
import           Data.Swagger.Declare
-- import           Data.Swagger.Lens
-- import           Data.Swagger.Operation
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Haskell.Ide.Engine.PluginDescriptor

-- ---------------------------------------------------------------------

hieSwagger :: Plugins -> Swagger
hieSwagger plugins = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare (declareHieSwagger plugins) mempty

-- ---------------------------------------------------------------------

instance ToSchema ExtendedCommandDescriptor
instance ToSchema (CommandDescriptor [AcceptedContext] [ParamDescription])
instance ToSchema AcceptedContext
instance ToSchema ParamDescription
instance ToSchema ParamType
instance ToSchema ParamRequired

-- ---------------------------------------------------------------------

declareHieSwagger :: Plugins -> Declare (Definitions Schema) Swagger
declareHieSwagger plugins = do
  -- param schemas
  let usernameParamSchema = toParamSchema (Proxy :: Proxy Username)
  let textParamSchema     = toParamSchema (Proxy :: Proxy T.Text)

  -- responses
  userSummaryResponse   <- declareResponse (Proxy :: Proxy UserSummary)
  userDetailedResponse  <- declareResponse (Proxy :: Proxy UserDetailed)
  packagesResponse      <- declareResponse (Proxy :: Proxy [Package])

  textResponse          <- declareResponse (Proxy :: Proxy T.Text)
  cmdDescriptorResponse <- declareResponse (Proxy :: Proxy ExtendedCommandDescriptor)

  let Just basePlugin = Map.lookup "base" plugins
  let baseCommands    = map cmdDesc $ pdCommands basePlugin :: [UntaggedCommandDescriptor]
  let versionCmd      = head baseCommands

  let h = Just $ Host "localhost" (Just 8001)

  return $ mempty
    & host .~ h
    & paths .~
        [ ("/users", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & at 200 ?~ Inline userSummaryResponse))

        , ("/user/{username}", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & parameters .~ [ Inline $ mempty
                & name .~ "username"
                & required ?~ True
                & schema .~ ParamOther (mempty
                    & in_ .~ ParamPath
                    & paramSchema .~ usernameParamSchema) ]
            & at 200 ?~ Inline userDetailedResponse))

        , ("/packages", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & at 200 ?~ Inline packagesResponse))

        -- starting the hie stuff
        , ("/req/base/version", mempty
           & post ?~ (mempty
                     -- & produces ?~ MimeList ["application/json"]
                     & parameters .~ [
                                     Inline $ mempty
                                     & name .~ "blank"
                                     & required ?~ False
                                     & schema .~ ParamOther (mempty
                                         & in_ .~ ParamFormData
                                     --     & paramSchema .~ usernameParamSchema
                                         )
                                     ]
                     & consumes ?~ MimeList ["application/json"]
                     & at 200 ?~ Inline textResponse)
          )

        , ("/req/base/commandDetail", mempty
           & post ?~ (mempty
                     & produces ?~ MimeList ["application/json"]
                     & parameters .~ [
                                       Inline $ mempty
                                       & name .~ "plugin"
                                       & required ?~ True
                                       & schema .~ ParamOther (mempty
                                           & in_ .~ ParamFormData
                                           & paramSchema .~ textParamSchema
                                           )
                                     , Inline $ mempty
                                       & name .~ "command"
                                       & required ?~ True
                                       & schema .~ ParamOther (mempty
                                           & in_ .~ ParamFormData
                                           & paramSchema .~ textParamSchema
                                           )
                                     ]
                     & consumes ?~ MimeList ["application/json"]
                     & at 200 ?~ Inline cmdDescriptorResponse)
          )
        ]

-- ---------------------------------------------------------------------


type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Generic, ToSchema)

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Generic, ToSchema)

newtype Package = Package { packageName :: Text }
  deriving (Generic, ToSchema)

hackageSwagger :: Swagger
hackageSwagger = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare declareHackageSwagger mempty

declareHackageSwagger :: Declare (Definitions Schema) Swagger
declareHackageSwagger = do
  -- param schemas
  let usernameParamSchema = toParamSchema (Proxy :: Proxy Username)

  -- responses
  userSummaryResponse   <- declareResponse (Proxy :: Proxy UserSummary)
  userDetailedResponse  <- declareResponse (Proxy :: Proxy UserDetailed)
  packagesResponse      <- declareResponse (Proxy :: Proxy [Package])


-- DO NOT EDIT HERE
  return $ mempty
    & paths .~
        [ ("/users", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & at 200 ?~ Inline userSummaryResponse))
        , ("/user/{username}", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & parameters .~ [ Inline $ mempty
                & name .~ "username"
                & required ?~ True
                & schema .~ ParamOther (mempty
                    & in_ .~ ParamPath
                    & paramSchema .~ usernameParamSchema) ]
            & at 200 ?~ Inline userDetailedResponse))
-- DO NOT EDIT HERE
        , ("/packages", mempty & get ?~ (mempty
            & produces ?~ MimeList ["application/json"]
            & at 200 ?~ Inline packagesResponse))
        ]
