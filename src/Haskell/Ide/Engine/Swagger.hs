{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts    #-}
module Haskell.Ide.Engine.Swagger
  (
    hieSwagger
  , hieSwagger2
  ) where

-- Based on https://github.com/GetShopTV/swagger2/blob/master/examples/hackage.hs
-- TODO: Make sure we comply with license conditions.

import           Control.Lens
import           Data.Aeson
import           Data.List
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Proxy
import           Data.Swagger
import           Data.Swagger.Declare
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Haskell.Ide.Engine.Transport.JsonHttp
import           Haskell.Ide.Engine.PluginDescriptor
-- import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.PluginTypes.Singletons
import           Servant
import           Servant.Swagger

import           Haskell.Ide.Engine.BasePlugin

-- ---------------------------------------------------------------------

hieSwagger :: Plugins -> Swagger
hieSwagger plugins = spec & definitions .~ defs
  where
    (defs, spec) = runDeclare (declareHieSwagger plugins) mempty

-- ---------------------------------------------------------------------

{-
instance ToSchema ExtendedCommandDescriptor
instance ToSchema (CommandDescriptor [AcceptedContext] [ParamDescription])
instance ToSchema AcceptedContext
instance ToSchema ParamDescription
instance ToSchema ParamType
instance ToSchema ParamRequired

-- Naughty, doing this here and in SemanticTypes. But the same way each time.
deriving instance Generic  Value
instance ToSchema Value
-}

-- ---------------------------------------------------------------------

declareHieSwagger :: Plugins -> Declare (Definitions Schema) Swagger
declareHieSwagger plugins = do
  -- param schemas
  -- let usernameParamSchema = toParamSchema (Proxy :: Proxy Username)
  let textParamSchema     = toParamSchema (Proxy :: Proxy T.Text)

  -- responses
  -- userSummaryResponse   <- declareResponse (Proxy :: Proxy UserSummary)
  -- userDetailedResponse  <- declareResponse (Proxy :: Proxy UserDetailed)
  -- packagesResponse      <- declareResponse (Proxy :: Proxy [Package])

  objectResponse        <- declareResponse (Proxy :: Proxy Object)
  textResponse          <- declareResponse (Proxy :: Proxy T.Text)
  cmdDescriptorResponse <- declareResponse (Proxy :: Proxy ExtendedCommandDescriptor)

  let Just basePlugin = Map.lookup "base" plugins
  let baseCommands    = map cmdDesc $ pdCommands basePlugin :: [UntaggedCommandDescriptor]
  let versionCmd      = head baseCommands
  -- let (Command _ versionCmdFunc)  = head $ pdCommands basePlugin

  versionDecriptorResponse <- commandResponse (head $ pdCommands basePlugin)
  cmdDetailsResponse       <- commandResponse (head $ drop 3 $ pdCommands basePlugin)

  let h = Just $ Host "localhost" (Just 8001)

  return $ mempty
    & host .~ h
    & paths .~
        [
        --   ("/users", mempty & get ?~ (mempty
        --     & produces ?~ MimeList ["application/json"]
        --     & at 200 ?~ Inline userSummaryResponse))

        -- , ("/user/{username}", mempty & get ?~ (mempty
        --     & produces ?~ MimeList ["application/json"]
        --     & parameters .~ [ Inline $ mempty
        --         & name .~ "username"
        --         & required ?~ True
        --         & schema .~ ParamOther (mempty
        --             & in_ .~ ParamPath
        --             & paramSchema .~ usernameParamSchema) ]
        --     & at 200 ?~ Inline userDetailedResponse))

        -- , ("/packages", mempty & get ?~ (mempty
        --     & produces ?~ MimeList ["application/json"]
        --     & at 200 ?~ Inline packagesResponse))

        -- starting the hie stuff
          ("/req/base/version", mempty
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
                     -- & at 200 ?~ Inline textResponse)
                     & at 200 ?~ Inline versionDecriptorResponse)
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
                     -- & at 200 ?~ Inline cmdDescriptorResponse)
                     & at 200 ?~ Inline cmdDetailsResponse)
          )
        ]

-- ---------------------------------------------------------------------

-- | Swagger spec for Todo API.
-- hieSwagger2 :: (HieServer api, HasServer (PluginRoutes api)) => Proxy api -> Swagger
hieSwagger2 :: (HasServer (PluginRoutes api),HasSwagger api) => Proxy api -> Swagger
hieSwagger2 api = toSwagger api
  -- & info.title   .~ "Todo API"
  -- & info.version .~ "1.0"
  -- & info.description ?~ "This is an API that tests swagger integration"
  -- & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")


-- ---------------------------------------------------------------------

commandToPath :: PluginId -> UntaggedCommand -> Declare (Definitions Schema) (FilePath,PathItem)
commandToPath pName c@(Command cd f) = do
  let textParamSchema     = toParamSchema (Proxy :: Proxy T.Text)
  cmdResponse  <- commandResponse c
  let allParams = nub $ concatMap contextMapping (cmdContexts cd) ++ cmdAdditionalParams cd
  let pi = mempty
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
                     & at 200 ?~ Inline cmdResponse)
  let route =  "req/" ++ T.unpack pName ++ "/" ++ T.unpack (cmdName cd) 
  return (route,pi)

-- ---------------------------------------------------------------------

swaggerParam :: ParamDescription -> Declare (Definitions Schema) (Referenced Param)
swaggerParam pd = do
  let req Required = True
      req Optional = False
  let p = Inline $ mempty
                 & name .~ pName pd
                 & required ?~ req (pRequired pd)
                 & schema .~ ParamOther
                      ( mempty
                      & in_ .~ ParamFormData
                      & paramSchema .~ pSchema (pType pd)
                      )
  return p

pSchema :: ParamType -> ParamSchema t
pSchema PtText = toParamSchema (Proxy :: Proxy T.Text)
pSchema PtFile = toParamSchema (Proxy :: Proxy T.Text)
pSchema PtPos  = toParamSchema (Proxy :: Proxy Pos)

-- ---------------------------------------------------------------------

-- instance ToParamSchema (Int,Int)
instance ToParamSchema (Int,Int) where
  toParamSchema _ = mempty
    & type_ .~ SwaggerArray
    -- & items ?~ SwaggerItemsArray [Inline (toSchema (Proxy :: Proxy Int)),
    --                               Inline (toSchema (Proxy :: Proxy Int))]

{-
  "parameters": [
      {
          "required": true,
          "schema": {
              "items": [
                  {
                      "maximum": 9223372036854775807,
                      "minimum": -9223372036854775808,
                      "type": "integer"
                  },
                  {
                      "maximum": 9223372036854775807,
                      "minimum": -9223372036854775808,
                      "type": "integer"
                  }
              ],
              "type": "array"
          },
          "in": "body",
          "name": "body"
      }

-}


-- ---------------------------------------------------------------------
-- instance ToSchema (Int,Int) where
--   declareNamedSchema = pure (Just "Coord", schema)
--    where
--      schema = mempty
--        & type_ .~ SwaggerObject
--        & properties .~
--            [ ("x", toSchemaRef (Proxy :: Proxy Double))
--            , ("y", toSchemaRef (Proxy :: Proxy Double))
--            ]
--        & required .~ [ "x", "y" ]

-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance ToSchema Coord where
--   declareNamedSchema = pure (Just \"Coord\", schema)
--    where
--      schema = mempty
--        & type_ .~ SwaggerObject
--        & properties .~
--            [ (\"x\", toSchemaRef (Proxy :: Proxy Double))
--            , (\"y\", toSchemaRef (Proxy :: Proxy Double))
--            ]
--        & required .~ [ \"x\", \"y\" ]


-- ---------------------------------------------------------------------

commandResponse :: UntaggedCommand -> Declare (Definitions Schema) Response
commandResponse (Command x f) = declareCmdResponse f

-- ---------------------------------------------------------------------

-- declareResponse :: ToSchema a => proxy a -> Declare (Definitions Schema) Response

declareCmdResponse :: ToSchema a => CommandFunc a -> Declare (Definitions Schema) Response
declareCmdResponse = declareResponse

-- ---------------------------------------------------------------------

{-

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
-}
