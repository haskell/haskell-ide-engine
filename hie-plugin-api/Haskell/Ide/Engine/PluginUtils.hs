{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Haskell.Ide.Engine.PluginUtils
  (
    getParams
  , mapEithers
  , missingParameter
  , incorrectParameter
  , validatePlugins
  ) where

import           Data.Aeson

import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Vinyl
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Data.Map as Map
import qualified Data.Text as T
import           Prelude hiding (log)

-- ---------------------------------------------------------------------

-- TODO: should this be in the haskell-ide-plugins-api

-- |If all the listed params are present in the request resturn their values,
-- else return an error message.
getParams :: forall r ts. (ValidResponse r) =>
  Rec TaggedParamId ts -> IdeRequest -> Either (IdeResponse r) (Rec ParamVal ts)
getParams params req = go params
  where
    go :: forall r ts. (ValidResponse r) =>
      Rec TaggedParamId ts -> Either (IdeResponse r) (Rec ParamVal ts)
    go RNil = Right RNil
    go (x:&xs) = case go xs of
                    Left err -> Left err
                    Right ys -> case checkOne x of
                                  Left err -> Left err
                                  Right y -> Right (y:&ys)
    checkOne :: forall r t. (ValidResponse r) =>
      TaggedParamId t -> Either (IdeResponse r) (ParamVal t)
    checkOne (IdText param) = case Map.lookup param (ideParams req) of
      Just (ParamTextP v)  -> Right (ParamText v)
      _ -> Left (missingParameter param)
    checkOne (IdFile param) = case Map.lookup param (ideParams req) of
      Just (ParamFileP v)  -> Right (ParamFile v)
      _ -> Left (missingParameter param)
    checkOne (IdPos param) = case Map.lookup param (ideParams req) of
      Just (ParamPosP v)  -> Right (ParamPos v)
      _ -> Left (missingParameter param)


-- ---------------------------------------------------------------------
-- courtesy of http://stackoverflow.com/questions/19891061/mapeithers-function-in-haskell
mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

-- ---------------------------------------------------------------------
-- Helper functions for errors
-- ---------------------------------------------------------------------

-- Missing parameter error
missingParameter :: forall r. (ValidResponse r) => ParamId -> IdeResponse r
missingParameter param = IdeResponseFail (IdeError MissingParameter
            ("need `" <> param <> "` parameter")
            (Just $ toJSON param))

-- Incorrect parameter error
incorrectParameter :: forall r a b. (ValidResponse r,Show a,Show b)
  => ParamId -> a -> b -> IdeResponse r
incorrectParameter name expected value = IdeResponseFail
    (IdeError IncorrectParameterType
    ("got wrong parameter type for `" <> name <> "`, expected: " <>
      T.pack (show expected) <>" , got:" <> T.pack (show value))
    (Just $ object ["param" .= toJSON name,"expected".= toJSON (show expected),
      "value" .= toJSON (show value)]))


-- ---------------------------------------------------------------------

type ParamNameCollision = (PluginId, [(CommandName, [ParamName])])

-- throw an error if the parameter names are colliding in any of the plugins
validatePlugins :: Plugins -> IO ()
validatePlugins plugins =
  case findParameterNameCollisions plugins of
     [] -> return ()
     collisions -> error (formatParamNameCollisionErrorMsg collisions)

formatParamNameCollisionErrorMsg :: [ParamNameCollision] -> String
formatParamNameCollisionErrorMsg = show -- TODO

findParameterNameCollisions :: Plugins -> [ParamNameCollision]
findParameterNameCollisions plugins =
  let
      collisionsForPlugin (pluginId, pluginDesc) =
        case collisionsForPluginDesc pluginDesc of
          [] -> Nothing
          commands -> Just (pluginId, commands)
      collisionsForPluginDesc pluginDesc = mapMaybe collisionsForCmd (getCmdDesc pluginDesc)
      collisionsForCmd cmd =
          case allCollidingParamNames cmd of
            [] -> Nothing
            paramNames -> Just (cmdName cmd, paramNames)
   in mapMaybe collisionsForPlugin (Map.toList plugins)
   where
         allCollidingParamNames = collidingParamNames . allParams
         getCmdDesc = map cmdDesc . pdCommands
         allParams cmd = cmdAdditionalParams cmd ++ uniqueParamNamesFromContext cmd
         uniqueParamNamesFromContext cmd = nub (concatMap contextMapping (cmdContexts cmd))

collidingParamNames :: [ParamDescription] ->[ParamName]
collidingParamNames params =
  let pNames = map pName params
      uniquePNames = nub pNames
   in pNames \\ uniquePNames
