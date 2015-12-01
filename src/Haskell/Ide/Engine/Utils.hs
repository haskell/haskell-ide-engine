{-# LANGUAGE OverloadedStrings #-}

module Haskell.Ide.Engine.Utils
  (
    validatePlugins
  , PluginDescriptionError(..)
  , ParamCollision(..)
  , ParamOccurence(..)
  ) where

import           Data.List
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Data.Map as Map

-- |For all the plugins, validate that parameter names are unique within each
-- command. Otherwise return an error, describing the collisions.
validatePlugins :: Plugins -> Maybe PluginDescriptionError
validatePlugins plugins =
 case paramNameCollisions plugins of
    [] -> Nothing
    collisions -> Just PluginDescriptionError {
         pdeCollisions = collisions
       , pdeErrorMsg = formatParamNameCollisionErrorMsg collisions
     }

data PluginDescriptionError =
  PluginDescriptionError {
      pdeCollisions :: [ParamCollision]
    , pdeErrorMsg :: String
    } deriving (Eq, Show)

data ParamCollision = ParamCollision PluginId CommandName ParamName [ParamOccurence] deriving (Eq, Show)
data ParamOccurence = AdditionalParam ParamDescription
                    | ContextParam ParamDescription AcceptedContext
                    deriving (Eq, Show)

paramNameCollisions :: Plugins -> [ParamCollision]
paramNameCollisions plugins =
  concatMap (\(plId, plDesc) ->
    concatMap (paramNameCollisionsForCmd plId . cmdDesc) (pdCommands plDesc)) (Map.toList plugins)

paramNameCollisionsForCmd :: PluginId -> CommandDescriptor -> [ParamCollision]
paramNameCollisionsForCmd plId cmdDescriptor =
  let collidingParamNames = findCollidingParamNames cmdDescriptor
      collisionSources = map (\param -> ParamCollision plId (cmdName cmdDescriptor) param (paramsByName cmdDescriptor param)) collidingParamNames
   in collisionSources

-- |Find all the parameters within the CommandDescriptor that goes by the given ParamName
paramsByName :: CommandDescriptor -> ParamName -> [ParamOccurence]
paramsByName cmdDescriptor paramName =
  let matchingParamName param = pName param == paramName
      additionalParams = map AdditionalParam $ filter matchingParamName (cmdAdditionalParams cmdDescriptor)
      cmdContextParams = concatMap
                           (\accContext -> map
                                             (\param -> (param, accContext))
                                           (contextMapping accContext))
                         (cmdContexts cmdDescriptor)
      cmdContextParamsWithCollisions = map (uncurry ContextParam) $ filter (\(param, _) -> matchingParamName param) cmdContextParams
   in additionalParams ++ cmdContextParamsWithCollisions

findCollidingParamNames :: CommandDescriptor -> [ParamName]
findCollidingParamNames cmdDescriptor =
  let cmdContextPNames = nub $ concatMap (map pName . contextMapping) (cmdContexts cmdDescriptor) -- collisions within AcceptedContext should not count
      additionalPNames = map pName (cmdAdditionalParams cmdDescriptor)
      allPNames = cmdContextPNames ++ additionalPNames
      uniquePNames = nub allPNames
   in nub (allPNames \\ uniquePNames)

formatParamNameCollisionErrorMsg :: [ParamCollision] -> String
formatParamNameCollisionErrorMsg paramCollisions =
  "Error: Parameter name collision in plugin description\n" ++
  "Parameter names must be unique for each command. The following collisions were found:\n" ++
    concatMap (\(ParamCollision plId cmd paramName occurences) ->
      ("In " ++ show plId ++ ":" ++ show cmd ++ " the parameter " ++ show paramName ++ " is defined in:\n" ++ unlines (map formatOccurence occurences)))
    paramCollisions
  where formatOccurence (AdditionalParam paramDesc)         = "    cmdAdditionalParams = " ++ show paramDesc
        formatOccurence (ContextParam paramDesc accContext) = "    cmdContexts = [" ++ show accContext ++ "]: " ++ show paramDesc
