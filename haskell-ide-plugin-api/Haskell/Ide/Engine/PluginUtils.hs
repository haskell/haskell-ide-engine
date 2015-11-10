{-# LANGUAGE OverloadedStrings #-}
module Haskell.Ide.Engine.PluginUtils
  (
    getParams
  , mapEithers
  , missingParameter
  , incorrectParameter
  ) where

import           Data.Aeson
import           Data.Monoid
import           Haskell.Ide.Engine.PluginDescriptor
import qualified Data.Map as Map
import qualified Data.Text as T
import           Prelude hiding (log)

-- ---------------------------------------------------------------------

-- TODO: should this be in the haskell-ide-plugins-api

-- |If all the listed params are present in the request resturn their values,
-- else return an error message.
getParams :: [ParamId] -> IdeRequest -> Either IdeResponse [ParamVal]
getParams params req = mapEithers checkOne params
  where
    checkOne :: ParamId -> Either IdeResponse ParamVal
    checkOne param = case Map.lookup param (ideParams req) of
      Nothing -> Left (missingParameter param)
      Just v  -> Right v



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

-- Missing parameter error
missingParameter :: ParamId -> IdeResponse
missingParameter param = IdeResponseFail (IdeError MissingParameter
            ("need `" <> param <> "` parameter")
            (Just $ toJSON param))

-- Incorrect parameter error
incorrectParameter :: (Show a,Show b) => ParamId -> a -> b -> IdeResponse
incorrectParameter name expected value = IdeResponseFail
    (IdeError IncorrectParameterType
    ("got wrong parameter type for `" <> name <> "`, expected: " <>
      T.pack (show expected) <>" , got:" <> T.pack (show value))
    (Just $ object ["param" .= toJSON name,"expected".= toJSON (show expected),
      "value" .= toJSON (show value)]))
