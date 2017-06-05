{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Haskell.Ide.Engine.PluginUtils
  (
    getParams
  , mapEithers
  , pluginGetFile
  , diffFiles
  , diffText
  -- * Helper functions for errors
  , missingParameter
  , incorrectParameter
  , fileInfo
  ) where

import           Data.Aeson
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Vinyl
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Prelude hiding (log)
import           System.FilePath

-- ---------------------------------------------------------------------
pluginGetFile
  :: Monad m
  => T.Text -> Uri -> (FilePath -> m (IdeResponse a)) -> m (IdeResponse a)
pluginGetFile name uri f =
  case uriToFilePath uri of
    Just file -> f file
    Nothing -> return $ IdeResponseFail (IdeError PluginError
                 (name <> "Couldn't resolve uri" <> getUri uri) Null)

----------------------------------------

-- |If all the listed params are present in the request return their values,
-- else return an error message.
getParams :: (ValidResponse r) =>
  Rec TaggedParamId ts -> IdeRequest -> Either (IdeResponse r) (Rec ParamVal ts)
getParams params req = go params
  where
    go :: (ValidResponse r) =>
      Rec TaggedParamId ts -> Either (IdeResponse r) (Rec ParamVal ts)
    go RNil = Right RNil
    go (x:&xs) = case go xs of
                    Left err -> Left err
                    Right ys -> case checkOne x of
                                  Left err -> Left err
                                  Right y -> Right (y:&ys)
    checkOne ::
      TaggedParamId t -> Either (IdeResponse r) (ParamVal t)
    checkOne (IdText param) = case Map.lookup param (ideParams req) of
      Just (ParamTextP v) -> Right (ParamText v)
      _ -> Left (missingParameter param)
    checkOne (IdInt param) = case Map.lookup param (ideParams req) of
      Just (ParamIntP v) -> Right (ParamInt v)
      _ -> Left (missingParameter param)
    checkOne (IdBool param) = case Map.lookup param (ideParams req) of
      Just (ParamBoolP v) -> Right (ParamBool v)
      _ -> Left (missingParameter param)
    checkOne (IdFile param) = case Map.lookup param (ideParams req) of
      Just (ParamFileP v) -> Right (ParamFile v)
      _ -> Left (missingParameter param)
    checkOne (IdPos param) = case Map.lookup param (ideParams req) of
      Just (ParamPosP v) -> Right (ParamPos v)
      _ -> Left (missingParameter param)
    checkOne (IdRange param) = case Map.lookup param (ideParams req) of
      Just (ParamRangeP v) -> Right (ParamRange v)
      _ -> Left (missingParameter param)
    checkOne (IdLoc param) = case Map.lookup param (ideParams req) of
      Just (ParamLocP v) -> Right (ParamLoc v)
      _ -> Left (missingParameter param)
    checkOne (IdTextDocId param) = case Map.lookup param (ideParams req) of
      Just (ParamTextDocIdP v) -> Right (ParamTextDocId v)
      _ -> Left (missingParameter param)
    checkOne (IdTextDocPos param) = case Map.lookup param (ideParams req) of
      Just (ParamTextDocPosP v) -> Right (ParamTextDocPos v)
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

-- |Missing parameter error
missingParameter :: ParamId -> IdeResponse r
missingParameter param = IdeResponseFail (IdeError MissingParameter
            ("need `" <> param <> "` parameter")
            (toJSON param))

-- |Incorrect parameter error
incorrectParameter :: forall r a b. (Show a,Show b)
  => ParamId -> a -> b -> IdeResponse r
incorrectParameter name expected value = IdeResponseFail
    (IdeError IncorrectParameterType
    ("got wrong parameter type for `" <> name <> "`, expected: " <>
      T.pack (show expected) <>" , got:" <> T.pack (show value))
    (object ["param" .= toJSON name,"expected".= toJSON (show expected),
     "value" .= toJSON (show value)]))

-- ---------------------------------------------------------------------

-- |Generate a 'HieDiff' value from a pair of files
diffFiles :: FilePath -> FilePath -> IO HieDiff
diffFiles f1 f2 = do
  f1Text <- T.readFile f1
  f2Text <- T.readFile f2
  let dt = diffText (f1,f1Text) (f2,f2Text)
  logm $ "diffFiles:diff=[" ++ dDiff dt ++ "]"
  return dt

-- |Generate a 'HieDiff' value from a pair of source Text
diffText :: (FilePath,T.Text) -> (FilePath,T.Text) -> HieDiff
diffText (f1,f1Text) (f2,f2Text) = HieDiff f1 f2 diff
  where
    d = getGroupedDiff (lines $ T.unpack f1Text) (lines $ T.unpack f2Text)
    diff = ppDiff d

-- ---------------------------------------------------------------------

-- | Returns the directory and file name
fileInfo :: T.Text -> (FilePath,FilePath)
fileInfo tfileName =
  let sfileName = T.unpack tfileName
      dir = takeDirectory sfileName
  in (dir,sfileName)
