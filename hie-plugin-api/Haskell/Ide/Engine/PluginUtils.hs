{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Haskell.Ide.Engine.PluginUtils
  (
    getParams
  , mapEithers
  , pluginGetFile
  , diffText
  , srcLoc2Range
  , srcLoc2Loc
  , makeAsync
  -- * Helper functions for errors
  , missingParameter
  , incorrectParameter
  , fileInfo
  ) where

import           Control.Lens                          (view)
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Data.Aeson
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.HashMap.Strict                   as H
import qualified Data.Map                              as Map
import           Data.Monoid
import qualified Data.Text                             as T
import           Data.Vinyl
import           FastString
import qualified GhcMod.Monad                          as GM
import qualified GhcMod.Utils                          as GM
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import           Prelude                               hiding (log)
import           SrcLoc
import           System.Directory
import           System.FilePath

-- ---------------------------------------------------------------------
makeAsync :: IO (IdeResponse a) -> IdeM (Async a)
makeAsync action = liftIO $ do
  resMVar <- newEmptyMVar
  _ <- forkIO $ action >>= putMVar resMVar
  return $ \callback -> takeMVar resMVar >>= callback
-- ---------------------------------------------------------------------

srcLoc2Range :: (Monad m, MonadIO m) => SrcSpan -> GM.GhcModT m (Either String Range)
srcLoc2Range = (fmap . fmap) (view J.range) . srcLoc2Loc

srcLoc2Loc :: (Monad m, MonadIO m) => SrcSpan -> GM.GhcModT m (Either String Location)
srcLoc2Loc (RealSrcSpan r) = do
  revMapp <- GM.mkRevRedirMapFunc
  file <- liftIO $ makeAbsolute $ revMapp $ unpackFS $ srcSpanFile r
  return $ Right $ Location (filePathToUri file) $ Range (toPos (l1,c1)) (toPos (l2,c2))
  where s = realSrcSpanStart r
        l1 = srcLocLine s
        c1 = srcLocCol s
        e = realSrcSpanEnd r
        l2 = srcLocLine e
        c2 = srcLocCol e
srcLoc2Loc (UnhelpfulSpan x) = return $ Left $ unpackFS x

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
                                  Right y  -> Right (y:&ys)
    checkOne ::
      TaggedParamId t -> Either (IdeResponse r) (ParamVal t)
    checkOne (IdText param) = case Map.lookup param (ideParams req) of
      Just (ParamTextP v) -> Right (ParamText v)
      _                   -> Left (missingParameter param)
    checkOne (IdInt param) = case Map.lookup param (ideParams req) of
      Just (ParamIntP v) -> Right (ParamInt v)
      _                  -> Left (missingParameter param)
    checkOne (IdBool param) = case Map.lookup param (ideParams req) of
      Just (ParamBoolP v) -> Right (ParamBool v)
      _                   -> Left (missingParameter param)
    checkOne (IdFile param) = case Map.lookup param (ideParams req) of
      Just (ParamFileP v) -> Right (ParamFile v)
      _                   -> Left (missingParameter param)
    checkOne (IdPos param) = case Map.lookup param (ideParams req) of
      Just (ParamPosP v) -> Right (ParamPos v)
      _                  -> Left (missingParameter param)
    checkOne (IdRange param) = case Map.lookup param (ideParams req) of
      Just (ParamRangeP v) -> Right (ParamRange v)
      _                    -> Left (missingParameter param)
    checkOne (IdLoc param) = case Map.lookup param (ideParams req) of
      Just (ParamLocP v) -> Right (ParamLoc v)
      _                  -> Left (missingParameter param)
    checkOne (IdTextDocId param) = case Map.lookup param (ideParams req) of
      Just (ParamTextDocIdP v) -> Right (ParamTextDocId v)
      _                        -> Left (missingParameter param)
    checkOne (IdTextDocPos param) = case Map.lookup param (ideParams req) of
      Just (ParamTextDocPosP v) -> Right (ParamTextDocPos v)
      _                         -> Left (missingParameter param)


-- ---------------------------------------------------------------------
-- courtesy of http://stackoverflow.com/questions/19891061/mapeithers-function-in-haskell
mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y  -> Right (y:ys)
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

-- |Generate a 'WorkspaceEdit' value from a pair of source Text
diffText :: (Uri,T.Text) -> T.Text -> WorkspaceEdit
diffText (f,fText) f2Text = WorkspaceEdit (Just h) Nothing
  where
    d = getGroupedDiff (lines $ T.unpack fText) (lines $ T.unpack f2Text)
    diffOps = diffToLineRanges d
    r = map diffOperationToTextEdit diffOps
    diff = J.List r
    h = H.singleton f diff

    diffOperationToTextEdit :: DiffOperation LineRange -> J.TextEdit
    diffOperationToTextEdit (Change fm to) = J.TextEdit range nt
      where
        range = calcRange fm
        nt = T.pack $ init $ unlines $ lrContents to

    diffOperationToTextEdit (Deletion fm _) = J.TextEdit range ""
      where
        range = calcRange fm

    diffOperationToTextEdit (Addition fm _) = J.TextEdit range nt
      where
        range = calcRange fm
        nt = T.pack $ unlines $ lrContents fm


    calcRange fm = J.Range s e
      where
        sl = fst $ lrNumbers fm
        sc = 0
        s = J.Position (sl - 1) sc -- Note: zero-based lines
        el = snd $ lrNumbers fm
        ec = length $ last $ lrContents fm
        e = J.Position (el - 1) ec  -- Note: zero-based lines

-- ---------------------------------------------------------------------

-- | Returns the directory and file name
fileInfo :: T.Text -> (FilePath,FilePath)
fileInfo tfileName =
  let sfileName = T.unpack tfileName
      dir = takeDirectory sfileName
  in (dir,sfileName)
