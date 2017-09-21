{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Haskell.Ide.Engine.PluginUtils
  (
    mapEithers
  , pluginGetFile
  , diffText
  , srcSpan2Range
  , srcSpan2Loc
  , reverseMapFile
  , fileInfo
  , realSrcSpan2Range
  , canonicalizeUri
  , newRangeToOld
  , oldRangeToNew
  , newPosToOld
  , oldPosToNew
  , unPos
  , toPos
  , position2pos
  , pos2position
  , uri2fileUri
  , fileUri2uri
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.HashMap.Strict                   as H
import           Data.Monoid
import qualified Data.Text                             as T
import           FastString
import qualified GhcMod.ModuleLoader as GM
import           Haskell.Ide.Engine.MonadTypes
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import           Prelude                               hiding (log)
import           SrcLoc
import           System.Directory
import           System.FilePath

-- ---------------------------------------------------------------------

-- | Converts to one based tuple
unPos :: Position -> (Int,Int)
unPos (Position l c) = (l+1,c+1)

-- | Converts from one based tuple
toPos :: (Int,Int) -> Position
toPos (l,c) = Position (l-1) (c-1)

newPosToOld :: GM.CachedModule -> Position -> Maybe Position
newPosToOld cm p
  = pos2position <$> GM.newPosToOldPos cm (position2pos p)

oldPosToNew :: GM.CachedModule -> Position -> Maybe Position
oldPosToNew cm p
  = pos2position <$> GM.oldPosToNewPos cm (position2pos p)

pos2position :: GM.Pos -> Position
pos2position (GM.Pos l c) = Position l c

position2pos :: Position -> GM.Pos
position2pos (Position l c) = GM.Pos l c

unpackRealSrcSpan' :: RealSrcSpan -> (Position, Position)
unpackRealSrcSpan' rspan = (pos2position s,pos2position e)
  where (s,e) = GM.unpackRealSrcSpan rspan

canonicalizeUri :: MonadIO m => Uri -> m Uri
canonicalizeUri uri =
  case uriToFilePath uri of
    Nothing -> return uri
    Just fp -> do
      fp' <- liftIO $ canonicalizePath fp
      return $ filePathToUri fp'

newRangeToOld :: GM.CachedModule -> Range -> Maybe Range
newRangeToOld cm (Range start end) = do
  start' <- newPosToOld cm start
  end'   <- newPosToOld cm end
  return (Range start' end')

oldRangeToNew :: GM.CachedModule -> Range -> Maybe Range
oldRangeToNew cm (Range start end) = do
  start' <- oldPosToNew cm start
  end'   <- oldPosToNew cm end
  return (Range start' end')

getRealSrcSpan :: SrcSpan -> Either T.Text RealSrcSpan
getRealSrcSpan (RealSrcSpan r)   = pure r
getRealSrcSpan (UnhelpfulSpan x) = Left $ T.pack $ unpackFS x

realSrcSpan2Range :: RealSrcSpan -> Range
realSrcSpan2Range = uncurry Range . unpackRealSrcSpan'

srcSpan2Range :: SrcSpan -> Either T.Text Range
srcSpan2Range spn =
  realSrcSpan2Range <$> getRealSrcSpan spn

reverseMapFile :: MonadIO m => (FilePath -> FilePath) -> FilePath -> m FilePath
reverseMapFile rfm fp =
  liftIO $ canonicalizePath . rfm =<< canonicalizePath fp

srcSpan2Loc :: (MonadIO m) => (FilePath -> FilePath) -> SrcSpan -> m (Either T.Text Location)
srcSpan2Loc revMapp spn = runEitherT $ do
  rspan <- hoistEither $ getRealSrcSpan spn
  let fp = unpackFS $ srcSpanFile rspan
  file <- reverseMapFile revMapp fp
  return $ Location (filePathToUri file) (realSrcSpan2Range rspan)

-- ---------------------------------------------------------------------

-- | Helper function that extracts a filepath from a Uri if the Uri
-- is well formed (i.e. begins with a file:// )
-- fails with an IdeResponseFail otherwise
pluginGetFile
  :: Monad m
  => T.Text -> Uri -> (FilePath -> m (IdeResponse a)) -> m (IdeResponse a)
pluginGetFile name uri f =
  case uriToFilePath uri of
    Just file -> f file
    Nothing -> return $ IdeResponseFail (IdeError PluginError
                 (name <> "Couldn't resolve uri" <> getUri uri) Null)

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

    diffOperationToTextEdit (Addition fm l) = J.TextEdit range nt
    -- fm has a range wrt to the changed file, which starts in the current file at l
    -- So the range has to be shifted to start at l
      where
        range = J.Range (J.Position (l' - 1) 0)
                        (J.Position (l' - 1) 0)
        l' = max l sl -- Needed to add at the end of the file
        sl = fst $ lrNumbers fm
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

-- ---------------------------------------------------------------------

uri2fileUri :: Uri -> GM.FileUri
uri2fileUri (Uri u) = GM.FileUri u

fileUri2uri :: GM.FileUri -> Uri
fileUri2uri (GM.FileUri u) = Uri u
