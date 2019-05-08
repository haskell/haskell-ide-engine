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
  , makeDiffResult
  , WithDeletions(..)
  , makeAdditiveDiffResult
  , diffText
  , diffText'
  , srcSpan2Range
  , srcSpan2Loc
  , unpackRealSrcSpan
  , reverseMapFile
  , extractRange
  , fullRange
  , fileInfo
  , realSrcSpan2Range
  , canonicalizeUri
  , newRangeToOld
  , oldRangeToNew
  , newPosToOld
  , oldPosToNew
  , unPos
  , toPos
  , clientSupportsDocumentChanges
  , readVFS
  , getRangeFromVFS
  , rangeLinesFromVfs
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.HashMap.Strict                   as H
import           Data.Monoid
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import           Data.Maybe
-- import qualified GhcMod.Utils                          as GM ( makeAbsolute' )
import qualified GhcModCore                          as GM ( makeAbsolute' )
import           FastString
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.ArtifactMap
import           Language.Haskell.LSP.VFS
import           Language.Haskell.LSP.Types.Capabilities
import qualified Language.Haskell.LSP.Types            as J
import           Prelude                               hiding (log)
import           SrcLoc
import           System.Directory
import           System.FilePath
import qualified Data.Rope.UTF16 as Rope

-- ---------------------------------------------------------------------

canonicalizeUri :: MonadIO m => Uri -> m Uri
canonicalizeUri uri =
  case uriToFilePath uri of
    Nothing -> return uri
    Just fp -> do
      fp' <- liftIO $ canonicalizePath fp
      return $ filePathToUri fp'

newRangeToOld :: CachedInfo -> Range -> Maybe Range
newRangeToOld info (Range start end) = do
  start' <- newPosToOld info start
  end'   <- newPosToOld info end
  return (Range start' end')

oldRangeToNew :: CachedInfo -> Range -> Maybe Range
oldRangeToNew info (Range start end) = do
  start' <- oldPosToNew info start
  end'   <- oldPosToNew info end
  return (Range start' end')

getRealSrcSpan :: SrcSpan -> Either T.Text RealSrcSpan
getRealSrcSpan (RealSrcSpan r)   = Right r
getRealSrcSpan (UnhelpfulSpan x) = Left $ T.pack $ unpackFS x

realSrcSpan2Range :: RealSrcSpan -> Range
realSrcSpan2Range = uncurry Range . unpackRealSrcSpan

srcSpan2Range :: SrcSpan -> Either T.Text Range
srcSpan2Range spn =
  realSrcSpan2Range <$> getRealSrcSpan spn



reverseMapFile :: MonadIO m => (FilePath -> FilePath) -> FilePath -> m FilePath
reverseMapFile rfm fp = do
  fp' <- liftIO $ canonicalizePath fp
  debugm $ "reverseMapFile: mapped file is " ++ fp'
  let orig = rfm fp'
  debugm $ "reverseMapFile: original is " ++ orig
  orig' <- liftIO $ canonicalizePath orig
  debugm $ "reverseMapFile: Canonicalized original is " ++ orig
  return orig'

srcSpan2Loc :: (MonadIO m) => (FilePath -> FilePath) -> SrcSpan -> m (Either T.Text Location)
srcSpan2Loc revMapp spn = runExceptT $ do
  let
    foo :: (Monad m) => Either T.Text RealSrcSpan -> ExceptT T.Text m RealSrcSpan
    foo (Left  e) = throwE e
    foo (Right v) = pure v
  rspan <- foo $ getRealSrcSpan spn
  let fp = unpackFS $ srcSpanFile rspan
  debugm $ "srcSpan2Loc: mapped file is " ++ fp
  file <- reverseMapFile revMapp fp
  debugm $ "srcSpan2Loc: Original file is " ++ file
  return $ Location (filePathToUri file) (realSrcSpan2Range rspan)

-- ---------------------------------------------------------------------

-- | Helper function that extracts a filepath from a Uri if the Uri
-- is well formed (i.e. begins with a file:// )
-- fails with an IdeResultFail otherwise
pluginGetFile
  :: Monad m
  => T.Text -> Uri -> (FilePath -> m (IdeResult a)) -> m (IdeResult a)
pluginGetFile name uri f =
  case uriToFilePath uri of
    Just file -> f file
    Nothing -> return $ IdeResultFail (IdeError PluginError
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

data WithDeletions = IncludeDeletions | SkipDeletions
  deriving Eq

-- | Generate a 'WorkspaceEdit' value from an original file and text to replace it.
makeDiffResult :: FilePath -> T.Text -> (FilePath -> FilePath) -> IdeM WorkspaceEdit
makeDiffResult orig new fileMap = do
  origText <- liftIO $ T.readFile orig
  let fp' = fileMap orig
  fp <- liftIO $ GM.makeAbsolute' fp'
  diffText (filePathToUri fp,origText) new IncludeDeletions

-- | A version of 'makeDiffResult' that has does not insert any deletions
makeAdditiveDiffResult :: FilePath -> T.Text -> (FilePath -> FilePath) -> IdeM WorkspaceEdit
makeAdditiveDiffResult orig new fileMap = do
  origText <- liftIO $ T.readFile orig
  let fp' = fileMap orig
  fp <- liftIO $ GM.makeAbsolute' fp'
  diffText (filePathToUri fp,origText) new SkipDeletions

-- | Generate a 'WorkspaceEdit' value from a pair of source Text
-- TODO: Doesn't seem to work with 'editHpackPackage'?
diffText :: (Uri,T.Text) -> T.Text -> WithDeletions -> IdeM WorkspaceEdit
diffText old new withDeletions = do
  supports <- clientSupportsDocumentChanges
  return $ diffText' supports old new withDeletions

-- | A pure version of 'diffText' for testing
diffText' :: Bool -> (Uri,T.Text) -> T.Text -> WithDeletions -> WorkspaceEdit
diffText' supports (f,fText) f2Text withDeletions  =
  if supports
    then WorkspaceEdit Nothing (Just docChanges)
    else WorkspaceEdit (Just h) Nothing
  where
    d = getGroupedDiff (lines $ T.unpack fText) (lines $ T.unpack f2Text)

    diffOps = filter (\x -> (withDeletions == IncludeDeletions) || not (isDeletion x))
                     (diffToLineRanges d)

    isDeletion (Deletion _ _) = True
    isDeletion _ = False

    r = map diffOperationToTextEdit diffOps
    diff = J.List r
    h = H.singleton f diff
    docChanges = J.List [docEdit]
    docEdit = J.TextDocumentEdit (J.VersionedTextDocumentIdentifier f (Just 0)) diff

    diffOperationToTextEdit :: DiffOperation LineRange -> J.TextEdit
    diffOperationToTextEdit (Change fm to) = J.TextEdit range nt
      where
        range = calcRange fm
        nt = T.pack $ init $ unlines $ lrContents to

    {-
      In order to replace everything including newline characters,
      the end range should extend below the last line. From the specification:
      "If you want to specify a range that contains a line including
      the line ending character(s) then use an end position denoting
      the start of the next line"
    -}
    diffOperationToTextEdit (Deletion (LineRange (sl, el) _) _) = J.TextEdit range ""
      where
        range = J.Range (J.Position (sl - 1) 0)
                        (J.Position el 0)

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

extractRange :: Range -> T.Text -> T.Text
extractRange (Range (Position sl _) (Position el _)) s = newS
  where focusLines = take (el-sl+1) $ drop sl $ T.lines s
        newS = T.unlines focusLines

-- | Gets the range that covers the entire text
fullRange :: T.Text -> Range
fullRange s = Range startPos endPos
  where startPos = Position 0 0
        endPos = Position lastLine 0
        {-
        In order to replace everything including newline characters,
        the end range should extend below the last line. From the specification:
        "If you want to specify a range that contains a line including
        the line ending character(s) then use an end position denoting
        the start of the next line"
        -}
        lastLine = length $ T.lines s

-- ---------------------------------------------------------------------

-- | Returns the directory and file name
fileInfo :: T.Text -> (FilePath,FilePath)
fileInfo tfileName =
  let sfileName = T.unpack tfileName
      dir = takeDirectory sfileName
  in (dir,sfileName)

-- ---------------------------------------------------------------------

clientSupportsDocumentChanges :: IdeM Bool
clientSupportsDocumentChanges = do
  ClientCapabilities mwCaps _ _ _ <- getClientCapabilities
  let supports = do
        wCaps <- mwCaps
        WorkspaceEditClientCapabilities mDc <- _workspaceEdit wCaps
        mDc
  return $ fromMaybe False supports

-- ---------------------------------------------------------------------

readVFS :: (MonadIde m, MonadIO m) => Uri -> m (Maybe T.Text)
readVFS uri = do
  mvf <- getVirtualFile uri
  case mvf of
    Just (VirtualFile _ txt _) -> return $ Just (Rope.toText txt)
    Nothing -> return Nothing

getRangeFromVFS :: (MonadIde m, MonadIO m) => Uri -> Range -> m (Maybe T.Text)
getRangeFromVFS uri rg = do
  mvf <- getVirtualFile uri
  case mvf of
    Just vfs -> return $ Just $ rangeLinesFromVfs vfs rg
    Nothing  -> return Nothing

-- ---------------------------------------------------------------------
