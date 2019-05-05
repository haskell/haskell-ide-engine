{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskell.Ide.Engine.Plugin.HaRe where

import           Control.Lens.Operators
import           Control.Lens.Traversal
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Aeson
import qualified Data.Aeson.Types                             as J
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import           Data.Foldable
#if __GLASGOW_HASKELL__ < 804
import           Data.Monoid
#endif
import qualified Data.Text                                    as T
import qualified Data.Text.IO                                 as T
import           Exception
import           GHC.Generics                                 (Generic)
import qualified GhcMod.Exe.CaseSplit                         as GM
import qualified GhcMod.Error                                 as GM
import qualified GhcMod.Utils                                 as GM
import           Haskell.Ide.Engine.ArtifactMap
import           Haskell.Ide.Engine.Support.Extras
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           Language.Haskell.GHC.ExactPrint.Print
import qualified Language.Haskell.LSP.Core                    as Core
import qualified Language.Haskell.LSP.Types                   as J
import qualified Language.Haskell.LSP.Types.Lens              as J
import           Language.Haskell.Refact.API                  hiding (logm)
import           Language.Haskell.Refact.HaRe
import           Language.Haskell.Refact.Utils.Monad          hiding (logm)

-- ---------------------------------------------------------------------

hareDescriptor :: PluginDescriptor
hareDescriptor = PluginDescriptor
  { pluginId = "hare"
  , pluginCommands =
      [ PluginCommand "demote" demoteCmd
      , PluginCommand "dupdef" dupdefCmd
      , PluginCommand "iftocase" iftocaseCmd
      , PluginCommand "liftonelevel" liftonelevelCmd
      , PluginCommand "lifttotoplevel" lifttotoplevelCmd
      , PluginCommand "rename" renameCmd
      , PluginCommand "deletedef" deleteDefCmd
      , PluginCommand "genapplicative" genApplicativeCommand
      , PluginCommand "casesplit" splitCaseCmd
      ]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

-- ---------------------------------------------------------------------

data HarePoint =
  HP { hpFile :: Uri
     , hpPos  :: Position
     } deriving (Eq,Generic,Show)

customOptions :: Int -> J.Options
customOptions n = J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop n}

instance FromJSON HarePoint where
  parseJSON = genericParseJSON $ customOptions 2
instance ToJSON HarePoint where
  toJSON = genericToJSON $ customOptions 2

data HarePointWithText =
  HPT { hptFile :: Uri
      , hptPos  :: Position
      , hptText :: T.Text
      } deriving (Eq,Generic,Show)

instance FromJSON HarePointWithText where
  parseJSON = genericParseJSON $ customOptions 3
instance ToJSON HarePointWithText where
  toJSON = genericToJSON $ customOptions 3

data HareRange =
  HR { hrFile     :: Uri
     , hrStartPos :: Position
     , hrEndPos   :: Position
     } deriving (Eq,Generic,Show)

instance FromJSON HareRange where
  parseJSON = genericParseJSON $ customOptions 2
instance ToJSON HareRange where
  toJSON = genericToJSON $ customOptions 2

-- ---------------------------------------------------------------------

demoteCmd :: HarePoint -> IdeGhcM (IdeResult WorkspaceEdit)
demoteCmd (HP uri pos) =
  pluginGetFile "demote: " uri $ \file ->
    runHareCommand "demote" (compDemote file (unPos pos))

-- compDemote :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

dupdefCmd :: HarePointWithText -> IdeGhcM (IdeResult WorkspaceEdit)
dupdefCmd (HPT uri pos name) =
  pluginGetFile "dupdef: " uri $ \file ->
    runHareCommand  "dupdef" (compDuplicateDef file (T.unpack name) (unPos pos))

-- compDuplicateDef :: FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

iftocaseCmd :: HareRange -> IdeGhcM (IdeResult WorkspaceEdit)
iftocaseCmd (HR uri startPos endPos) =
  pluginGetFile "iftocase: " uri $ \file ->
    runHareCommand "iftocase" (compIfToCase file (unPos startPos) (unPos endPos))

-- compIfToCase :: FilePath -> SimpPos -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

liftonelevelCmd :: HarePoint  -> IdeGhcM (IdeResult WorkspaceEdit)
liftonelevelCmd (HP uri pos) =
  pluginGetFile "liftonelevelCmd: " uri $ \file ->
    runHareCommand "liftonelevel" (compLiftOneLevel file (unPos pos))

-- compLiftOneLevel :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

lifttotoplevelCmd :: HarePoint -> IdeGhcM (IdeResult WorkspaceEdit)
lifttotoplevelCmd (HP uri pos) =
  pluginGetFile "lifttotoplevelCmd: " uri $ \file ->
    runHareCommand "lifttotoplevel" (compLiftToTopLevel file (unPos pos))

-- compLiftToTopLevel :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

renameCmd :: HarePointWithText -> IdeGhcM (IdeResult WorkspaceEdit)
renameCmd (HPT uri pos name) =
  pluginGetFile "rename: " uri $ \file ->
      runHareCommand "rename" (compRename file (T.unpack name) (unPos pos))

-- compRename :: FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

deleteDefCmd :: HarePoint -> IdeGhcM (IdeResult WorkspaceEdit)
deleteDefCmd (HP uri pos) =
  pluginGetFile "deletedef: " uri $ \file ->
      runHareCommand "deltetedef" (compDeleteDef file (unPos pos))

-- compDeleteDef ::FilePath -> SimpPos -> RefactGhc [ApplyRefacResult]

-- ---------------------------------------------------------------------

genApplicativeCommand :: HarePoint -> IdeGhcM (IdeResult WorkspaceEdit)
genApplicativeCommand (HP uri pos) =
  pluginGetFile "genapplicative: " uri $ \file ->
      runHareCommand "genapplicative" (compGenApplicative file (unPos pos))


-- ---------------------------------------------------------------------

getRefactorResult :: [ApplyRefacResult] -> [(FilePath,T.Text)]
getRefactorResult = map getNewFile . filter fileModified
  where fileModified ((_,m),_) = m == RefacModified
        getNewFile ((file,_),(ann, parsed)) = (file, T.pack $ exactPrint parsed ann)

makeRefactorResult :: [(FilePath,T.Text)] -> IdeGhcM WorkspaceEdit
makeRefactorResult changedFiles = do
  let
    diffOne :: (FilePath, T.Text) -> IdeGhcM WorkspaceEdit
    diffOne (fp, newText) = do
      origText <- GM.withMappedFile fp $ liftIO . T.readFile
      -- TODO: remove this logging once we are sure we have a working solution
      logm $ "makeRefactorResult:groupedDiff = " ++ show (getGroupedDiff (lines $ T.unpack origText) (lines $ T.unpack newText))
      logm $ "makeRefactorResult:diffops = " ++ show (diffToLineRanges $ getGroupedDiff (lines $ T.unpack origText) (lines $ T.unpack newText))
      liftToGhc $ diffText (filePathToUri fp, origText) newText IncludeDeletions
  diffs <- mapM diffOne changedFiles
  return $ Core.reverseSortEdit $ fold diffs

-- ---------------------------------------------------------------------

runHareCommand :: String -> RefactGhc [ApplyRefacResult]
                 -> IdeGhcM (IdeResult WorkspaceEdit)
runHareCommand name cmd = do
     eitherRes <- runHareCommand' cmd
     case eitherRes of
       Left err ->
         pure (IdeResultFail
                 (IdeError PluginError
                           (T.pack $ name <> ": \"" <> err <> "\"")
                           Null))
       Right res -> do
            let changes = getRefactorResult res
            refactRes <- makeRefactorResult changes
            pure (IdeResultOk refactRes)

-- ---------------------------------------------------------------------

-- newtype RefactGhc a = RefactGhc
--     { unRefactGhc :: StateT RefactState HIE.IdeGhcM a
--     }

runHareCommand' :: forall a. RefactGhc a
                 -> IdeGhcM (Either String a)
runHareCommand' cmd =
  do let initialState =
           -- TODO: Make this a command line flag
           RefSt {rsSettings = defaultSettings
           -- RefSt {rsSettings = logSettings
                 ,rsUniqState = 1
                 ,rsSrcSpanCol = 1
                 ,rsFlags = RefFlags False
                 ,rsStorage = StorageNone
                 ,rsCurrentTarget = Nothing
                 ,rsModule = Nothing}
     let
         cmd' :: StateT RefactState IdeGhcM a
         cmd' = unRefactGhc cmd
         embeddedCmd =
           evalStateT cmd' initialState
         handlers
           :: Applicative m
           => [GM.GHandler m (Either String a)]
         handlers =
           [GM.GHandler (\(ErrorCall e) -> pure (Left e))
           ,GM.GHandler (\(err :: GM.GhcModError) -> pure (Left (show err)))]
     fmap Right embeddedCmd `GM.gcatches` handlers


-- ---------------------------------------------------------------------
-- | This is like hoist from the mmorph package, but build on
-- `MonadTransControl` since we don’t have an `MFunctor` instance.
hoist
  :: (MonadTransControl t,Monad (t m'),Monad m',Monad m)
  => (forall b. m b -> m' b) -> t m a -> t m' a
hoist f a =
  liftWith (\run ->
              let b = run a
                  c = f b
              in pure c) >>=
  restoreT

-- ---------------------------------------------------------------------

codeActionProvider :: CodeActionProvider
codeActionProvider pId docId (J.Range pos _) _ =
  pluginGetFile "HaRe codeActionProvider: " (docId ^. J.uri) $ \file ->
    ifCachedInfo file (IdeResultOk mempty) $ \info ->
      case getArtifactsAtPos pos (defMap info) of
        [h] -> do
          let name = showName $ snd h
          debugm $ show name
          IdeResultOk <$> sequence [
              mkAction "liftonelevel"
                J.CodeActionRefactorExtract $ "Lift " <> name <> " one level"
            , mkAction "lifttotoplevel"
                J.CodeActionRefactorExtract $ "Lift " <> name <> " to top level"
            , mkAction "demote"
                J.CodeActionRefactorInline $ "Demote " <> name <> " one level"
            , mkAction "deletedef"
                J.CodeActionRefactor $ "Delete definition of " <> name
            , mkHptAction "dupdef"
                J.CodeActionRefactor "Duplicate definition of " name
            ]
        _   -> case getArtifactsAtPos pos (locMap info) of
               [h] -> do
                let name = showName $ snd h
                IdeResultOk <$> sequence [
                    mkAction "casesplit"
                      J.CodeActionRefactorRewrite $ "Case split on " <> name
                  ]
               _   -> return $ IdeResultOk []
  where
    mkAction aId kind title = do
      let args = [J.toJSON $ HP (docId ^. J.uri) pos]
      cmd <- mkLspCommand pId aId title (Just args)
      return $ J.CodeAction title (Just kind) mempty Nothing (Just cmd)

    mkHptAction aId kind title name = do
      let args = [J.toJSON $ HPT (docId ^. J.uri) pos (name <> "'")]
      cmd <- mkLspCommand pId aId title (Just args)
      return $ J.CodeAction (title <> name) (Just kind) mempty Nothing (Just cmd)

-- ---------------------------------------------------------------------

splitCaseCmd :: HarePoint -> IdeGhcM (IdeResult WorkspaceEdit)
splitCaseCmd (HP uri newPos) =
  pluginGetFile "splitCaseCmd: " uri $ \path -> do
    origText <- GM.withMappedFile path $ liftIO . T.readFile
    ifCachedModule path (IdeResultOk mempty) $ \tm info -> runGhcModCommand $
      case newPosToOld info newPos of
        Just oldPos -> do
          let (line, column) = unPos oldPos
          splitResult' <- GM.splits' path tm line column
          case splitResult' of
            Just splitResult -> do
              wEdit <- liftToGhc $ splitResultToWorkspaceEdit origText splitResult
              return $ oldToNewPositions info wEdit
            Nothing -> return mempty
        Nothing -> return mempty
  where

    -- | Transform all ranges in a WorkspaceEdit from old to new positions.
    oldToNewPositions :: CachedInfo -> WorkspaceEdit -> WorkspaceEdit
    oldToNewPositions info wsEdit =
      wsEdit
        & J.documentChanges %~ (>>= traverseOf (traverse . J.edits . traverse . J.range) (oldRangeToNew info))
        & J.changes %~ (>>= traverseOf (traverse . traverse . J.range) (oldRangeToNew info))

    -- | Given the range and text to replace, construct a 'WorkspaceEdit'
    -- by diffing the change against the current text.
    splitResultToWorkspaceEdit :: T.Text -> GM.SplitResult -> IdeM WorkspaceEdit
    splitResultToWorkspaceEdit originalText (GM.SplitResult replaceFromLine replaceFromCol replaceToLine replaceToCol replaceWith) =
      diffText (uri, originalText) newText IncludeDeletions
      where
        before = takeUntil (toPos (replaceFromLine, replaceFromCol)) originalText
        after = dropUntil (toPos (replaceToLine, replaceToCol)) originalText
        newText = before <> replaceWith <> after

    -- | Take the first part of text until the given position.
    -- Returns all characters before the position.
    takeUntil :: Position -> T.Text -> T.Text
    takeUntil (Position l c) txt =
      T.unlines takeLines <> takeCharacters
      where
        textLines = T.lines txt
        takeLines = take l textLines
        takeCharacters = T.take c (textLines !! c)

    -- | Drop the first part of text until the given position.
    -- Returns all characters after and including the position.
    dropUntil :: Position -> T.Text -> T.Text
    dropUntil (Position l c) txt = dropCharacters
      where
        textLines = T.lines txt
        dropLines = drop l textLines
        dropCharacters = T.drop c (T.unlines dropLines)

    runGhcModCommand :: IdeGhcM a
                    -> IdeGhcM (IdeResult a)
    runGhcModCommand cmd =
      (IdeResultOk <$> cmd) `gcatch`
        \(e :: GM.GhcModError) ->
          return $
          IdeResultFail $
          IdeError PluginError (T.pack $ "ghc-mod: " ++ show e) Null