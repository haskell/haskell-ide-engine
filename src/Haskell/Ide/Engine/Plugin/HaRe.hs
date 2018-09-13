{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Haskell.Ide.Engine.Plugin.HaRe where

import           Control.Lens.Operators
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
import qualified GhcMod.Error                                 as GM
import qualified GhcMod.Monad                                 as GM
import qualified GhcMod.Utils                                 as GM
import           Haskell.Ide.Engine.ArtifactMap
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import qualified Haskell.Ide.Engine.Plugin.HieExtras          as Hie
import           Language.Haskell.GHC.ExactPrint.Print
import qualified Language.Haskell.LSP.Core                    as Core
import qualified Language.Haskell.LSP.Types                   as J
import qualified Language.Haskell.LSP.Types.Lens              as J
import           Language.Haskell.Refact.API                  hiding (logm)
import           Language.Haskell.Refact.HaRe
import           Language.Haskell.Refact.Utils.Monad          hiding (logm)

-- ---------------------------------------------------------------------

hareDescriptor :: PluginId -> PluginDescriptor
hareDescriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginName = "HaRe"
  , pluginDesc = "A Haskell 2010 refactoring tool. HaRe supports the full "
              <> "Haskell 2010 standard, through making use of the GHC API.  HaRe attempts to "
              <> "operate in a safe way, by first writing new files with proposed changes, and "
              <> "only swapping these with the originals when the change is accepted. "
  , pluginCommands =
      [ PluginCommand "demote" "Move a definition one level down"
          demoteCmd
      , PluginCommand "dupdef" "Duplicate a definition"
          dupdefCmd
      , PluginCommand "iftocase" "Converts an if statement to a case statement"
          iftocaseCmd
      , PluginCommand "liftonelevel" "Move a definition one level up from where it is now"
          liftonelevelCmd
      , PluginCommand "lifttotoplevel" "Move a definition to the top level from where it is now"
          lifttotoplevelCmd
      , PluginCommand "rename" "rename a variable or type"
          renameCmd
      , PluginCommand "deletedef" "Delete a definition"
          deleteDefCmd
      , PluginCommand "genapplicative" "Generalise a monadic function to use applicative"
          genApplicativeCommand
      ]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  }

-- ---------------------------------------------------------------------

customOptions :: Int -> J.Options
customOptions n = J.defaultOptions { J.fieldLabelModifier = J.camelTo2 '_' . drop n}

data HarePoint =
  HP { hpFile :: Uri
     , hpPos  :: Position
     } deriving (Eq,Generic,Show)

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

demoteCmd :: CommandFunc HarePoint WorkspaceEdit
demoteCmd  = CmdSync $ \(HP uri pos) ->
  demoteCmd' uri pos

demoteCmd' :: Uri -> Position -> IdeGhcM (IdeResult WorkspaceEdit)
demoteCmd' uri pos =
  pluginGetFile "demote: " uri $ \file -> do
    runHareCommand "demote" (compDemote file (unPos pos))

-- compDemote :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

dupdefCmd :: CommandFunc HarePointWithText WorkspaceEdit
dupdefCmd = CmdSync $ \(HPT uri pos name) ->
  dupdefCmd' uri pos name

dupdefCmd' :: Uri -> Position -> T.Text -> IdeGhcM (IdeResult WorkspaceEdit)
dupdefCmd' uri pos name =
  pluginGetFile "dupdef: " uri $ \file -> do
    runHareCommand  "dupdef" (compDuplicateDef file (T.unpack name) (unPos pos))

-- compDuplicateDef :: FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

iftocaseCmd :: CommandFunc HareRange WorkspaceEdit
iftocaseCmd = CmdSync $ \(HR uri startPos endPos) ->
  iftocaseCmd' uri (Range startPos endPos)

iftocaseCmd' :: Uri -> Range -> IdeGhcM (IdeResult WorkspaceEdit)
iftocaseCmd' uri (Range startPos endPos) =
  pluginGetFile "iftocase: " uri $ \file -> do
    runHareCommand "iftocase" (compIfToCase file (unPos startPos) (unPos endPos))

-- compIfToCase :: FilePath -> SimpPos -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

liftonelevelCmd :: CommandFunc HarePoint WorkspaceEdit
liftonelevelCmd = CmdSync $ \(HP uri pos) ->
  liftonelevelCmd' uri pos

liftonelevelCmd' :: Uri -> Position -> IdeGhcM (IdeResult WorkspaceEdit)
liftonelevelCmd' uri pos =
  pluginGetFile "liftonelevelCmd: " uri $ \file -> do
    runHareCommand "liftonelevel" (compLiftOneLevel file (unPos pos))

-- compLiftOneLevel :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

lifttotoplevelCmd :: CommandFunc HarePoint WorkspaceEdit
lifttotoplevelCmd = CmdSync $ \(HP uri pos) ->
  lifttotoplevelCmd' uri pos

lifttotoplevelCmd' :: Uri -> Position -> IdeGhcM (IdeResult WorkspaceEdit)
lifttotoplevelCmd' uri pos =
  pluginGetFile "lifttotoplevelCmd: " uri $ \file -> do
    runHareCommand "lifttotoplevel" (compLiftToTopLevel file (unPos pos))

-- compLiftToTopLevel :: FilePath -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

renameCmd :: CommandFunc HarePointWithText WorkspaceEdit
renameCmd = CmdSync $ \(HPT uri pos name) ->
  renameCmd' uri pos name

renameCmd' :: Uri -> Position -> T.Text -> IdeGhcM (IdeResult WorkspaceEdit)
renameCmd' uri pos name =
  pluginGetFile "rename: " uri $ \file -> do
      runHareCommand "rename" (compRename file (T.unpack name) (unPos pos))

-- compRename :: FilePath -> String -> SimpPos -> IO [FilePath]

-- ---------------------------------------------------------------------

deleteDefCmd :: CommandFunc HarePoint WorkspaceEdit
deleteDefCmd  = CmdSync $ \(HP uri pos) ->
  deleteDefCmd' uri pos

deleteDefCmd' :: Uri -> Position -> IdeGhcM (IdeResult WorkspaceEdit)
deleteDefCmd' uri pos =
  pluginGetFile "deletedef: " uri $ \file -> do
      runHareCommand "deltetedef" (compDeleteDef file (unPos pos))

-- compDeleteDef ::FilePath -> SimpPos -> RefactGhc [ApplyRefacResult]

-- ---------------------------------------------------------------------

genApplicativeCommand :: CommandFunc HarePoint WorkspaceEdit
genApplicativeCommand  = CmdSync $ \(HP uri pos) ->
  genApplicativeCommand' uri pos

genApplicativeCommand' :: Uri -> Position -> IdeGhcM (IdeResult WorkspaceEdit)
genApplicativeCommand' uri pos =
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

runHareCommand' :: RefactGhc a
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
     let cmd' = unRefactGhc cmd
         embeddedCmd =
           GM.unGmlT $
           hoist (liftIO . flip evalStateT initialState)
                 (GM.GmlT cmd')
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
codeActionProvider pId docId _ (J.Range pos _) _ =
  pluginGetFile "HaRe codeActionProvider: " (docId ^. J.uri) $ \file ->
    ifCachedInfo file (IdeResultOk mempty) $ \info -> do
      let symbols = getArtifactsAtPos pos (defMap info)
      debugm $ show $ map (Hie.showName . snd) symbols
      if not (null symbols)
        then
          let name = Hie.showName $ snd $ head symbols
            in IdeResultOk <$> sequence [
              mkLiftOneAction name
            , mkLiftTopAction name
            , mkDemoteAction name
            , mkDeleteAction name
            , mkDuplicateAction name
            ]
        else return (IdeResultOk [])

  where
    mkLiftOneAction name = do
      let args = [J.toJSON $ HP (docId ^. J.uri) pos]
          title = "Lift " <> name <> " one level"
      liftCmd <- mkLspCommand pId "liftonelevel" title (Just args)
      return $ J.CodeAction title (Just J.CodeActionRefactorExtract) mempty Nothing (Just liftCmd)

    mkLiftTopAction name = do
      let args = [J.toJSON $ HP (docId ^. J.uri) pos]
          title = "Lift " <> name <> " to top level"
      liftCmd <- mkLspCommand pId "lifttotoplevel" title (Just args)
      return $ J.CodeAction title (Just J.CodeActionRefactorExtract) mempty Nothing (Just liftCmd)

    mkDemoteAction name = do
      let args = [J.toJSON $ HP (docId ^. J.uri) pos]
          title = "Demote " <> name <> " one level"
      demCmd <- mkLspCommand pId "demote" title (Just args)
      return $ J.CodeAction title (Just J.CodeActionRefactorInline) mempty Nothing (Just demCmd)

    mkDeleteAction name = do
      let args = [J.toJSON $ HP (docId ^. J.uri) pos]
          title = "Delete definition of " <> name
      delCmd <- mkLspCommand pId "deletedef" title (Just args)
      return $ J.CodeAction title (Just J.CodeActionRefactor) mempty Nothing (Just delCmd)

    mkDuplicateAction name = do
      let args = [J.toJSON $ HPT (docId ^. J.uri) pos (name <> "'")]
          title = "Duplicate definition of " <> name
      dupCmd <- mkLspCommand pId "dupdef" title (Just args)
      return $ J.CodeAction title (Just J.CodeActionRefactor) mempty Nothing (Just dupCmd)
