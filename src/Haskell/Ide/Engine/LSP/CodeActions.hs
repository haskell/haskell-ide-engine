{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Haskell.Ide.Engine.LSP.CodeActions where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.Bimap as BM
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Maybe
import Data.Foldable
import Haskell.Ide.Engine.LSP.Reactor
import qualified Haskell.Ide.Engine.Plugin.ApplyRefact as ApplyRefact
import qualified Haskell.Ide.Engine.Plugin.Hoogle as Hoogle
import qualified Haskell.Ide.Engine.Plugin.HsImport as HsImport
import Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Capabilities as C
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Messages
import Haskell.Ide.Engine.Plugin.Package
import Haskell.Ide.Engine.MonadTypes

handleCodeActionReq :: TrackingNumber -> BM.Bimap T.Text T.Text -> J.CodeActionRequest -> R ()
handleCodeActionReq tn commandMap req = do

  vfsFunc <- asksLspFuncs Core.getVirtualFileFunc
  maybeVf <- liftIO $ vfsFunc doc
  let docVersion = case maybeVf of
        Just vf -> _version vf
        Nothing -> 0
      docId = J.VersionedTextDocumentIdentifier doc docVersion

  maybeRootDir <- asksLspFuncs Core.rootPath

  let hlintActions = mapMaybe mkHlintAction $ filter validCommand diags
      -- |Some hints do not have an associated refactoring
      validCommand (J.Diagnostic _ _ (Just code) (Just "hlint") _ _) =
        case code of
          "Eta reduce" -> False
          _            -> True
      validCommand _ = False

      renamableActions = map (uncurry (mkRenamableAction docId)) $ concatMap isRenamableDiag diags
      plainActions = renamableActions ++ hlintActions

      -- For these diagnostics need to search hoogle before we can make code actions
      addPackageDiags = mapMaybe isPackageAddableDiag diags
      importableDiags = mapMaybe isImportableDiag diags
  

  makeSearches Hoogle.searchPackages (mkAddPackageAction maybeRootDir) addPackageDiags $ \addPackageActions ->
    makeSearches Hoogle.searchModules mkImportAction importableDiags $ \importActions ->
      if null importActions
        then
          -- If we don't get any results, try relaxing the Hoogle search:
          -- myFunc :: Int -> String
          -- will go to:
          -- myFunc
          let relaxed = map (bimap id (head . T.words)) importableDiags
              allActions = ((plainActions ++ addPackageActions) ++) 
          in makeSearches Hoogle.searchModules mkImportAction relaxed (send . allActions)
        else send (plainActions ++ addPackageActions ++ importActions)

  where
  params = req ^. J.params
  doc = params ^. J.textDocument . J.uri
  (J.List diags) = params ^. J.context . J.diagnostics

  wrapCodeAction :: J.CodeAction -> R (Maybe J.CommandOrCodeAction)
  wrapCodeAction action = do
    (C.ClientCapabilities _ textDocCaps _) <- asksLspFuncs Core.clientCapabilities
    let literalSupport = textDocCaps >>= C._codeAction >>= C._codeActionLiteralSupport
    case literalSupport of
      Nothing -> return $ fmap J.CommandOrCodeActionCommand (action ^. J.command)
      Just _ -> return $ Just (J.CommandOrCodeActionCodeAction action)

  send :: [J.CodeAction] -> R ()
  send codeActions = do
    body <- Just . J.List . catMaybes <$> mapM wrapCodeAction codeActions
    reactorSend $ RspCodeAction $ Core.makeResponseMessage req body

  mkHlintAction :: J.Diagnostic -> Maybe J.CodeAction
  mkHlintAction diag@(J.Diagnostic (J.Range start _) _s (Just code) (Just "hlint") m _) = Just codeAction
    where
      codeAction = J.CodeAction title (Just J.CodeActionRefactor) (Just (J.List [diag])) Nothing (Just cmd)
      title :: T.Text
      title = "Apply hint:" <> head (T.lines m)
      -- NOTE: the cmd needs to be registered via the InitializeResponse message. See hieOptions above
      cmd = J.Command title cmdName cmdparams
      cmdName = commandMap BM.! "applyrefact:applyOne"
      -- need 'file', 'start_pos' and hint title (to distinguish between alternative suggestions at the same location)
      args = J.toJSON [ApplyRefact.AOP doc start code]
      cmdparams = Just args
  mkHlintAction (J.Diagnostic _r _s _c _source _m _) = Nothing

  mkRenamableAction :: J.VersionedTextDocumentIdentifier -> J.Diagnostic -> T.Text -> J.CodeAction
  mkRenamableAction docId diag replacement = codeAction
    where
      title = "Replace with " <> replacement

      workspaceEdit = J.WorkspaceEdit (Just changes) (Just docChanges)
      changes = HM.singleton doc (J.List [textEdit])
      docChanges = J.List [textDocEdit]
      textDocEdit = J.TextDocumentEdit docId (J.List [textEdit])
      textEdit = J.TextEdit (diag ^. J.range) replacement

      cmd = J.Command title cmdName (Just cmdParams)
      cmdName = commandMap BM.! "hie:applyWorkspaceEdit"
      --TODO: Support the name parameter in J.Applyworkspaceeditparams
      cmdParams = J.toJSON [J.ApplyWorkspaceEditParams workspaceEdit]

      codeAction = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) (Just workspaceEdit) (Just cmd)

  --TODO: Check if package is already installed
  mkImportAction :: J.Diagnostic -> T.Text -> Maybe J.CodeAction
  mkImportAction diag modName = Just codeAction
    where
      codeAction = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) Nothing (Just cmd)
      cmd = J.Command title cmdName (Just cmdParams)
      title = "Import module " <> modName
      cmdName = commandMap BM.! "hsimport:import"
      cmdParams = J.toJSON [HsImport.ImportParams doc modName]

  mkAddPackageAction :: Maybe FilePath -> J.Diagnostic -> T.Text -> Maybe J.CodeAction
  mkAddPackageAction (Just rootDir) diag packageName = case J.uriToFilePath doc of
    Just docFp ->
      let title = "Add " <> packageName <> " as a dependency"
          cmd = J.Command title (commandMap BM.! "package:add") (Just cmdParams)
          cmdParams = J.toJSON [AddParams rootDir docFp packageName]
      in Just $ J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) Nothing (Just cmd)
    _ -> Nothing
  mkAddPackageAction _ _ _ = Nothing

  -- | Execute multiple ide requests sequentially
  collectRequests :: (a -> IdeM (IdeResponse b)) -- ^ The requests to make
                  -> [a]                         -- ^ The inputs to the requests
                  -> ([(a, b)] -> R ())          -- ^ Callback with the request inputs and results
                  -> R ()
  collectRequests = go []
    where
      go acc _ [] callback = callback acc
      go acc ideReq (x:xs) callback =
        let reqCallback result = go (acc ++ [(x, result)]) ideReq xs callback
        in makeRequest $ IReq tn (req ^. J.id) reqCallback (ideReq x)

  -- | Make multiple hoogle searches at once to construct code actions
  makeSearches :: (T.Text -> IdeM (IdeResponse [T.Text]))         -- ^ The search function
             -> (J.Diagnostic -> T.Text -> Maybe J.CodeAction)  -- ^ A function to construct actions
             -> [(J.Diagnostic, T.Text)]                        -- ^ Input diagnostics and search terms
             -> ([J.CodeAction] -> R ())                        -- ^ Callback
             -> R ()
  makeSearches search maker xs callback = collectRequests (search . snd) xs $ \allResults -> do
    let actions = concatMap (\((diag, _), results) -> mapMaybe (maker diag) results) allResults
    callback actions

-- TODO: make context specific commands for all sorts of things, such as refactorings          

isImportableDiag :: J.Diagnostic -> Maybe (J.Diagnostic, T.Text)
isImportableDiag diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) = (diag,) <$> extractImportableTerm msg
isImportableDiag _ = Nothing

extractImportableTerm :: T.Text -> Maybe T.Text
extractImportableTerm dirtyMsg = T.strip <$> asum
  [T.stripPrefix "Variable not in scope: " msg,
  T.init <$> T.stripPrefix "Not in scope: type constructor or class ‘" msg]
  where msg = head
              -- Get rid of the rename suggestion parts
              $ T.splitOn "Perhaps you meant "
              $ T.replace "\n" " "
              -- Get rid of trailing/leading whitespace on each individual line
              $ T.unlines $ map T.strip $ T.lines
              $ T.replace "• " "" dirtyMsg

isRenamableDiag :: J.Diagnostic -> [(J.Diagnostic, T.Text)]
isRenamableDiag diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) = map (diag,) $ extractRenamableTerms msg
isRenamableDiag _ = []

extractRenamableTerms :: T.Text -> [T.Text]
extractRenamableTerms msg
  | "Variable not in scope: " `T.isPrefixOf` head noBullets = mapMaybe extractReplacement replacementLines
  | otherwise = []

  where noBullets = T.lines $ T.replace "• " "" msg
        replacementLines = tail noBullets
        extractReplacement line =
          let startOfTerm = T.dropWhile (/= '‘') line
          in if startOfTerm == ""
            then Nothing
            else Just $ T.takeWhile (/= '’') (T.tail startOfTerm)

isPackageAddableDiag :: J.Diagnostic -> Maybe (J.Diagnostic, T.Text)
isPackageAddableDiag diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) = (diag,) <$> extractModuleName msg
isPackageAddableDiag _ = Nothing

extractModuleName :: T.Text -> Maybe T.Text
extractModuleName msg
  | T.isPrefixOf "Could not find module " msg = Just $ T.tail $ T.init nameAndQuotes
  | otherwise = Nothing
  where line = T.replace "\n" "" msg
        nameAndQuotes = T.dropWhileEnd (/= '’') $ T.dropWhile (/= '‘') line

-- TODO: Make code actions for this warning
extractRedundantImport :: T.Text -> Maybe T.Text
extractRedundantImport msg =
  if "The import of " `T.isPrefixOf` firstLine && " is redundant" `T.isSuffixOf` firstLine
    then Just $ T.dropWhileEnd (/= '’') $ T.dropWhile (/= '‘') firstLine
    else Nothing
  where firstLine = head (T.lines msg)