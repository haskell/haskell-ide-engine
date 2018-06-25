{-# LANGUAGE LambdaCase #-}
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
import Haskell.Ide.Engine.MonadFunctions
import qualified Haskell.Ide.Engine.Plugin.ApplyRefact as ApplyRefact
import qualified Haskell.Ide.Engine.Plugin.Hoogle as Hoogle
import qualified Haskell.Ide.Engine.Plugin.HsImport as HsImport
import Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Capabilities as C
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Messages

handleCodeActionReq :: TrackingNumber -> BM.Bimap T.Text T.Text -> (PluginRequest R -> R ()) -> J.CodeActionRequest -> R ()
handleCodeActionReq tn commandMap makeRequest req = do

  let params = req ^. J.params
      doc = params ^. J.textDocument . J.uri
      (J.List diags) = params ^. J.context . J.diagnostics

  vfsFunc <- asks Core.getVirtualFileFunc
  maybeVf <- liftIO $ vfsFunc doc
  let docVersion = case maybeVf of
        Just vf -> _version vf
        Nothing -> 0

  let
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

    hlintActions = mapMaybe mkHlintAction $ filter validCommand diags
      where
            -- |Some hints do not have an associated refactoring
        validCommand (J.Diagnostic _ _ (Just code) (Just "hlint") _ _) =
          case code of
            "Eta reduce" -> False
            _            -> True
        validCommand _ = False


    mkImportAction diag modName = codeAction
      where
        codeAction = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) Nothing (Just cmd)
        cmd = J.Command title cmdName (Just cmdParams)
        title = "Import module " <> modName
        cmdName = commandMap BM.! "hsimport:import"
        cmdParams = J.toJSON [HsImport.ImportParams doc modName]

    searchModules term callback = do
      let searchReq = IReq tn (req ^. J.id)
                              (callback . take 5)
                              (Hoogle.searchModules term)
      logm $ "Searching modules for " ++ show term
      makeRequest searchReq

    renamableActions = map (uncurry mkRenamableAction) $ concatMap isRenamableDiag diags

    mkRenamableAction diag replacement = codeAction
      where
        title = "Replace with " <> replacement

        workspaceEdit = J.WorkspaceEdit (Just changes) (Just docChanges)
        changes = HM.singleton doc (J.List [textEdit])
        docChanges = J.List [textDocEdit]
        textDocEdit = J.TextDocumentEdit docId (J.List [textEdit])
        docId = J.VersionedTextDocumentIdentifier doc docVersion
        textEdit = J.TextEdit (diag ^. J.range) replacement

        cmd = J.Command title cmdName (Just cmdParams)
        cmdName = commandMap BM.! "hie:applyWorkspaceEdit"
        --TODO: Support the name parameter in J.Applyworkspaceeditparams
        cmdParams = J.toJSON [J.ApplyWorkspaceEditParams workspaceEdit]

        codeAction = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) (Just workspaceEdit) (Just cmd)

    send :: [J.CodeAction] -> R ()
    send codeActions = do
      body <- Just . J.List . catMaybes <$> mapM wrapCodeActionIfNeeded codeActions
      reactorSend $ RspCodeAction $ Core.makeResponseMessage req body

    plainActions = renamableActions ++ hlintActions

    importableDiags = mapMaybe isImportableDiag diags

    in
      -- If we have possible import code actions
      -- we need to make a request to Hoogle
      if not (null importableDiags) then do
        let -- Will look like myFunc :: Int -> String
            (diag, term) = head importableDiags
            makeActions = map (mkImportAction diag)
        searchModules term $ \case
          -- Try with just the name and no type
          -- e.g. myFunc
          [] -> searchModules (head (T.words term)) (send . (++ plainActions) . makeActions)
          -- We got some module sos use these
          xs -> send $ makeActions xs ++ plainActions
      else
        send plainActions

  where wrapCodeActionIfNeeded :: J.CodeAction -> R (Maybe J.CommandOrCodeAction)
        wrapCodeActionIfNeeded action@(J.CodeAction _ _ _ _ cmd) = do
          (C.ClientCapabilities _ textDocCaps _) <- asks Core.clientCapabilities
          let literalSupport = textDocCaps >>= C._codeAction >>= C._codeActionLiteralSupport
          case literalSupport of
            Nothing -> return $ fmap J.CommandOrCodeActionCommand cmd
            Just _ -> return $ Just (J.CommandOrCodeActionCodeAction action)

-- TODO: make context specific commands for all sorts of things, such as refactorings          

isImportableDiag :: J.Diagnostic -> Maybe (J.Diagnostic, T.Text)
isImportableDiag diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) = fmap (diag,) $ extractImportableTerm msg
isImportableDiag _ = Nothing

extractImportableTerm :: T.Text -> Maybe T.Text
extractImportableTerm dirtyMsg = asum
  [T.stripPrefix "Variable not in scope: " msg,
  T.init <$> T.stripPrefix "Not in scope: type constructor or class ‘" msg]
  where msg = head $ T.lines $ T.replace "• " "" dirtyMsg

isRenamableDiag :: J.Diagnostic -> [(J.Diagnostic, T.Text)]
isRenamableDiag diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) = map (diag,) $ extractRenamableTerms msg
isRenamableDiag _ = []

extractRenamableTerms :: T.Text -> [T.Text]
extractRenamableTerms msg = mapMaybe extractReplacement replacementLines

  where noBullets = T.lines $ T.replace "• " "" msg
        replacementLines = tail noBullets
        extractReplacement line =
          let startOfTerm = T.dropWhile (/= '‘') line
          in if startOfTerm == ""
            then Nothing
            else Just $ T.takeWhile (/= '’') (T.tail startOfTerm)