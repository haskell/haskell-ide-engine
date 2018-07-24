{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Haskell.Ide.Engine.LSP.CodeActions where

import Control.Lens
import Control.Monad.Reader
import qualified Data.Aeson as J
import qualified Data.Bimap as BM
-- import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Maybe
-- import Data.Monoid ((<>))
import Data.Foldable
import Haskell.Ide.Engine.LSP.Reactor
-- import qualified Haskell.Ide.Engine.Plugin.ApplyRefact as ApplyRefact
-- import qualified Haskell.Ide.Engine.Plugin.Hoogle as Hoogle
-- import qualified Haskell.Ide.Engine.Plugin.HsImport as HsImport
import Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.Core as Core
import qualified Language.Haskell.LSP.Types as J
import qualified Language.Haskell.LSP.Types.Capabilities as C
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Messages
-- import Haskell.Ide.Engine.Plugin.Package
import Haskell.Ide.Engine.IdeFunctions
import Haskell.Ide.Engine.PluginsIdeMonads
-- import Haskell.Ide.Engine.MonadFunctions
-- import Haskell.Ide.Engine.MonadTypes

handleCodeActionReq :: TrackingNumber -> BM.Bimap T.Text T.Text -> J.CodeActionRequest -> R ()
handleCodeActionReq tn commandMap req = do

  maybeRootDir <- asksLspFuncs Core.rootPath

  vfsFunc <- asksLspFuncs Core.getVirtualFileFunc
  docVersion <- fmap _version <$> liftIO (vfsFunc docUri)
  let docId = J.VersionedTextDocumentIdentifier docUri docVersion

  let getProviders :: IdeM (IdeResponse [CodeActionProvider])
      getProviders = do
        IdePlugins m <- lift getPlugins
        return $ IdeResponseOk $ map snd $ toList m

      providersCb :: [CodeActionProvider] -> R ()
      providersCb providers =
        collectRequests' (map (uncurry . uncurry . uncurry) providers)
                         (((docId, maybeRootDir), params ^. J.range), params ^. J.context)
                         cb

      cb :: [[J.CodeAction]] -> R ()
      cb = send . concat
  makeRequest (IReq tn (req ^. J.id) providersCb getProviders)


--       -- For these diagnostics need to search hoogle before we can make code actions
--       addPackageDiags = mapMaybe isPackageAddableDiag diags
--       importableDiags = mapMaybe isImportableDiag diags


--   makeSearches Hoogle.searchPackages (mkAddPackageAction maybeRootDir) addPackageDiags $ \addPackageActions ->
--     makeSearches Hoogle.searchModules mkImportAction importableDiags $ \importActions ->
--       if null importActions
--         then
--           -- If we don't get any results, try relaxing the Hoogle search:
--           -- myFunc :: Int -> String
--           -- will go to:
--           -- myFunc
--           let relaxed = map (bimap id (head . T.words)) importableDiags
--               allActions = ((plainActions ++ addPackageActions) ++)
--           in makeSearches Hoogle.searchModules mkImportAction relaxed (send . allActions)
--         else send (plainActions ++ addPackageActions ++ importActions)

  where
  params = req ^. J.params
  docUri = params ^. J.textDocument . J.uri

  wrapCodeAction :: J.CodeAction -> R (Maybe J.CommandOrCodeAction)
  wrapCodeAction action = do
    (C.ClientCapabilities _ textDocCaps _) <- asksLspFuncs Core.clientCapabilities
    let literalSupport = textDocCaps >>= C._codeAction >>= C._codeActionLiteralSupport
    case literalSupport of
      Nothing ->
        case (action ^. J.edit, action ^. J.command) of
          (Just e, _) -> 
            let cmd = J.Command (action ^. J.title) cmdName (Just cmdParams)
                cmdName = commandMap BM.! "hie:applyWorkspaceEdit"
                cmdParams = J.toJSON [J.ApplyWorkspaceEditParams e]
            in return $ Just (J.CommandOrCodeActionCommand cmd)
          (_, Just (J.Command title cmdName args)) -> do
            let cmd = J.Command title (commandMap BM.! cmdName) args
            return $ Just (J.CommandOrCodeActionCommand cmd)
          _ -> error "A code action needs either a workspace edit or a command"
      Just _ -> return $ Just (J.CommandOrCodeActionCodeAction action)

  send :: [J.CodeAction] -> R ()
  send codeActions = do
    body <- J.List . catMaybes <$> mapM wrapCodeAction codeActions
    reactorSend $ RspCodeAction $ Core.makeResponseMessage req body


--  --TODO: Check if package is already installed
--  mkImportAction :: J.Diagnostic -> T.Text -> Maybe (J.CodeAction, J.Command)
--  mkImportAction diag modName = Just (codeAction, cmd)
--    where
--      codeAction = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) Nothing (Just cmd)
--      cmd = J.Command title cmdName (Just cmdParams)
--      title = "Import module " <> modName
--      cmdName = commandMap BM.! "hsimport:import"
--      cmdParams = J.toJSON [HsImport.ImportParams doc modName]

--  mkAddPackageAction :: Maybe FilePath -> J.Diagnostic -> T.Text -> Maybe (J.CodeAction, J.Command)
--  mkAddPackageAction (Just rootDir) diag packageName = case J.uriToFilePath doc of
--    Just docFp ->
--      let title = "Add " <> packageName <> " as a dependency"
--          cmd = J.Command title (commandMap BM.! "package:add") (Just cmdParams)
--          cmdParams = J.toJSON [AddParams rootDir docFp packageName]
--      in Just (J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) Nothing (Just cmd), cmd)
--    _ -> Nothing
--  mkAddPackageAction _ _ _ = Nothing

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

  collectRequests' :: [a -> IdeM (IdeResponse b)] -> a -> ([b] -> R ()) -> R ()
  collectRequests' reqs x cb = collectRequests (\(f, a) -> f a)
                                               (zip reqs (repeat x))
                                               (cb . map snd)


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

isPackageAddableDiag :: J.Diagnostic -> Maybe (J.Diagnostic, T.Text)
isPackageAddableDiag diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) = (diag,) <$> extractModuleName msg
isPackageAddableDiag _ = Nothing

extractModuleName :: T.Text -> Maybe T.Text
extractModuleName msg
  | T.isPrefixOf "Could not find module " msg = Just $ T.tail $ T.init nameAndQuotes
  | otherwise = Nothing
  where line = T.replace "\n" "" msg
        nameAndQuotes = T.dropWhileEnd (/= '’') $ T.dropWhile (/= '‘') line

