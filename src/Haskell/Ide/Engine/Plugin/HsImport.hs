{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
module Haskell.Ide.Engine.Plugin.HsImport where

import           Control.Lens.Operators
import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Aeson
import           Data.Bitraversable
import           Data.Bifunctor
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified GHC.Generics                  as Generics
import qualified GhcMod.Utils                  as GM
import           HsImport
import           Haskell.Ide.Engine.Config
import           Haskell.Ide.Engine.MonadTypes
import qualified Haskell.Ide.Engine.Support.Extras as Hie
import qualified Language.Haskell.LSP.Types      as J
import qualified Language.Haskell.LSP.Types.Lens as J
import qualified Haskell.Ide.Engine.Hoogle
                                               as Hoogle
import           Haskell.Ide.Engine.PluginUtils
import           System.Directory
import           System.IO

hsimportDescriptor :: PluginDescriptor
hsimportDescriptor = PluginDescriptor
  { pluginId = "hsimport"
  , pluginCommands = [PluginCommand "import" importCmd]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

data ImportParams = ImportParams
  { file           :: Uri
  , moduleToImport :: T.Text
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

importCmd :: ImportParams -> IdeGhcM (IdeResult J.WorkspaceEdit)
importCmd (ImportParams uri modName) =
  pluginGetFile "hsimport cmd: " uri $ \origInput -> do
    shouldFormat <- formatOnImportOn <$> getConfig

    fileMap      <- GM.mkRevRedirMapFunc
    GM.withMappedFile origInput $ \input -> do

      tmpDir            <- liftIO getTemporaryDirectory
      (output, outputH) <- liftIO $ openTempFile tmpDir "hsimportOutput"
      liftIO $ hClose outputH

      let args = defaultArgs { moduleName    = T.unpack modName
                             , inputSrcFile  = input
                             , outputSrcFile = output
                             }
      maybeErr <- liftIO $ hsimportWithArgs defaultConfig args
      case maybeErr of
        Just err -> do
          liftIO $ removeFile output
          let msg = T.pack $ show err
          return $ IdeResultFail (IdeError PluginError msg Null)
        Nothing -> do
          newText <- liftIO $ T.readFile output
          liftIO $ removeFile output
          J.WorkspaceEdit mChanges mDocChanges <- liftToGhc
            $ makeDiffResult input newText fileMap

          if shouldFormat
            then do
              config  <- getConfig
              plugins <- getPlugins
              let mprovider = Hie.getFormattingPlugin config plugins
              case mprovider of
                Nothing ->
                  return $ IdeResultOk (J.WorkspaceEdit mChanges mDocChanges)

                Just (_, provider) -> do
                  let formatEdit :: J.TextEdit -> IdeGhcM J.TextEdit
                      formatEdit origEdit@(J.TextEdit _ t) = do
                        -- TODO: are these default FormattingOptions ok?
                        res <- liftToGhc $ provider t uri FormatDocument (FormattingOptions 2 True)
                        let formatEdits = case res of
                                            IdeResultOk xs -> xs
                                            _ -> []
                        return $ foldl' J.editTextEdit origEdit formatEdits

                  -- behold: the legendary triple mapM
                  newChanges <- (mapM . mapM . mapM) formatEdit mChanges

                  newDocChanges <- forM mDocChanges $ \change -> do
                    let cmd (J.TextDocumentEdit vids edits) = do
                          newEdits <- mapM formatEdit edits
                          return $ J.TextDocumentEdit vids newEdits
                    mapM cmd change

                  return
                    $ IdeResultOk (J.WorkspaceEdit newChanges newDocChanges)
            else return $ IdeResultOk (J.WorkspaceEdit mChanges mDocChanges)

codeActionProvider :: CodeActionProvider
codeActionProvider plId docId _ context = do
  let J.List diags = context ^. J.diagnostics
      terms = mapMaybe getImportables diags

  res <- mapM (bimapM return Hoogle.searchModules) terms
  actions <- catMaybes <$> mapM (uncurry mkImportAction) (concatTerms res)

  if null actions
     then do
       let relaxedTerms = map (bimap id (head . T.words)) terms
       relaxedRes <- mapM (bimapM return Hoogle.searchModules) relaxedTerms
       relaxedActions <- catMaybes <$> mapM (uncurry mkImportAction) (concatTerms relaxedRes)
       return $ IdeResultOk relaxedActions
     else return $ IdeResultOk actions

  where
    concatTerms = concatMap (\(d, ts) -> map (d,) ts)

    --TODO: Check if package is already installed
    mkImportAction :: J.Diagnostic -> T.Text -> IdeM (Maybe J.CodeAction)
    mkImportAction diag modName = do
      cmd <- mkLspCommand plId "import" title  (Just cmdParams)
      return (Just (codeAction cmd))
     where
       codeAction cmd = J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [diag])) Nothing (Just cmd)
       title = "Import module " <> modName
       cmdParams = [toJSON (ImportParams (docId ^. J.uri) modName)]

    getImportables :: J.Diagnostic -> Maybe (J.Diagnostic, T.Text)
    getImportables diag@(J.Diagnostic _ _ _ (Just "ghcmod") msg _) = (diag,) <$> extractImportableTerm msg
    getImportables _ = Nothing

extractImportableTerm :: T.Text -> Maybe T.Text
extractImportableTerm dirtyMsg = T.strip <$> asum
  [ T.stripPrefix "Variable not in scope: " msg
  , T.init <$> T.stripPrefix "Not in scope: type constructor or class ‘" msg
  , T.stripPrefix "Data constructor not in scope: " msg]
  where msg = head
              -- Get rid of the rename suggestion parts
              $ T.splitOn "Perhaps you meant "
              $ T.replace "\n" " "
              -- Get rid of trailing/leading whitespace on each individual line
              $ T.unlines $ map T.strip $ T.lines
              $ T.replace "• " "" dirtyMsg
