{-# LANGUAGE OverloadedStrings #-}

module DiagnosticsSpec where

import Control.Applicative.Combinators
import           Control.Lens hiding (List)
import           Control.Monad.IO.Class
import           Data.Aeson (toJSON)
import qualified Data.Text as T
import qualified Data.Default
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.Config
import           Language.Haskell.LSP.Test hiding (message)
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as LSP
import           Test.Hspec
import           TestUtils
import           Utils

-- ---------------------------------------------------------------------

spec :: Spec
spec = describe "diagnostics providers" $ do
  describe "diagnostics triggers" $
    it "runs diagnostics on save" $
      runSession hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
      -- runSessionWithConfig logConfig hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
        logm "starting DiagnosticSpec.runs diagnostic on save"
        doc <- openDoc "ApplyRefact2.hs" "haskell"

        diags@(reduceDiag:_) <- waitForDiagnostics

        -- liftIO $ show diags `shouldBe` ""

        liftIO $ do
          length diags `shouldBe` 2
          reduceDiag ^. LSP.range `shouldBe` Range (Position 1 0) (Position 1 12)
          reduceDiag ^. LSP.severity `shouldBe` Just DsInfo
          reduceDiag ^. LSP.code `shouldBe` Just (StringValue "Eta reduce")
          reduceDiag ^. LSP.source `shouldBe` Just "hlint"

        diags2a <- waitForDiagnostics
        -- liftIO $ show diags2a `shouldBe` ""
        liftIO $ length diags2a `shouldBe` 2

        -- docItem <- getDocItem file languageId
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
        -- diags2hlint <- waitForDiagnostics
        -- -- liftIO $ show diags2hlint `shouldBe` ""
        -- liftIO $ length diags2hlint `shouldBe` 3
        -- diags2liquid <- waitForDiagnostics
        -- liftIO $ length diags2liquid `shouldBe` 3
        -- -- liftIO $ show diags2 `shouldBe` ""
        diags3@(d:_) <- waitForDiagnostics
        -- liftIO $ show diags3 `shouldBe` ""
        liftIO $ do
          length diags3 `shouldBe` 3
          d ^. LSP.range `shouldBe` Range (Position 0 0) (Position 1 0)
          d ^. LSP.severity `shouldBe` Nothing
          d ^. LSP.code `shouldBe` Nothing
          d ^. LSP.source `shouldBe` Just "eg2"
          d ^. LSP.message `shouldBe` T.pack "Example plugin diagnostic, triggered byDiagnosticOnSave"

  describe "typed hole errors" $
    it "is deferred" $
      runSession hieCommand fullCaps "test/testdata" $ do
        _ <- openDoc "TypedHoles.hs" "haskell"
        [diag] <- waitForDiagnosticsSource "bios"
        liftIO $ diag ^. LSP.severity `shouldBe` Just DsWarning

  describe "Warnings are warnings" $
    it "Overrides -Werror" $
      runSession hieCommand fullCaps "test/testdata/wErrorTest" $ do
        _ <- openDoc "src/WError.hs" "haskell"
        [diag] <- waitForDiagnosticsSource "bios"
        liftIO $ diag ^. LSP.severity `shouldBe` Just DsWarning

  describe "only diagnostics on save" $
    it "Respects diagnosticsOnChange setting" $
      runSession hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
        let config = Data.Default.def { diagnosticsOnChange = False } :: Config
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))
        doc <- openDoc "Hover.hs" "haskell"
        diags <- waitForDiagnostics

        liftIO $ do
          length diags `shouldBe` 0

        let te = TextEdit (Range (Position 0 0) (Position 0 13)) ""
        _ <- applyEdit doc te
        skipManyTill loggingNotification noDiagnostics

        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
        diags2 <- waitForDiagnostics
        liftIO $
          length diags2 `shouldBe` 1

-- ---------------------------------------------------------------------
