{-# LANGUAGE OverloadedStrings #-}

module DiagnosticsSpec where

import           Control.Lens hiding (List)
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Haskell.Ide.Engine.MonadFunctions
import           Language.Haskell.LSP.Test hiding (message)
import           Language.Haskell.LSP.Types as LSP
import           Language.Haskell.LSP.Types.Lens as LSP hiding (contents, error )
import           Test.Hspec
import           TestUtils
import           Utils

-- ---------------------------------------------------------------------

spec :: Spec
spec = describe "diagnostics providers" $ do
  describe "diagnostics triggers" $ do
    it "runs diagnostics on save" $
      runSession hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
      -- runSessionWithConfig logConfig hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
        logm "starting DiagnosticSpec.runs diagnostic on save"
        doc <- openDoc "ApplyRefact2.hs" "haskell"

        diags <- waitForDiagnostics
        reduceDiag <- case diags of
          (reduceDiag:_) -> return reduceDiag
          _ -> fail "match fail"

        -- liftIO $ show diags `shouldBe` ""

        liftIO $ do
          length diags `shouldBe` 2
          reduceDiag ^. range `shouldBe` Range (Position 1 0) (Position 1 12)
          reduceDiag ^. severity `shouldBe` Just DsInfo
          reduceDiag ^. code `shouldBe` Just "Eta reduce"
          reduceDiag ^. source `shouldBe` Just "hlint"

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
        diags3 <- waitForDiagnostics
        d <- case diags3 of
          (d:_) -> return d
          _ -> fail "match fail"
        -- liftIO $ show diags3 `shouldBe` ""
        liftIO $ do
          length diags3 `shouldBe` 3
          d ^. range `shouldBe` Range (Position 0 0) (Position 1 0)
          d ^. severity `shouldBe` Nothing
          d ^. code `shouldBe` Nothing
          d ^. source `shouldBe` Just "eg2"
          d ^. message `shouldBe` T.pack "Example plugin diagnostic, triggered byDiagnosticOnSave"

  describe "typed hole errors" $
    it "is deferred" $
      runSession hieCommand fullCaps "test/testdata" $ do
        _ <- openDoc "TypedHoles.hs" "haskell"
        mdiags <- waitForDiagnosticsSource "ghcmod"
        diag <- case mdiags of
          [diag] -> return diag
          _ -> fail "match fail"
        liftIO $ diag ^. severity `shouldBe` Just DsWarning

  describe "Warnings are warnings" $
    it "Overrides -Werror" $
      runSession hieCommand fullCaps "test/testdata/wErrorTest" $ do
        _ <- openDoc "src/WError.hs" "haskell"
        mdiags <- waitForDiagnosticsSource "ghcmod"
        diag <- case mdiags of
          [diag] -> return diag
          _ -> fail "match fail"
        liftIO $ diag ^. severity `shouldBe` Just DsWarning

-- ---------------------------------------------------------------------
