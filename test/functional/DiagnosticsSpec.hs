{-# LANGUAGE OverloadedStrings #-}

module DiagnosticsSpec where

import Control.Lens hiding (List)
import Control.Monad.IO.Class
import Data.Default
import qualified Data.Text as T
import qualified Language.Haskell.LSP.Test as Test
import Language.Haskell.LSP.Test hiding (message)
import Language.Haskell.LSP.Types as LSP hiding (contents, error )
import qualified Language.Haskell.LSP.Types.Capabilities as C
import Test.Hspec
import TestUtils

-- ---------------------------------------------------------------------

spec :: Spec
spec = describe "diagnostics providers" $ do
  describe "diagnostics triggers" $ do
    it "runs diagnostics on save" $
      runSessionWithConfig codeActionSupportConfig hieCommandExamplePlugin "test/testdata" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"

        diags@(reduceDiag:_) <- waitForDiagnostics

        -- liftIO $ show diags `shouldBe` ""

        liftIO $ do
          length diags `shouldBe` 2
          reduceDiag ^. range `shouldBe` Range (Position 1 0) (Position 1 12)
          reduceDiag ^. severity `shouldBe` Just DsInfo
          reduceDiag ^. code `shouldBe` Just "Eta reduce"
          reduceDiag ^. source `shouldBe` Just "hlint"

        -- docItem <- getDocItem file languageId
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
        diags2hlint <- waitForDiagnostics
        liftIO $ length diags2hlint `shouldBe` 2
        diags2liquid <- waitForDiagnostics
        liftIO $ length diags2liquid `shouldBe` 2
        -- liftIO $ show diags2 `shouldBe` ""
        diags3@(d:_) <- waitForDiagnostics
        -- liftIO $ show diags3 `shouldBe` ""
        liftIO $ do
          length diags3 `shouldBe` 3
          d ^. range `shouldBe` Range (Position 0 0) (Position 1 0)
          d ^. severity `shouldBe` Nothing
          d ^. code `shouldBe` Nothing
          d ^. source `shouldBe` Just "eg2"
          d ^. message `shouldBe` (T.pack "Example plugin diagnostic, triggered byDiagnosticOnSave")


-- ---------------------------------------------------------------------

codeActionSupportConfig :: SessionConfig
codeActionSupportConfig = def { Test.capabilities = codeActionSupportCaps }

codeActionSupportCaps :: C.ClientCapabilities
codeActionSupportCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = C.CodeActionClientCapabilities (Just True) (Just literalSupport)
    literalSupport = C.CodeActionLiteralSupport def
