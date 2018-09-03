{-# LANGUAGE OverloadedStrings #-}

module LiquidSpec where

import           Control.Lens hiding (List)
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Text as T
import           Language.Haskell.LSP.Test hiding (message)
-- import           Language.Haskell.LSP       as LSP
import           Language.Haskell.LSP.Types as LSP hiding (contents, error )
import           Haskell.Ide.Engine.LSP.Config
import           Test.Hspec
import           TestUtils
import           Utils

-- ---------------------------------------------------------------------

spec :: Spec
spec = describe "liquid haskell diagnostics" $ do
    it "runs diagnostics on save, no liquid" $
      -- runSessionWithConfig noLogConfig hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
      runSessionWithConfig logConfig hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
        doc <- openDoc "liquid/Evens.hs" "haskell"

        diags@(reduceDiag:_) <- waitForDiagnostics

        -- liftIO $ show diags `shouldBe` ""
        liftIO $ putStrLn "a"

        liftIO $ do
          length diags `shouldBe` 2
          reduceDiag ^. range `shouldBe` Range (Position 5 18) (Position 5 22)
          reduceDiag ^. severity `shouldBe` Just DsHint
          reduceDiag ^. code `shouldBe` Just "Use negate"
          reduceDiag ^. source `shouldBe` Just "hlint"

        liftIO $ putStrLn "b"

        diags2hlint <- waitForDiagnostics
        liftIO $ putStrLn "c"
        -- liftIO $ show diags2hlint `shouldBe` ""
        liftIO $ length diags2hlint `shouldBe` 2

        -- docItem <- getDocItem file languageId
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

        diags2liquid <- waitForDiagnostics
        liftIO $ putStrLn "d"
        liftIO $ length diags2liquid `shouldBe` 3
        -- liftIO $ show diags2liquid `shouldBe` ""

        diags3@(d:_) <- waitForDiagnostics
        liftIO $ putStrLn "e"
        -- liftIO $ show diags3 `shouldBe` ""
        liftIO $ do
          length diags3 `shouldBe` 3
          d ^. range `shouldBe` Range (Position 0 0) (Position 1 0)
          d ^. severity `shouldBe` Nothing
          d ^. code `shouldBe` Nothing
          d ^. source `shouldBe` Just "eg2"
          d ^. message `shouldBe` (T.pack "Example plugin diagnostic, triggered byDiagnosticOnSave")

    -- ---------------------------------

    it "runs diagnostics on save, with liquid haskell" $
      runSessionWithConfig noLogConfig hieCommand codeActionSupportCaps "test/testdata" $ do
      -- runSessionWithConfig logConfig hieCommand codeActionSupportCaps "test/testdata" $ do
        doc <- openDoc "liquid/Evens.hs" "haskell"

        diags@(reduceDiag:_) <- waitForDiagnostics

        -- liftIO $ show diags `shouldBe` ""

        liftIO $ do
          length diags `shouldBe` 2
          reduceDiag ^. range `shouldBe` Range (Position 5 18) (Position 5 22)
          reduceDiag ^. severity `shouldBe` Just DsHint
          reduceDiag ^. code `shouldBe` Just "Use negate"
          reduceDiag ^. source `shouldBe` Just "hlint"

        -- Enable liquid haskell plugin
        let config =
             Config
               { hlintOn             = False
               , maxNumberOfProblems = 50
               , liquidOn            = True
               }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        -- docItem <- getDocItem file languageId
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
        diags2hlint <- waitForDiagnostics
        -- liftIO $ show diags2hlint `shouldBe` ""

        -- We turned hlint diagnostics off
        liftIO $ length diags2hlint `shouldBe` 0
        diags2liquid <- waitForDiagnostics
        liftIO $ length diags2liquid `shouldBe` 0
        -- liftIO $ show diags2liquid `shouldBe` ""
        diags3@(d:_) <- waitForDiagnostics
        -- liftIO $ show diags3 `shouldBe` ""
        liftIO $ do
          length diags3 `shouldBe` 1
          d ^. range `shouldBe` Range (Position 8 0) (Position 8 7)
          d ^. severity `shouldBe` Just DsError
          d ^. code `shouldBe` Nothing
          d ^. source `shouldBe` Just "liquid"
          d ^. message `shouldBe` (T.pack "Error: Liquid Type Mismatch\n  Inferred type\n    VV : {v : Int | v == (7 : int)}\n \n  not a subtype of Required type\n    VV : {VV : Int | VV mod 2 == 0}\n \n  In Context")


-- ---------------------------------------------------------------------
