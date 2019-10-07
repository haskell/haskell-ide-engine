{-# LANGUAGE OverloadedStrings #-}
module ProgressSpec where

import Control.Applicative.Combinators
import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default
import Haskell.Ide.Engine.Config
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Messages -- TODO: Move this into haskell-lsp-types
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as L
import Language.Haskell.LSP.Types.Capabilities
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "window/progress" $ do
  it "sends indefinite progress notifications" $
    -- Testing that ghc-mod sends progress notifications
    runSession hieCommand progressCaps "test/testdata" $ do
      doc <- openDoc "ApplyRefact2.hs" "haskell"

      skipMany loggingNotification

      startNotification <- message :: Session ProgressStartNotification
      liftIO $ do
        startNotification ^. L.params . L.title `shouldBe` "Initialising Cradle"
        startNotification ^. L.params . L.id `shouldBe` "0"

      reportNotification <- message :: Session ProgressReportNotification
      liftIO $ do
        reportNotification ^. L.params . L.message `shouldBe` Just "Main"
        reportNotification ^. L.params . L.id `shouldBe` "0"

      -- may produce diagnostics
      skipMany publishDiagnosticsNotification

      doneNotification <- message :: Session ProgressDoneNotification
      liftIO $ doneNotification ^. L.params . L.id `shouldBe` "0"

      -- Initial hlint notifications
      _ <- publishDiagnosticsNotification

      -- Test incrementing ids
      sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

      startNotification' <- message :: Session ProgressStartNotification
      liftIO $ do
        startNotification' ^. L.params . L.title `shouldBe` "loading"
        startNotification' ^. L.params . L.id `shouldBe` "1"

      reportNotification' <- message :: Session ProgressReportNotification
      liftIO $ do
        reportNotification' ^. L.params . L.message `shouldBe` Just "Main"
        reportNotification' ^. L.params . L.id `shouldBe` "1"

      doneNotification' <- message :: Session ProgressDoneNotification
      liftIO $ doneNotification' ^. L.params . L.id `shouldBe` "1"

      -- hlint notifications
      _ <- publishDiagnosticsNotification
      return ()

  it "sends indefinite progress notifications with liquid" $
    -- Testing that Liquid Haskell sends progress notifications
    runSession hieCommand progressCaps "test/testdata" $ do
      doc <- openDoc "liquid/Evens.hs" "haskell"

      skipMany loggingNotification

      -- Initial project setup progress notifications
      _ <- message :: Session ProgressStartNotification
      _ <- message :: Session ProgressDoneNotification

      -- Enable liquid haskell plugin
      let config = def { liquidOn  = True, hlintOn = False }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      -- Test liquid
      sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

      -- hlint notifications
      -- TODO: potential race between typechecking, e.g. context intialisation
      -- TODO: and disabling hlint notifications
      -- _ <- publishDiagnosticsNotification

      let startPred (NotProgressStart m) =
            m ^. L.params . L.title == "Running Liquid Haskell on Evens.hs"
          startPred _ = False

      let donePred (NotProgressDone _) = True
          donePred _ = False

      _ <- skipManyTill anyMessage $ between (satisfy startPred) (satisfy donePred) $
              many (satisfy (\x -> not (startPred x || donePred x)))
      return ()

progressCaps :: ClientCapabilities
progressCaps = fullCaps { _window = Just (WindowClientCapabilities (Just True)) }
