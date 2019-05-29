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

      -- Initial hlint notifications
      _ <- publishDiagnosticsNotification

      startNotification <- message :: Session ProgressStartNotification
      liftIO $ do
        startNotification ^. L.params . L.title `shouldBe` "Typechecking ApplyRefact2.hs"
        startNotification ^. L.params . L.id `shouldBe` "0"

      doneNotification <- message :: Session ProgressDoneNotification
      liftIO $ doneNotification ^. L.params . L.id `shouldBe` "0"

      -- the ghc-mod diagnostics
      _ <- publishDiagnosticsNotification

      -- Test incrementing ids
      sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

      -- hlint notifications
      _ <- publishDiagnosticsNotification

      startNotification' <- message :: Session ProgressStartNotification
      liftIO $ do
        startNotification' ^. L.params . L.title `shouldBe` "Typechecking ApplyRefact2.hs"
        startNotification' ^. L.params . L.id `shouldBe` "1"

      doneNotification' <- message :: Session ProgressDoneNotification
      liftIO $ doneNotification' ^. L.params . L.id `shouldBe` "1"

      -- the ghc-mod diagnostics
      const () <$> publishDiagnosticsNotification
  it "sends indefinite progress notifications with liquid" $
    -- Testing that Liquid Haskell sends progress notifications
    runSession hieCommand progressCaps "test/testdata" $ do
      doc <- openDoc "liquid/Evens.hs" "haskell"

      skipMany loggingNotification

      -- Initial hlint notifications
      _ <- publishDiagnosticsNotification

      _ <- message :: Session ProgressStartNotification
      _ <- message :: Session ProgressDoneNotification

      -- the ghc-mod diagnostics
      _ <- publishDiagnosticsNotification

      -- Enable liquid haskell plugin
      let config = def { liquidOn  = True, hlintOn = False }
      sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

      -- Test liquid
      sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

      -- hlint notifications
      _ <- publishDiagnosticsNotification

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