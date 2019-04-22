{-# LANGUAGE OverloadedStrings #-}
module ProgressSpec where

import Control.Applicative.Combinators
import Control.Lens
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as L
import Language.Haskell.LSP.Types.Capabilities
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "window/progress" $
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
      

progressCaps :: ClientCapabilities
progressCaps = fullCaps { _window = Just (WindowClientCapabilities (Just True)) }