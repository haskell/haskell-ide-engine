{-# LANGUAGE OverloadedStrings #-}
module ModuleCacheSpec where

import Control.Applicative.Combinators
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Aeson as J
import qualified Data.Text as T
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import System.Directory
import System.Environment
import Test.Hspec
import TestUtils

spec :: Spec
spec = around_ setTelemetry $ describe "module cache" $ do
  it "caches cradle outside of component" $
    runSession hieCommand fullCaps "test/testdata/multiComponent" $ do
      doc <- openDoc "Main.hs" "haskell"
      NotTelemetry (NotificationMessage _ _ (J.String v)) <- skipManyTill loggingNotification (satisfy isTelemetry)
      absPath <- liftIO $ canonicalizePath "test/testdata/multiComponent/Main.hs"
      liftIO $ v `shouldBe` "loadCradle:NewCradle:" <> T.pack absPath
      sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)
      NotTelemetry (NotificationMessage _ _ (J.String v')) <- skipManyTill anyMessage (satisfy isTelemetry)
      liftIO $ v' `shouldBe` "loadCradle:ReuseCradle"
  where isTelemetry (NotTelemetry _) = True
        isTelemetry _                = False
        setTelemetry f = bracket_ (setEnv "HIE_TELEMETRY" "1") (unsetEnv "HIE_TELEMETRY") f
