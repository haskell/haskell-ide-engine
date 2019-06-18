{-# LANGUAGE OverloadedStrings #-}
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Control.Monad.IO.Class
import Control.Applicative.Combinators
import Control.Concurrent
import Control.Monad
import Haskell.Ide.Engine.Config
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Default


main = runSessionWithConfig (defaultConfig { logStdErr = True, logMessages = True, messageTimeout = 500 }) "/home/matt/ghc/hie-wrapper -d" fullCaps "/home/matt/ParsleyHaskell" $ do
  let config = def { hlintOn = False }
  sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))
  doc <- openDoc "CodeGenerator.hs" "haskell"
  waitForDiagnostics
  replicateM_ 50 $ do
    liftIO $ putStrLn "----editing----"
    let te = TextEdit (Range (Position 37 0) (Position 37 0)) " "
    applyEdit doc te
--    documentContents doc >>= liftIO . T.putStrLn
    sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

    waitForDiagnostics

