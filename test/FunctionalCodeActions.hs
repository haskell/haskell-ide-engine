{-# LANGUAGE OverloadedStrings #-}

module FunctionalCodeActions where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import Language.Haskell.LSP.Test as Test
import Language.Haskell.LSP.Types as LSP hiding (contents, error)
import qualified Language.Haskell.LSP.Types.Capabilities as C
import System.Environment
import Test.Hspec
import TestUtils

codeActionSpec :: Spec
codeActionSpec = do
  it "provides hlint suggestions" $ runSession hieCommand "test/testdata" $ do
    doc <- openDoc "ApplyRefact2.hs" "haskell"
    diagsRsp <- skipManyTill anyNotification notification :: Session PublishDiagnosticsNotification
    let (List diags) = diagsRsp ^. params . diagnostics
        reduceDiag = head diags

    liftIO $ do
      length diags `shouldBe` 2
      reduceDiag ^. range `shouldBe` Range (Position 1 0) (Position 1 12)
      reduceDiag ^. severity `shouldBe` Just DsInfo
      reduceDiag ^. code `shouldBe` Just "Eta reduce"
      reduceDiag ^. source `shouldBe` Just "hlint"

    let r = Range (Position 0 0) (Position 99 99)
        c = CodeActionContext (diagsRsp ^. params . diagnostics) Nothing
    _ <- sendRequest TextDocumentCodeAction (CodeActionParams doc r c)

    rsp <- response :: Session CodeActionResponse
    let (Just (List [(CommandOrCodeActionCommand cmd)])) = fromJust $ rsp ^. result
    liftIO $ cmd ^. title `shouldBe` "Apply hint:Evaluate"



    let args = decode $ encode $ fromJust $ cmd ^. arguments
        execParams = ExecuteCommandParams (cmd ^. command) args
    _ <- sendRequest WorkspaceExecuteCommand execParams
    _ <- skipManyTill anyNotification response :: Session ExecuteCommandResponse

    contents <- getDocumentEdit doc
    liftIO $ contents `shouldBe` "main = undefined\nfoo x = x\n"

    noDiagnostics

  it "provides rename suggestions" $ runSession hieCommand "test/testData" $ do
    doc <- openDoc "CodeActionRename.hs" "haskell"

    -- ignore the first empty hlint diagnostic publish
    [_, diagsRsp] <- skipManyTill loggingNotification (count 2 notification) :: Session [PublishDiagnosticsNotification]
    let (List diags) = diagsRsp ^. params . diagnostics
        diag = head diags
        c = CodeActionContext (diagsRsp ^. params . diagnostics) Nothing

    _ <- sendRequest TextDocumentCodeAction (CodeActionParams doc (diag ^. range) c)

    rsp <- response :: Session CodeActionResponse
    let (Just (List [CommandOrCodeActionCommand cmd])) = fromJust $ rsp ^. result
        args = decode $ encode $ fromJust $ cmd ^. arguments
        execParams = ExecuteCommandParams (cmd ^. command) args

    _ <- sendRequest WorkspaceExecuteCommand execParams
    _ <- response :: Session ExecuteCommandResponse

    contents <- getDocumentEdit doc
    liftIO $ contents `shouldBe` "main = putStrLn \"hello\""

  let codeActionSupportCaps = def { C._textDocument = Just textDocumentCaps }
        where
        textDocumentCaps = def { C._codeAction = Just codeActionCaps }
        codeActionCaps = C.CodeActionClientCapabilities (Just True) (Just literalSupport)
        literalSupport = C.CodeActionLiteralSupport def
      codeActionSupportConfig = def { Test.capabilities = codeActionSupportCaps }
  it "provides import suggestions and 3.8 code action kinds" $
    runSessionWithConfig codeActionSupportConfig hieCommand "test/testData" $ do
      doc <- openDoc "CodeActionImport.hs" "haskell"

      -- ignore the first empty hlint diagnostic publish
      [_, diagsRsp] <- skipManyTill loggingNotification (count 2 notification) :: Session [PublishDiagnosticsNotification]
      let (List (diag:_)) = diagsRsp ^. params . diagnostics
          c = CodeActionContext (diagsRsp ^. params . diagnostics) Nothing

      liftIO $ diag ^. message `shouldBe` "Variable not in scope: when :: Bool -> IO () -> IO ()"

      _ <- sendRequest TextDocumentCodeAction (CodeActionParams doc (diag ^. range) c)

      rsp <- response :: Session CodeActionResponse
      let (Just (List actionsOrCommands)) = fromJust $ rsp ^. result
          actns = map fromAction actionsOrCommands

      liftIO $ do
        head actns ^. title `shouldBe` "Import module Control.Monad"
        forM_ actns $ \a -> do
          a ^. kind `shouldBe` Just CodeActionQuickFix
          a ^. command `shouldSatisfy` isJust
          let hasOneDiag (Just (List [_])) = True
              hasOneDiag _ = False
          a ^. diagnostics `shouldSatisfy` hasOneDiag
        length actns `shouldBe` 5

      let (Just cmd) = head actns ^. command
          args = decode $ encode $ fromJust $ cmd ^. arguments
          execParams = ExecuteCommandParams (cmd ^. command) args

      _ <- sendRequest WorkspaceExecuteCommand execParams
      _ <- response :: Session ExecuteCommandResponse

      contents <- getDocumentEdit doc
      liftIO $ contents `shouldBe` "import Control.Monad\nmain :: IO ()\nmain = when True $ putStrLn \"hello\""


  describe "add package suggestions" $ do
    it "adds to .cabal files" $ withUnsetEnv "STACK_EXE" $
      runSessionWithConfig codeActionSupportConfig hieCommand "test/testdata/addPackageTest/cabal" $ do
        doc <- openDoc "AddPackage.hs" "haskell"

        -- ignore the first empty hlint diagnostic publish
        [_,diagsRsp] <- skipManyTill loggingNotification (count 2 notification) :: Session [PublishDiagnosticsNotification]
        let (List (diag:_)) = diagsRsp ^. params . diagnostics
            c = CodeActionContext (diagsRsp ^. params . diagnostics) Nothing

        liftIO $ diag ^. message `shouldSatisfy` T.isPrefixOf "Could not find module ‘Data.Text’"

        _ <- sendRequest TextDocumentCodeAction (CodeActionParams doc (diag ^. range) c)

        rsp <- response :: Session CodeActionResponse
        let (Just (List [CommandOrCodeActionCodeAction action])) = fromJust $ rsp ^. result

        liftIO $ do
          action ^. title `shouldBe` "Add text as a dependency"
          action ^. kind `shouldBe` Just CodeActionQuickFix
          action ^. command . _Just . command `shouldSatisfy` T.isSuffixOf "package:add"

        let (Just cmd) = action ^. command
            args = decode $ encode $ fromJust $ cmd ^. arguments
            execParams = ExecuteCommandParams (cmd ^. command) args

        _ <- sendRequest WorkspaceExecuteCommand execParams
        _ <- response :: Session ExecuteCommandResponse

        contents <- getDocumentEdit . TextDocumentIdentifier =<< getDocUri "add-package-test.cabal"
        liftIO $ T.lines contents !! 16 `shouldSatisfy`
                  T.isSuffixOf "text -any"
    it "adds to hpack package.yaml files" $ withUnsetEnv "STACK_EXE" $
      runSessionWithConfig codeActionSupportConfig hieCommand "test/testdata/addPackageTest/hpack" $ do
        doc <- openDoc "app/Asdf.hs" "haskell"

        liftIO $ print doc

        -- ignore the first empty hlint diagnostic publish
        [_,diagsRsp] <- skipManyTill loggingNotification (count 2 notification) :: Session [PublishDiagnosticsNotification]
        let (List (diag:_)) = diagsRsp ^. params . diagnostics
            c = CodeActionContext (diagsRsp ^. params . diagnostics) Nothing

        liftIO $ diag ^. message `shouldSatisfy` T.isPrefixOf "Could not find module ‘Codec.Compression.GZip’"

        _ <- sendRequest TextDocumentCodeAction (CodeActionParams doc (diag ^. range) c)

        rsp <- response :: Session CodeActionResponse
        let (Just (List mActions)) = fromJust $ rsp ^. result
            allActions = map fromAction mActions
            action = head allActions

        liftIO $ do
          action ^. title `shouldBe` "Add zlib as a dependency"
          forM_ allActions $ \a -> a ^. kind `shouldBe` Just CodeActionQuickFix
          forM_ allActions $ \a -> a ^. command . _Just . command `shouldSatisfy` T.isSuffixOf "package:add"

        let (Just cmd) = action ^. command
            args = decode $ encode $ fromJust $ cmd ^. arguments
            execParams = ExecuteCommandParams (cmd ^. command) args

        _ <- sendRequest WorkspaceExecuteCommand execParams
        _ <- response :: Session ExecuteCommandResponse

        contents <- getDocumentEdit . TextDocumentIdentifier =<< getDocUri "package.yaml"
        liftIO $ do
          T.lines contents !! 33 `shouldSatisfy` T.isSuffixOf "zlib"
          T.lines contents !! 12 `shouldNotSatisfy` T.isSuffixOf "zlib"
          T.lines contents !! 13 `shouldNotSatisfy` T.isSuffixOf "zlib"

withUnsetEnv :: String -> IO a -> IO a
withUnsetEnv e f = do
  oldStackExe <- lookupEnv e
  unsetEnv e
  res <- f
  forM_ oldStackExe $ setEnv e
  return res

fromAction :: CommandOrCodeAction -> CodeAction
fromAction (CommandOrCodeActionCodeAction action) = action
fromAction _ = error "Not a code action"