{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Control.Lens hiding (List)
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Language.Haskell.LSP.Test hiding (capabilities)
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types as LSP (error, id)
import Test.Hspec
import System.Directory
import System.FilePath
import FunctionalDispatch
import FunctionalCodeActions
import TestUtils

main :: IO ()
main = do
  setupStackFiles
  withFileLogging "functional.log" $ do
    hspec spec
    cdAndDo "./test/testdata" $ hspec dispatchSpec

spec :: Spec
spec = do
  describe "deferred responses" $ do
    it "do not affect hover requests" $ runSession hieCommand "test/testdata" $ do
      doc <- openDoc "FuncTest.hs" "haskell"

      id1 <- sendRequest TextDocumentHover (TextDocumentPositionParams doc (Position 4 2))

      skipMany anyNotification
      hoverRsp <- response :: Session HoverResponse
      let (Just (List contents1)) = hoverRsp ^? result . _Just . contents
      liftIO $ contents1 `shouldBe` []
      liftIO $ hoverRsp ^. LSP.id `shouldBe` responseId id1

      id2 <- sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc)
      symbolsRsp <- skipManyTill anyNotification response :: Session DocumentSymbolsResponse
      liftIO $ symbolsRsp ^. LSP.id `shouldBe` responseId id2

      id3 <- sendRequest TextDocumentHover (TextDocumentPositionParams doc (Position 4 2))
      hoverRsp2 <- skipManyTill anyNotification response :: Session HoverResponse
      liftIO $ hoverRsp2 ^. LSP.id `shouldBe` responseId id3

      let (Just (List contents2)) = hoverRsp2 ^? result . _Just . contents
      liftIO $ contents2 `shouldNotSatisfy` null

      -- Now that we have cache the following request should be instant
      let highlightParams = TextDocumentPositionParams doc (Position 7 0)
      _ <- sendRequest TextDocumentDocumentHighlight highlightParams

      highlightRsp <- response :: Session DocumentHighlightsResponse
      let (Just (List locations)) = highlightRsp ^. result
      liftIO $ locations `shouldBe` [ DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 7, _character = 0}
                       , _end   = Position {_line = 7, _character = 2}
                       }
                     , _kind  = Just HkWrite
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 7, _character = 0}
                       , _end   = Position {_line = 7, _character = 2}
                       }
                     , _kind  = Just HkWrite
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 5, _character = 6}
                       , _end   = Position {_line = 5, _character = 8}
                       }
                     , _kind  = Just HkRead
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 7, _character = 0}
                       , _end   = Position {_line = 7, _character = 2}
                       }
                     , _kind  = Just HkWrite
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 7, _character = 0}
                       , _end   = Position {_line = 7, _character = 2}
                       }
                     , _kind  = Just HkWrite
                     }
                   , DocumentHighlight
                     { _range = Range
                       { _start = Position {_line = 5, _character = 6}
                       , _end   = Position {_line = 5, _character = 8}
                       }
                     , _kind  = Just HkRead
                     }
                   ]

    it "instantly respond to failed modules with no cache" $ runSession hieCommand "test/testdata" $ do
      doc <- openDoc "FuncTestFail.hs" "haskell"

      _ <- sendRequest TextDocumentDocumentSymbol (DocumentSymbolParams doc)
      skipMany anyNotification
      symbols <- response :: Session DocumentSymbolsResponse
      liftIO $ symbols ^. LSP.error `shouldNotBe` Nothing

    it "returns hints as diagnostics" $ runSession hieCommand "test/testdata" $ do
      _ <- openDoc "FuncTest.hs" "haskell"

      cwd <- liftIO getCurrentDirectory
      let testUri = filePathToUri $ cwd </> "test/testdata/FuncTest.hs"

      diags <- skipManyTill loggingNotification publishDiagnosticsNotification
      liftIO $ diags ^? params `shouldBe` (Just $ PublishDiagnosticsParams
                { _uri         = testUri
                , _diagnostics = List
                  [ Diagnostic
                      (Range (Position 9 6) (Position 10 18))
                      (Just DsInfo)
                      (Just "Redundant do")
                      (Just "hlint")
                      "Redundant do\nFound:\n  do putStrLn \"hello\"\nWhy not:\n  putStrLn \"hello\"\n"
                      Nothing
                  ]
                }
              )

      let args' = H.fromList [("pos", toJSON (Position 7 0)), ("file", toJSON testUri)]
          args = List [Object args']
      _ <- sendRequest WorkspaceExecuteCommand (ExecuteCommandParams "hare:demote" (Just args))

      executeRsp <- skipManyTill anyNotification response :: Session ExecuteCommandResponse
      liftIO $ executeRsp ^. result `shouldBe` Just (Object H.empty)

      editReq <- request :: Session ApplyWorkspaceEditRequest
      liftIO $ editReq ^. params . edit `shouldBe` WorkspaceEdit
            ( Just
            $ H.singleton testUri
            $ List
                [ TextEdit (Range (Position 6 0) (Position 7 6))
                            "  where\n    bb = 5"
                ]
            )
            Nothing

  -- -----------------------------------

  describe "multi-server setup" $
    it "doesn't have clashing commands on two servers" $ do
      let getCommands = runSession hieCommand "test/testdata" $ do
              rsp <- initializeResponse
              let uuids = rsp ^? result . _Just . capabilities . executeCommandProvider . _Just . commands
              return $ fromJust uuids
      List uuids1 <- getCommands
      List uuids2 <- getCommands
      liftIO $ forM_ (zip uuids1 uuids2) (uncurry shouldNotBe)

  -- -----------------------------------

  describe "code action support" codeActionSpec

  -- -----------------------------------

  describe "multiple main modules" $
    it "Can load one file at a time, when more than one Main module exists"
                                  -- $ runSession hieCommand "test/testdata" $ do
                                  $ runSession hieCommandVomit "test/testdata" $ do
      _doc <- openDoc "ApplyRefact2.hs" "haskell"
      _diagsRspHlint <- skipManyTill anyNotification notification :: Session PublishDiagnosticsNotification
      diagsRspGhc   <- skipManyTill anyNotification notification :: Session PublishDiagnosticsNotification
      let (List diags) = diagsRspGhc ^. params . diagnostics

      liftIO $ length diags `shouldBe` 2

      _doc2 <- openDoc "HaReRename.hs" "haskell"
      _diagsRspHlint2 <- skipManyTill anyNotification notification :: Session PublishDiagnosticsNotification
      -- errMsg <- skipManyTill anyNotification notification :: Session ShowMessageNotification
      diagsRsp2 <- skipManyTill anyNotification notification :: Session PublishDiagnosticsNotification
      let (List diags2) = diagsRsp2 ^. params . diagnostics

      liftIO $ show diags2 `shouldBe` "[]"
