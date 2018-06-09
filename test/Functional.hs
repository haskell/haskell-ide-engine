{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Control.Lens hiding (List)
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Proxy
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types as LSP (error, id)
import Language.Haskell.LSP.Messages
import Test.Hspec
import System.Directory
import System.FilePath
import FunctionalDispatch

main :: IO ()
main = do
  hspec spec
  dispatchSpec

spec :: Spec
spec =
  describe "deferred responses" $ do
    it "do not affect hover requests" $ runSession "test/testdata" $ do
      doc <- openDoc "FuncTest.hs" "haskell"

      id1 <- sendRequest (Proxy :: Proxy HoverRequest)
                  TextDocumentHover
                  (TextDocumentPositionParams doc (Position 4 2))

      skipMany notification
      RspHover hoverRsp <- response
      let (Just (List contents1)) = hoverRsp ^? result . _Just . contents
      liftIO $ contents1 `shouldBe` []
      liftIO $ hoverRsp ^. LSP.id `shouldBe` responseId id1

      id2 <- sendRequest (Proxy :: Proxy DocumentSymbolRequest)
                  TextDocumentDocumentSymbol
                  (DocumentSymbolParams doc)
      RspDocumentSymbols symbolsRsp <- skipManyTill notification response
      liftIO $ symbolsRsp ^. LSP.id `shouldBe` responseId id2

      id3 <- sendRequest (Proxy :: Proxy HoverRequest)
                  TextDocumentHover
                  (TextDocumentPositionParams doc (Position 4 2))
      RspHover hoverRsp2 <- skipManyTill notification response
      liftIO $ hoverRsp2 ^. LSP.id `shouldBe` responseId id3

      let (Just (List contents2)) = hoverRsp2 ^? result . _Just . contents
      liftIO $ contents2 `shouldNotSatisfy` null

      -- Now that we have cache the following request should be instant
      let highlightParams = TextDocumentPositionParams doc (Position 7 0)
      _ <- sendRequest (Proxy :: Proxy DocumentHighlightRequest) TextDocumentDocumentHighlight highlightParams

      RspDocumentHighlights highlightRsp <- response
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

    it "instantly respond to failed modules with no cache" $ runSession "test/testdata" $ do
      doc <- openDoc "FuncTestFail.hs" "haskell"

      _ <- sendRequest (Proxy :: Proxy DocumentSymbolRequest)
                  TextDocumentDocumentSymbol
                  (DocumentSymbolParams doc)
      skipMany notification
      RspDocumentSymbols symbols <- response
      liftIO $ symbols ^. LSP.error `shouldNotBe` Nothing

    it "returns hints as diagnostics" $ runSession "test/testdata" $ do
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
      _ <- sendRequest (Proxy :: Proxy ExecuteCommandRequest)
                       WorkspaceExecuteCommand
                       (ExecuteCommandParams "hare:demote" (Just args))

      RspExecuteCommand executeRsp <- skipManyTill notification response
      liftIO $ executeRsp ^. result `shouldBe` Just (Object H.empty)

      ReqApplyWorkspaceEdit editReq <- request
      liftIO $ editReq ^. params . edit `shouldBe` WorkspaceEdit
            ( Just
            $ H.singleton testUri
            $ List
                [ TextEdit (Range (Position 6 0) (Position 7 6))
                            "  where\n    bb = 5"
                ]
            )
            Nothing