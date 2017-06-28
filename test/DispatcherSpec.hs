{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module DispatcherSpec where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.Types
import qualified Language.Haskell.LSP.TH.DataTypesJSON as J
import qualified Data.Set as S
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dispatcher" dispatcherSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

dispatcherSpec :: Spec
dispatcherSpec = do
  describe "checking contexts" $ do

    it "identifies CtxNone" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmd1" (Map.fromList [])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxNone]"))

    -- ---------------------------------

    it "identifies bad CtxFile" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmd2" (Map.fromList [])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseFail (IdeError {ideCode = MissingParameter, ideMessage = "need `file` parameter", ideInfo = String "file"}))

    -- ---------------------------------

    it "identifies CtxFile" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmd2" (Map.fromList [("file", ParamFileP $ filePathToUri "foo.hs")])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxFile]"))

    -- ---------------------------------

    it "identifies CtxPoint" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmd3" (Map.fromList [("file", ParamFileP $ filePathToUri "foo.hs"),("start_pos", ParamPosP (toPos (1,2)))])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxPoint]"))

    -- ---------------------------------

    it "identifies CtxRegion" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmd4" (Map.fromList [("file", ParamFileP $ filePathToUri "foo.hs")
                                                ,("start_pos", ParamPosP (toPos (1,2)))
                                                ,("end_pos", ParamPosP (toPos (3,4)))])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxRegion]"))

    -- ---------------------------------

    it "identifies CtxCabal" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmd5" (Map.fromList [("cabal", ParamTextP "lib")])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxCabalTarget]"))


    -- ---------------------------------

    it "identifies CtxProject" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmd6" (Map.fromList [("dir", ParamFileP $ filePathToUri ".")])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxProject]"))

    -- ---------------------------------

    it "identifies all multiple" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmdmultiple" (Map.fromList [("file", ParamFileP $ filePathToUri "foo.hs")
                                                       ,("start_pos", ParamPosP (toPos (1,2)))
                                                       ,("end_pos", ParamPosP (toPos (3,4)))])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxFile,CtxPoint,CtxRegion]"))

    -- ---------------------------------

    it "identifies CtxFile,CtxPoint multiple" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmdmultiple" (Map.fromList [("file", ParamFileP $ filePathToUri "foo.hs")
                                                       ,("start_pos", ParamPosP (toPos (1,2)))])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxFile,CtxPoint]"))

    -- ---------------------------------

    it "identifies error when no match multiple" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmdmultiple" (Map.fromList [("cabal", ParamTextP "lib")])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe`
        Just (IdeResponseFail (IdeError { ideCode = MissingParameter
                                        , ideMessage = "need `file` parameter"
                                        , ideInfo = String "file"}))

  -- -----------------------------------

  describe "checking extra params" $ do

    it "identifies matching params" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmdextra" (Map.fromList [("file", ParamFileP $ filePathToUri "foo.hs")
                                                    ,("txt",  ParamTextP "a")
                                                    ,("file", ParamFileP $ filePathToUri "f")
                                                    ,("pos",  ParamPosP (toPos (1,2)))
                                                    ,("int", ParamIntP 4 )
                                                    ,("bool", ParamBoolP False )
                                                    ,("range", ParamRangeP $ Range (toPos (1,2)) (toPos (3,4)))
                                                    ,("loc", ParamLocP $ Location (filePathToUri "a") $ Range (toPos (1,2)) (toPos (3,4)))
                                                    ,("textDocId", ParamTextDocIdP $ TextDocumentIdentifier $ filePathToUri "ad" )
                                                    ,("textDocPos", ParamTextDocPosP $ TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "asd") (toPos (1,2)) )
                                                    ])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxFile]"))


    -- ---------------------------------

    it "reports mismatched param" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmdextra" (Map.fromList [("file", ParamFileP $ filePathToUri "foo.hs")
                                                    ,("txt",  ParamFileP $ filePathToUri "a")
                                                    ,("file", ParamFileP $ filePathToUri "f")
                                                    ,("pos",  ParamPosP (toPos (1,2)))
                                                    ,("int", ParamIntP 4 )
                                                    ,("bool", ParamBoolP False )
                                                    ,("range", ParamRangeP $ Range (toPos (1,2)) (toPos (3,4)))
                                                    ,("loc", ParamLocP $ Location (filePathToUri "a") $ Range (toPos (1,2)) (toPos (3,4)))
                                                    ,("textDocId", ParamTextDocIdP $ TextDocumentIdentifier $ filePathToUri "ad" )
                                                    ,("textDocPos", ParamTextDocPosP $ TextDocumentPositionParams (TextDocumentIdentifier $ filePathToUri "asd") (toPos (1,2)) )
                                                    ])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe`
         Just (IdeResponseFail
               (IdeError
                 { ideCode = IncorrectParameterType
                 , ideMessage = "got wrong parameter type for `txt`, expected: PtText , got:ParamValP {unParamValP = ParamFile (Uri {getUri = \"file://a\"})}"
                 , ideInfo = Object (HM.fromList [("value",String "ParamValP {unParamValP = ParamFile (Uri {getUri = \"file://a\"})}"),("param",String "txt"),("expected",String "PtText")])}))



    -- ---------------------------------

    it "reports matched optional param" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmdoptional" (Map.fromList [("txt",   ParamTextP "a")
                                                       ,("fileo", ParamFileP $ filePathToUri "f")
                                                       ,("poso",  ParamPosP (toPos (1,2)))
                                                       ])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe` Just (IdeResponseOk (String "result:ctxs=[CtxNone]"))

    -- ---------------------------------

    it "reports mismatched optional param" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req = IdeRequest "cmdoptional" (Map.fromList [("txt",   ParamTextP "a")
                                                       ,("fileo", ParamTextP "f")
                                                       ,("poso",  ParamPosP (toPos (1,2)))
                                                       ])
          cr = CReq "test" 1 req chan
      r <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr)
      r `shouldBe`
        Just (IdeResponseFail
              (IdeError { ideCode = IncorrectParameterType
                        , ideMessage = "got wrong parameter type for `fileo`, expected: PtFile , got:ParamValP {unParamValP = ParamText \"f\"}"
                        , ideInfo = Object (HM.fromList [("value"
                                                      ,String "ParamValP {unParamValP = ParamText \"f\"}")
                                                     ,("param"
                                                      ,String "fileo")
                                                      ,("expected",String "PtFile")])}))

  -- -----------------------------------

  describe "async dispatcher operation" $ do

    it "receives replies out of order" $ do
      chan <- atomically newTChan
      chSync <- atomically newTChan
      let req1 = IdeRequest "cmdasync1" Map.empty
          req2 = IdeRequest "cmdasync2" Map.empty
          cr1 = CReq "test" 1 req1 chan
          cr2 = CReq "test" 2 req2 chan
      r1 <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr1)
      r2 <- runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (doDispatch (testPlugins chSync) cr2)
      r1 `shouldBe` Nothing
      r2 `shouldBe` Nothing
      rc1 <- atomically $ readTChan chan
      rc2 <- atomically $ readTChan chan
      rc1 `shouldBe` (CResp { couPlugin = "test"
                            , coutReqId = 2
                            , coutResp = IdeResponseOk (String "asyncCmd2 sending strobe")})
      rc2 `shouldBe` (CResp { couPlugin = "test"
                            , coutReqId = 1
                            , coutResp = IdeResponseOk (String "asyncCmd1 got strobe")})

  describe "New plugin dispatcher operation" $ do
    it "dispatches response correctly" $ do
      inChan <- atomically newTChan
      outChan <- atomically newTChan
      cancelTVar <- newTVarIO S.empty
      wipTVar <- newTVarIO S.empty
      versionTVar <- newTVarIO $ Map.singleton (filePathToUri "test") 3
      let req1 = PReq Nothing (Just $ J.IdInt 1) (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text1"
          req2 = PReq Nothing (Just $ J.IdInt 2) (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text2"
          req3 = PReq (Just (filePathToUri "test", 2)) Nothing (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text3"
          req4 = PReq Nothing (Just $ J.IdInt 3) (atomically . writeTChan outChan) $ return $ IdeResponseOk $ T.pack "text4"
      pid <- forkIO $ runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (dispatcherP (DispatcherEnv cancelTVar wipTVar versionTVar) inChan)
      atomically $ writeTChan inChan req1
      atomically $ modifyTVar cancelTVar (S.insert (J.IdInt 2))
      atomically $ writeTChan inChan req2
      atomically $ writeTChan inChan req3
      atomically $ writeTChan inChan req4
      resp1 <- atomically $ readTChan outChan
      resp2 <- atomically $ readTChan outChan
      killThread pid
      resp1 `shouldBe` IdeResponseOk "text1"
      resp2 `shouldBe` IdeResponseOk "text4"

    it "dispatches async command correctly" $ do
      outChan <- atomically newTChan
      cont <- newEmptyMVar
      let myAsyncCmd :: IdeM (Async T.Text)
          myAsyncCmd = makeAsync $ putMVar cont ()  >> return (IdeResponseOk "text2")
          myCallback :: Async T.Text -> IO ()
          myCallback f = f $ atomically . writeTChan outChan
          req = PReq Nothing Nothing myCallback myAsyncCmd
          delayt = 300*1000
      inChan <- atomically newTChan
      cancelTVar <- newTVarIO S.empty
      wipTVar <- newTVarIO S.empty
      versionTVar <- newTVarIO Map.empty
      pid <- forkIO $ runIdeM testOptions (IdeState Map.empty Map.empty Map.empty) (dispatcherP (DispatcherEnv cancelTVar wipTVar versionTVar) inChan)
      atomically $ writeTChan inChan req
      atomically $ writeTChan outChan $ IdeResponseOk "text1"
      _ <- takeMVar cont
      atomically $ writeTChan outChan $ IdeResponseOk "text3"
      resp1 <- atomically $ readTChan outChan
      resp2 <- atomically $ readTChan outChan
      resp3 <- atomically $ readTChan outChan
      killThread pid
      resp1 `shouldBe` IdeResponseOk "text1"
      resp2 `shouldBe` IdeResponseOk "text2"
      resp3 `shouldBe` IdeResponseOk "text3"

-- ---------------------------------------------------------------------

testPlugins :: TChan () -> Plugins
testPlugins chSync = Map.fromList [("test",testDescriptor chSync)]

testDescriptor :: TChan () -> UntaggedPluginDescriptor
testDescriptor chSync = PluginDescriptor
  {
    pdUIShortName = "testDescriptor"
  , pdUIOverview = "PluginDescriptor for testing Dispatcher"
  , pdCommands =
      [
        mkCmdWithContext "cmd1" [CtxNone] []
      , mkCmdWithContext "cmd2" [CtxFile] []
      , mkCmdWithContext "cmd3" [CtxPoint] []
      , mkCmdWithContext "cmd4" [CtxRegion] []
      , mkCmdWithContext "cmd5" [CtxCabalTarget] []
      , mkCmdWithContext "cmd6" [CtxProject] []
      , mkCmdWithContext "cmdmultiple" [CtxFile,CtxPoint,CtxRegion] []

      , mkCmdWithContext "cmdextra" [CtxFile] [ RP "txt"  "help" PtText
                                              , RP "file" "help" PtFile
                                              , RP "pos"  "help" PtPos
                                              , RP "int" "help" PtInt
                                              , RP "bool" "help" PtBool
                                              , RP "range" "help" PtRange
                                              , RP "loc" "help" PtLoc
                                              , RP "textDocId" "help" PtTextDocId
                                              , RP "textDocPos" "help" PtTextDocPos
                                              ]

      , mkCmdWithContext "cmdoptional" [CtxNone] [ RP "txt"   "help" PtText
                                                 , OP "txto"  "help" PtText
                                                 , OP "fileo" "help" PtFile
                                                 , OP "poso"  "help" PtPos
                                                 ]
      , mkAsyncCmdWithContext (asyncCmd1 chSync) "cmdasync1" [CtxNone] []
      , mkAsyncCmdWithContext (asyncCmd2 chSync) "cmdasync2" [CtxNone] []
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

mkCmdWithContext :: CommandName -> [AcceptedContext] -> [ParamDescription] -> UntaggedCommand
mkCmdWithContext n cts pds =
        Command
          { cmdDesc = CommandDesc
                        { cmdName = n
                        , cmdUiDescription = "description"
                        , cmdFileExtensions = []
                        , cmdContexts = cts
                        , cmdAdditionalParams = pds
                        , cmdReturnType = "Text"
                        , cmdSave = SaveNone
                        }
          , cmdFunc = CmdSync $ \ctxs _ -> return (IdeResponseOk (T.pack $ "result:ctxs=" ++ show ctxs))
          }

mkAsyncCmdWithContext :: (ValidResponse a) => CommandFunc a -> CommandName -> [AcceptedContext] -> [ParamDescription] -> UntaggedCommand
mkAsyncCmdWithContext cf n cts pds =
  Command {cmdDesc =
             CommandDesc { cmdName = n
                         , cmdUiDescription = "description"
                         , cmdFileExtensions = []
                         , cmdContexts = cts
                         , cmdAdditionalParams = pds
                         , cmdReturnType = "Text"
                         , cmdSave = SaveNone
                         }
          ,cmdFunc = cf}

-- ---------------------------------------------------------------------

asyncCmd1 :: TChan () -> CommandFunc T.Text
asyncCmd1 ch = CmdAsync $ \f _ctxs _ -> do
  _ <- liftIO $ forkIO $ do
    _synced <- atomically $ readTChan ch
    logm $ "asyncCmd1 got val"
    f (IdeResponseOk "asyncCmd1 got strobe")
  return ()

asyncCmd2 :: TChan () -> CommandFunc T.Text
asyncCmd2 ch  = CmdAsync $ \f _ctxs _ -> do
  _ <- liftIO $ forkIO $ do
    f (IdeResponseOk "asyncCmd2 sending strobe")
    atomically $ writeTChan ch ()
  return ()

-- ---------------------------------------------------------------------
