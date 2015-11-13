{-# LANGUAGE OverloadedStrings #-}
module DispatcherSpec where

import           Control.Concurrent
import           Control.Logging
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.Types

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
    -- ---------------------------------

    it "identifies CtxNone" $ do
      chan <- newChan
      let req = IdeRequest "cmd1" (Map.fromList [])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxNone]"::String)])

    -- ---------------------------------

    it "identifies bad CtxFile" $ do
      chan <- newChan
      let req = IdeRequest "cmd2" (Map.fromList [])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseFail (IdeError {ideCode = MissingParameter, ideMessage = "need `file` parameter", ideInfo = Just (String "file")})

    -- ---------------------------------

    it "identifies CtxFile" $ do
      chan <- newChan
      let req = IdeRequest "cmd2" (Map.fromList [("file", ParamFileP "foo.hs")])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxFile]"::String)])

    -- ---------------------------------

    it "identifies CtxPoint" $ do
      chan <- newChan
      let req = IdeRequest "cmd3" (Map.fromList [("file", ParamFileP "foo.hs"),("start_pos", ParamPosP (1,2))])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxPoint]"::String)])

    -- ---------------------------------

    it "identifies CtxRegion" $ do
      chan <- newChan
      let req = IdeRequest "cmd4" (Map.fromList [("file", ParamFileP "foo.hs")
                                                ,("start_pos", ParamPosP (1,2))
                                                ,("end_pos", ParamPosP (3,4))])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxRegion]"::String)])

    -- ---------------------------------

    it "identifies CtxCabal" $ do
      chan <- newChan
      let req = IdeRequest "cmd5" (Map.fromList [("cabal", ParamTextP "lib")])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxCabalTarget]"::String)])

    -- ---------------------------------

    it "identifies CtxProject" $ do
      chan <- newChan
      let req = IdeRequest "cmd6" (Map.fromList [])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxProject]"::String)])

    -- ---------------------------------

    it "identifies all multiple" $ do
      chan <- newChan
      let req = IdeRequest "cmdmultiple" (Map.fromList [("file", ParamFileP "foo.hs")
                                                       ,("start_pos", ParamPosP (1,2))
                                                       ,("end_pos", ParamPosP (3,4))])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxFile,CtxPoint,CtxRegion]"::String)])

    -- ---------------------------------

    it "identifies CtxFile,CtxPoint multiple" $ do
      chan <- newChan
      let req = IdeRequest "cmdmultiple" (Map.fromList [("file", ParamFileP "foo.hs")
                                                       ,("start_pos", ParamPosP (1,2))])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxFile,CtxPoint]"::String)])
    -- ---------------------------------

    it "identifies error when no match multiple" $ do
      chan <- newChan
      let req = IdeRequest "cmdmultiple" (Map.fromList [("cabal", ParamTextP "lib")])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe`
        "IdeResponseFail (IdeError {ideCode = MissingParameter, ideMessage = \"need `file` parameter\", ideInfo = Just (String \"file\")})"

  -- -----------------------------------

  describe "checking extra params" $ do
    -- ---------------------------------

    it "identifies matching params" $ do
      chan <- newChan
      let req = IdeRequest "cmdextra" (Map.fromList [("file", ParamFileP "foo.hs")
                                                    ,("txt",  ParamTextP "a")
                                                    ,("file", ParamFileP "f")
                                                    ,("pos",  ParamPosP (1,2))
                                                    ])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxFile]"::String)])
    -- ---------------------------------

    it "reports mismatched param" $ do
      chan <- newChan
      let req = IdeRequest "cmdextra" (Map.fromList [("file", ParamFileP "foo.hs")
                                                    ,("txt",  ParamFileP "a")
                                                    ,("file", ParamFileP "f")
                                                    ,("pos",  ParamPosP (1,2))
                                                    ])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe`
        "IdeResponseFail (IdeError {ideCode = IncorrectParameterType, ideMessage = \"got wrong parameter type for `txt`, expected: PtText , got:ParamValP {unParamValP = ParamFile \\\"a\\\"}\", ideInfo = Just (Object (fromList [(\"value\",String \"ParamValP {unParamValP = ParamFile \\\"a\\\"}\"),(\"param\",String \"txt\"),(\"expected\",String \"PtText\")]))})"



    -- ---------------------------------

    it "reports matched optional param" $ do
      chan <- newChan
      let req = IdeRequest "cmdoptional" (Map.fromList [("txt",   ParamTextP "a")
                                                       ,("fileo", ParamFileP "f")
                                                       ,("poso",  ParamPosP (1,2))
                                                       ])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      r `shouldBe` IdeResponseOk (H.fromList ["ok" .= ("result:ctxs=[CtxNone]"::String)])
      
    -- ---------------------------------

    it "reports mismatched optional param" $ do
      chan <- newChan
      let req = IdeRequest "cmdoptional" (Map.fromList [("txt",   ParamTextP "a")
                                                       ,("fileo", ParamTextP "f")
                                                       ,("poso",  ParamPosP (1,2))
                                                       ])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe`
        "IdeResponseFail (IdeError {ideCode = IncorrectParameterType, ideMessage = \"got wrong parameter type for `fileo`, expected: PtFile , got:ParamValP {unParamValP = ParamText \\\"f\\\"}\", ideInfo = Just (Object (fromList [(\"value\",String \"ParamValP {unParamValP = ParamText \\\"f\\\"}\"),(\"param\",String \"fileo\"),(\"expected\",String \"PtFile\")]))})"

-- ---------------------------------------------------------------------

testPlugins :: Plugins
testPlugins = Map.fromList [("test",testDescriptor)]

testDescriptor :: PluginDescriptor
testDescriptor = PluginDescriptor
  {
    pdCommands =
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
                                              ]

      , mkCmdWithContext "cmdoptional" [CtxNone] [ RP "txt"   "help" PtText
                                                 , OP "txto"  "help" PtText
                                                 , OP "fileo" "help" PtFile
                                                 , OP "poso"  "help" PtPos
                                                 ]
      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

mkCmdWithContext :: CommandName -> [AcceptedContext] -> [ParamDescription] -> Command
mkCmdWithContext n cts pds =
        Command
          { cmdDesc = CommandDesc
                        { cmdName = n
                        , cmdUiDescription = "description"
                        , cmdFileExtensions = []
                        , cmdContexts = cts
                        , cmdAdditionalParams = pds
                        }
          , cmdFunc = \ctxs _ -> return (IdeResponseOk ("result:ctxs=" ++ show ctxs))
          }

-- ---------------------------------------------------------------------
