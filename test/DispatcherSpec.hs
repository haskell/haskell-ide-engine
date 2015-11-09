{-# LANGUAGE OverloadedStrings #-}
module DispatcherSpec where

import           Control.Concurrent
import           Control.Logging
import           Data.Aeson
import qualified Data.Text as T
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
      (show r) `shouldBe` "IdeResponseOk (String \"result:ctxs=[CtxNone]\")"

    -- ---------------------------------

    it "identifies bad CtxFile" $ do
      chan <- newChan
      let req = IdeRequest "cmd2" (Map.fromList [])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseFail (String \"missing parameter '\\\"file\\\"'\")"

    -- ---------------------------------

    it "identifies CtxFile" $ do
      chan <- newChan
      let req = IdeRequest "cmd2" (Map.fromList [("file",ParamFile "foo.hs")])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseOk (String \"result:ctxs=[CtxFile]\")"

    -- ---------------------------------

    it "identifies CtxPoint" $ do
      chan <- newChan
      let req = IdeRequest "cmd3" (Map.fromList [("file",ParamFile "foo.hs"),("start_pos",ParamPos (1,2))])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseOk (String \"result:ctxs=[CtxPoint]\")"

    -- ---------------------------------

    it "identifies CtxRegion" $ do
      chan <- newChan
      let req = IdeRequest "cmd4" (Map.fromList [("file",ParamFile "foo.hs")
                                                ,("start_pos",ParamPos (1,2))
                                                ,("end_pos",ParamPos (3,4))])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseOk (String \"result:ctxs=[CtxRegion]\")"

    -- ---------------------------------

    it "identifies CtxCabal" $ do
      chan <- newChan
      let req = IdeRequest "cmd5" (Map.fromList [("cabal",ParamText "lib")])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseOk (String \"result:ctxs=[CtxCabalTarget]\")"

    -- ---------------------------------

    it "identifies CtxProject" $ do
      chan <- newChan
      let req = IdeRequest "cmd6" (Map.fromList [])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseOk (String \"result:ctxs=[CtxProject]\")"


    -- ---------------------------------

    it "identifies all multiple" $ do
      chan <- newChan
      let req = IdeRequest "cmdmultiple" (Map.fromList [("file",ParamFile "foo.hs")
                                                       ,("start_pos",ParamPos (1,2))
                                                       ,("end_pos",ParamPos (3,4))])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseOk (String \"result:ctxs=[CtxFile,CtxPoint,CtxRegion]\")"


    -- ---------------------------------

    it "identifies CtxFile,CtxPoint multiple" $ do
      chan <- newChan
      let req = IdeRequest "cmdmultiple" (Map.fromList [("file",ParamFile "foo.hs")
                                                       ,("start_pos",ParamPos (1,2))])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseOk (String \"result:ctxs=[CtxFile,CtxPoint]\")"

    -- ---------------------------------

    it "identifies error when no match multiple" $ do
      chan <- newChan
      let req = IdeRequest "cmdmultiple" (Map.fromList [("cabal",ParamText "lib")])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseFail (String \"missing parameter '\\\"file\\\"'\")"

  -- -----------------------------------

  describe "checking extra params" $ do
    -- ---------------------------------

    it "identifies matching params" $ do
      chan <- newChan
      let req = IdeRequest "cmdextra" (Map.fromList [("file",ParamFile "foo.hs")
                                                    ,("txt", ParamText "a")
                                                    ,("file",ParamFile "f")
                                                    ,("pos", ParamPos (1,2))
                                                    ])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseOk (String \"result:ctxs=[CtxFile]\")"

    -- ---------------------------------

    it "reports mismatched param" $ do
      chan <- newChan
      let req = IdeRequest "cmdextra" (Map.fromList [("file",ParamFile "foo.hs")
                                                    ,("txt", ParamFile "a")
                                                    ,("file",ParamFile "f")
                                                    ,("pos", ParamPos (1,2))
                                                    ])
          cr = CReq "test" 1 req chan
      r <- withStdoutLogging $ runIdeM (IdeState Map.empty) (doDispatch testPlugins cr)
      (show r) `shouldBe` "IdeResponseFail (String \"parameter type mismatch, expected PtText but got ParamFile \\\"a\\\"\")"

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

      ]
  , pdExposedServices = []
  , pdUsedServices    = []
  }

mkCmdWithContext :: CommandName -> [AcceptedContext] -> [ParamDecription] -> Command
mkCmdWithContext n cts pds =
        Command
          { cmdDesc = CommandDesc
                        { cmdName = n
                        , cmdUiDescription = "description"
                        , cmdFileExtensions = []
                        , cmdContexts = cts
                        , cmdAdditionalParams = pds
                        }
          , cmdFunc = \ctxs _ -> return (IdeResponseOk (String $ T.pack $ "result:ctxs=" ++ show ctxs))
          }

-- ---------------------------------------------------------------------
