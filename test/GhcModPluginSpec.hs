{-# LANGUAGE OverloadedStrings #-}
module GhcModPluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Control.Exception
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadFunctions
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.GhcModPlugin
import           System.Directory
import qualified Data.Map as Map

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ghc-mod plugin" ghcmodSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

testPlugins :: Plugins
testPlugins = Map.fromList [("ghcmod",ghcmodDescriptor)]

-- TODO: break this out into a TestUtils file
dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "ghcmod" 1 req testChan
  r <- withStdoutLogging $ runIdeM (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)
  return r

-- ---------------------------------------------------------------------

ghcmodSpec :: Spec
ghcmodSpec = do
  describe "ghc-mod plugin commands" $ do
    it "runs the check command" $ do
      let req = IdeRequest "check" (Map.fromList [("file", ParamFileP "./test/testdata/FileWithWarning.hs")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk (H.fromList ["ok" .= ("test/testdata/FileWithWarning.hs:4:7:Not in scope: \8216x\8217\n"::String)]))


    -- ---------------------------------

    it "runs the lint command" $ do
      let req = IdeRequest "lint" (Map.fromList [("file", ParamFileP "./test/testdata/FileWithWarning.hs")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk (H.fromList ["ok" .= ("./test/testdata/FileWithWarning.hs:6:9: Error: Redundant do\NULFound:\NUL  do return (3 + x)\NULWhy not:\NUL  return (3 + x)\n"::String)]))


    -- ---------------------------------

    it "runs the find command" $ do
      let req = IdeRequest "find" (Map.fromList [("dir", ParamFileP "."),("symbol", ParamTextP "Show")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk (H.fromList ["modules" .= ["GHC.Show"::String,"Prelude","Test.Hspec.Discover","Text.Show"]]))


    -- ---------------------------------

    it "runs the info command" $ do
      let req = IdeRequest "info" (Map.fromList [("file", ParamFileP "./test/testdata/HaReRename.hs"),("expr", ParamTextP "main")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk (H.fromList ["ok" .= ("main :: IO () \t-- Defined at test/testdata/HaReRename.hs:2:1\n"::String)]))


    -- ---------------------------------

    it "runs the type command, incorrect params" $ do
      let req = IdeRequest "type" (Map.fromList [("file", ParamFileP "./test/testdata/FileWithWarning.hs")])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseFail (IdeError {ideCode = MissingParameter, ideMessage = "need `start_pos` parameter", ideInfo = String "start_pos"}))

    -- ---------------------------------

    it "runs the type command, correct params" $ do
      let req = IdeRequest "type" (Map.fromList [("file", ParamFileP "./test/testdata/HaReRename.hs")
                                                 ,("start_pos", ParamPosP (5,9))])
      r <- dispatchRequest req
      r `shouldBe` Just (IdeResponseOk (H.fromList ["type_info".=toJSON
                        [TypeResult (5,9) (5,10) "Int"
                        ,TypeResult (5,9) (5,14) "Int"
                        ,TypeResult (5,1) (5,14) "Int -> Int"]
                        ]))

    it "runs the type command with an absolute path from another folder, correct params" $ do
      fp <- makeAbsolute "./test/testdata/HaReRename.hs"
      cd <- getCurrentDirectory
      cd2 <- getHomeDirectory
      bracket (setCurrentDirectory cd2)
              (\_->setCurrentDirectory cd)
              $ \_-> do
        let req = IdeRequest "type" (Map.fromList [("file", ParamFileP $ T.pack fp)
                                                   ,("start_pos", ParamPosP (5,9))])
        r <- dispatchRequest req
        r `shouldBe` Just (IdeResponseOk (H.fromList ["type_info".=toJSON
                          [TypeResult (5,9) (5,10) "Int"
                          ,TypeResult (5,9) (5,14) "Int"
                          ,TypeResult (5,1) (5,14) "Int -> Int"]
                          ]))
    -- ---------------------------------
