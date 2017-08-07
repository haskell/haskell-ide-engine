{-# LANGUAGE OverloadedStrings #-}
module GhcModPluginSpec where

import           Control.Concurrent
import           Control.Exception
import           Data.Aeson
import qualified Data.Map                            as Map
import qualified Data.Set                            as S
import qualified GhcMod.ModuleLoader                 as GM
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.GhcModPlugin
import           System.Directory
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ghc-mod plugin" ghcmodSpec

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [("ghcmod",ghcmodDescriptor)]

dispatchRequest :: ToJSON a => PluginId -> CommandName -> a -> IO (IdeResponse Value)
dispatchRequest plugin com arg = do
  mv <- newEmptyMVar
  dispatchRequestP $ runPluginCommand plugin com (toJSON arg) (putMVar mv)
  takeMVar mv

dispatchRequestP :: IdeM a -> IO a
dispatchRequestP =
  cdAndDo "./test/testdata" .
    runIdeM testOptions (IdeState testPlugins GM.emptyModuleCache)

dispatchRequestNoCd :: ToJSON a => PluginId -> CommandName -> a -> IO (IdeResponse Value)
dispatchRequestNoCd plugin com arg = do
  mv <- newEmptyMVar
  dispatchRequestPNoCd $ runPluginCommand plugin com (toJSON arg) (putMVar mv)
  takeMVar mv

dispatchRequestPNoCd :: IdeM a -> IO a
dispatchRequestPNoCd =
    runIdeM testOptions (IdeState testPlugins GM.emptyModuleCache)

-- ---------------------------------------------------------------------

ghcmodSpec :: Spec
ghcmodSpec = do
  describe "ghc-mod plugin commands(old plugin api)" $ do
    it "runs the check command" $ cdAndDo "./test/testdata" $ do
      fp <- makeAbsolute "./FileWithWarning.hs"
      let req = filePathToUri fp
          res = Diagnostic (Range (toPos (4,7))
                                  (toPos (4,8)))
                                  (Just DsError)
                                  Nothing
                                  (Just "ghcmod")
                                  "Variable not in scope: x"

      r <- dispatchRequestNoCd "ghcmod" "check" req
      r `shouldBe`
        (IdeResponseOk
           $ (toJSON :: (Diagnostics, AdditionalErrs) -> Value)
              (Map.singleton (filePathToUri fp) (S.singleton res), []))
    -- ---------------------------------

    it "runs the lint command" $ do
      let req = filePathToUri "./FileWithWarning.hs"
      r <- dispatchRequest "ghcmod" "lint" req
      r `shouldBe` (IdeResponseOk (String "./FileWithWarning.hs:6:9: Warning: Redundant do\NULFound:\NUL  do return (3 + x)\NULWhy not:\NUL  return (3 + x)\n"))


    -- ---------------------------------

    it "runs the info command" $ do
      let req = IP (filePathToUri "HaReRename.hs") "main"
      -- ghc-mod tries to load the test file in the context of the hie project if we do not cd first.
      r <- dispatchRequest "ghcmod" "info" req
      r `shouldBe` (IdeResponseOk (String "main :: IO () \t-- Defined at HaReRename.hs:2:1\n"))


    -- ---------------------------------

    it "runs the type command" $ do
      let req = TP False (filePathToUri "HaReRename.hs") (toPos (5,9))
      r <- dispatchRequest "ghcmod" "type" req
      r `shouldBe` (IdeResponseOk ((toJSON :: [(Range, String)] -> Value)
                        [(Range (toPos (5,9)) (toPos (5,10)), "Int")
                        ,(Range (toPos (5,9)) (toPos (5,14)), "Int")
                        ,(Range (toPos (5,1)) (toPos (5,14)), "Int -> Int")
                        ]))

    it "runs the type command with an absolute path from another folder, correct params" $ do
      fp <- makeAbsolute "./test/testdata/HaReRename.hs"
      cd <- getCurrentDirectory
      cd2 <- getHomeDirectory
      bracket (setCurrentDirectory cd2)
              (\_->setCurrentDirectory cd)
              $ \_-> do
        let req = TP False (filePathToUri fp) (toPos (5,9))
        r <- dispatchRequestNoCd "ghcmod" "type" req
        r `shouldBe` (IdeResponseOk ((toJSON :: [(Range,String)] -> Value)
                          [(Range (toPos (5,9)) (toPos (5,10)), "Int")
                          ,(Range (toPos (5,9)) (toPos (5,14)), "Int")
                          ,(Range (toPos (5,1)) (toPos (5,14)), "Int -> Int")
                          ]))
    -- ---------------------------------

  describe "ghc-mod plugin commands(new plugin api)" $ do
    it "runs the check command" $ cdAndDo "./test/testdata" $ do
      fp <- makeAbsolute "./FileWithWarning.hs"
      let req = setTypecheckedModule (filePathToUri fp)
      r <- dispatchRequestPNoCd req
      let res = Diagnostic (Range (toPos (4,7))
                                  (toPos (4,8)))
                                  (Just DsError)
                                  Nothing
                                  (Just "ghcmod")
                                  "Variable not in scope: x"

      r `shouldBe`
        (IdeResponseOk
           (Map.singleton (filePathToUri fp) (S.singleton res), []))

    -- ---------------------------------

    it "runs the lint command" $ do
      let req = lintCmd' (filePathToUri "./FileWithWarning.hs")
      r <- dispatchRequestP req
      r `shouldBe` IdeResponseOk "./FileWithWarning.hs:6:9: Warning: Redundant do\NULFound:\NUL  do return (3 + x)\NULWhy not:\NUL  return (3 + x)\n"


    -- ---------------------------------

    it "runs the info command" $ do
      let req = infoCmd' (filePathToUri "HaReRename.hs") "main"
      -- ghc-mod tries to load the test file in the context of the hie project if we do not cd first.
      r <- dispatchRequestP req
      r `shouldBe` IdeResponseOk "main :: IO () \t-- Defined at HaReRename.hs:2:1\n"


    -- ---------------------------------

    it "runs the type command, correct params" $ do
      let uri = filePathToUri "HaReRename.hs"
      let req = newTypeCmd False uri (toPos (5,9))
      r <- dispatchRequestP $ setTypecheckedModule uri >> req
      r `shouldBe` (IdeResponseOk (
                        [(Range (toPos (5,9)) (toPos (5,10)), "Int")
                        ,(Range (toPos (5,9)) (toPos (5,14)), "Int")
                        ,(Range (toPos (5,1)) (toPos (5,14)), "Int -> Int")
                        ]))

    it "runs the type command with an absolute path from another folder, correct params" $ do
      fp <- makeAbsolute "./test/testdata/HaReRename.hs"
      cd <- getCurrentDirectory
      cd2 <- getHomeDirectory
      bracket (setCurrentDirectory cd2)
              (\_->setCurrentDirectory cd)
              $ \_-> do
        let req = newTypeCmd False (filePathToUri fp) (toPos (5,9))
        r <- dispatchRequestPNoCd $ setTypecheckedModule (filePathToUri fp) >> req
        r `shouldBe` (IdeResponseOk (
                          [(Range (toPos (5,9)) (toPos (5,10)), "Int")
                          ,(Range (toPos (5,9)) (toPos (5,14)), "Int")
                          ,(Range (toPos (5,1)) (toPos (5,14)), "Int -> Int")
                          ]))
