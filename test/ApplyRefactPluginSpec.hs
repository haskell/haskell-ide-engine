{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module ApplyRefactPluginSpec where

import           Control.Concurrent
import           Data.Aeson
import qualified Data.HashMap.Strict                   as H
import qualified GhcMod.ModuleLoader                   as GM
import           Haskell.Ide.ApplyRefactPlugin
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.PluginUtils
import           Language.Haskell.LSP.TH.DataTypesJSON
import           System.Directory
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "apply-refact plugin" applyRefactSpec

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [("applyrefact",applyRefactDescriptor)]

dispatchRequest :: ToJSON a => PluginId -> CommandName -> a -> IO (IdeResponse Value)
dispatchRequest plugin com arg = do
  mv <- newEmptyMVar
  dispatchRequestP $ runPluginCommand plugin com (toJSON arg) (putMVar mv)
  takeMVar mv

dispatchRequestP :: IdeM a -> IO a
dispatchRequestP = runIdeM testOptions (IdeState testPlugins GM.emptyModuleCache)

-- ---------------------------------------------------------------------

applyRefactSpec :: Spec
applyRefactSpec = do
  describe "apply-refact plugin commands(old plugin api)" $ do
    applyRefactPath  <- runIO $ filePathToUri <$> makeAbsolute "./test/testdata/ApplyRefact.hs"

    -- ---------------------------------

    it "applies one hint only" $ do

      let req = AOP (filePathToUri "./test/testdata/ApplyRefact.hs") (toPos (2,8))
      r <- dispatchRequest "applyrefact" "applyOne" req
      r `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
           (Just $ H.singleton applyRefactPath
                               $ List [TextEdit (Range (Position 1 0) (Position 1 25))
                                         "main = putStrLn \"hello\""])
           Nothing)

    -- ---------------------------------

    it "applies all hints" $ do

      let req = filePathToUri "./test/testdata/ApplyRefact.hs"
      r <- dispatchRequest "applyrefact" "applyAll" req
      r `shouldBe`
        (IdeResponseOk
         $ toJSON
         $ WorkspaceEdit
           (Just
             $ H.singleton applyRefactPath
                         $ List [TextEdit (Range (Position 1 0) (Position 1 25))
                                   "main = putStrLn \"hello\""
                                ,TextEdit (Range (Position 3 0) (Position 3 15))
                                   "foo x = x + 1"])
           Nothing)

    -- ---------------------------------

    it "returns hints as diagnostics" $ do

      let req = filePathToUri "./test/testdata/ApplyRefact.hs"
      r <- dispatchRequest "applyrefact" "lint" req
      r `shouldBe`
        (IdeResponseOk (toJSON (PublishDiagnosticsParams
                                 { _uri = filePathToUri "./test/testdata/ApplyRefact.hs"
                                 , _diagnostics = List
                                   [ Diagnostic (Range (Position 1 7) (Position 1 25))
                                                (Just DsHint)
                                                Nothing
                                                (Just "hlint")
                                                "Redundant bracket\nFound:\n  (putStrLn \"hello\")\nWhy not:\n  putStrLn \"hello\"\n"
                                   , Diagnostic (Range (Position 3 8) (Position 3 15))
                                                (Just DsHint)
                                                Nothing
                                                (Just "hlint")
                                                "Redundant bracket\nFound:\n  (x + 1)\nWhy not:\n  x + 1\n"
                                   ]
                                 }
                                )))

    -- ---------------------------------
  describe "apply-refact plugin commands(new plugin api)" $ do
    applyRefactPath  <- runIO $ filePathToUri <$> makeAbsolute "./test/testdata/ApplyRefact.hs"

    -- ---------------------------------

    it "applies one hint only" $ do

      let req = applyOneCmd' (filePathToUri "./test/testdata/ApplyRefact.hs")
                             (toPos (2,8))
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
         $ WorkspaceEdit
           (Just $ H.singleton applyRefactPath
                               $ List [TextEdit (Range (Position 1 0) (Position 1 25))
                                         "main = putStrLn \"hello\""])
           Nothing)

    -- ---------------------------------

    it "applies all hints" $ do

      let req = applyAllCmd' (filePathToUri "./test/testdata/ApplyRefact.hs")
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
         $ WorkspaceEdit
           (Just
             $ H.singleton applyRefactPath
                         $ List [TextEdit (Range (Position 1 0) (Position 1 25))
                                   "main = putStrLn \"hello\""
                                ,TextEdit (Range (Position 3 0) (Position 3 15))
                                   "foo x = x + 1"])
           Nothing)

    -- ---------------------------------

    it "returns hints as diagnostics" $ do

      let req = lintCmd' (filePathToUri "./test/testdata/ApplyRefact.hs")
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
           (PublishDiagnosticsParams
            { _uri = filePathToUri "./test/testdata/ApplyRefact.hs"
            , _diagnostics = List $
              [ Diagnostic (Range (Position 1 7) (Position 1 25))
                           (Just DsHint)
                           Nothing
                           (Just "hlint")
                           "Redundant bracket\nFound:\n  (putStrLn \"hello\")\nWhy not:\n  putStrLn \"hello\"\n"
              , Diagnostic (Range (Position 3 8) (Position 3 15))
                           (Just DsHint)
                           Nothing
                           (Just "hlint")
                           "Redundant bracket\nFound:\n  (x + 1)\nWhy not:\n  x + 1\n"
              ]
            }
           ))

    -- ---------------------------------

    it "respects hlint pragmas in the source file" $ do

      let req = lintCmd' (filePathToUri "./test/testdata/HlintPragma.hs")
      r <- dispatchRequestP req
      r `shouldBe`
        (IdeResponseOk
           (PublishDiagnosticsParams
            { _uri = filePathToUri "./test/testdata/HlintPragma.hs"
            , _diagnostics = List []
            }
           ))
