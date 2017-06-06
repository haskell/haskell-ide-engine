{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields   #-}
module ApplyRefactPluginSpec where

import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.Aeson
import qualified Data.Map as Map
import           Haskell.Ide.Engine.Dispatcher
import           Haskell.Ide.Engine.Monad
import           Haskell.Ide.Engine.PluginDescriptor
import           Haskell.Ide.Engine.SemanticTypes
import           Haskell.Ide.Engine.Types
import           Haskell.Ide.ApplyRefactPlugin
import           Language.Haskell.LSP.TH.DataTypesJSON
import qualified Data.HashMap.Strict as H
import           TestUtils

import           Test.Hspec

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "apply-refact plugin" applyRefactSpec

-- -- |Used when running from ghci, and it sets the current directory to ./tests
-- tt :: IO ()
-- tt = do
--   cd ".."
--   hspec spec

-- ---------------------------------------------------------------------

testPlugins :: Plugins
testPlugins = Map.fromList [("applyrefact",untagPluginDescriptor applyRefactDescriptor)]

-- TODO: break this out into a TestUtils file
dispatchRequest :: IdeRequest -> IO (Maybe (IdeResponse Object))
dispatchRequest req = do
  testChan <- atomically newTChan
  let cr = CReq "applyrefact" 1 req testChan
  r <- runIdeM testOptions (IdeState Map.empty Map.empty) (doDispatch testPlugins cr)
  return r

-- ---------------------------------------------------------------------

applyRefactSpec :: Spec
applyRefactSpec = do
  describe "apply-refact plugin commands" $ do

    -- ---------------------------------
    
    it "applies one hint only" $ do

      let req = IdeRequest "applyOne" (Map.fromList [("file",ParamFileP $ filePathToUri "./test/testdata/ApplyRefact.hs")
                                                    ,("start_pos",ParamPosP (toPos (2,8)))
                                                    ])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk $ jsWrite $ WorkspaceEdit {_changes = Just (H.fromList [(Uri {getUri = "file://./test/testdata/ApplyRefact.hs"},List [TextEdit {_range = Range {_start = Position {_line = 1, _character = 0}, _end = Position {_line = 1, _character = 25}}, _newText = "main = putStrLn \"hello\""}])]), _documentChanges = Nothing})

    -- ---------------------------------

    it "applies all hints" $ do

      let req = IdeRequest "applyAll" (Map.fromList [("file",ParamFileP $ filePathToUri "./test/testdata/ApplyRefact.hs")
                                                    ])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk $ jsWrite $ WorkspaceEdit {_changes = Just (H.fromList [(Uri {getUri = "file://./test/testdata/ApplyRefact.hs"},List [TextEdit {_range = Range {_start = Position {_line = 1, _character = 0}, _end = Position {_line = 1, _character = 25}}, _newText = "main = putStrLn \"hello\""},TextEdit {_range = Range {_start = Position {_line = 3, _character = 0}, _end = Position {_line = 3, _character = 15}}, _newText = "foo x = x + 1"}])]), _documentChanges = Nothing})

    -- ---------------------------------

    it "returns hints as diagnostics" $ do

      let req = IdeRequest "lint" (Map.fromList [("file",ParamFileP $ filePathToUri "./test/testdata/ApplyRefact.hs")
                                                ])
      r <- dispatchRequest req
      r `shouldBe`
        Just (IdeResponseOk (jsWrite (FileDiagnostics
                                      { fdFileName = filePathToUri "./test/testdata/ApplyRefact.hs"
                                      , fdDiagnostics =
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
