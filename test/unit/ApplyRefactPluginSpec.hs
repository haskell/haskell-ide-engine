{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module ApplyRefactPluginSpec where

import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict                   as H
import qualified Data.Text                             as T
import           Haskell.Ide.Engine.Plugin.ApplyRefact
import           Haskell.Ide.Engine.MonadTypes
import           Haskell.Ide.Engine.PluginUtils
import           System.Directory
import           TestUtils

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"       :: String) #-}

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "apply-refact plugin" applyRefactSpec

-- ---------------------------------------------------------------------

testPlugins :: IdePlugins
testPlugins = pluginDescToIdePlugins [applyRefactDescriptor "applyrefact"]

-- ---------------------------------------------------------------------

applyRefactSpec :: Spec
applyRefactSpec = do
  describe "apply-refact plugin commands" $ do
    applyRefactPath  <- runIO $ filePathToUri <$> makeAbsolute "./test/testdata/ApplyRefact.hs"

    -- ---------------------------------

    it "applies one hint only" $ do

      let furi = applyRefactPath
          act = applyOneCmd' furi (OneHint (toPos (2,8)) "Redundant bracket")
          arg = AOP furi (toPos (2,8)) "Redundant bracket"
          textEdits = List [TextEdit (Range (Position 1 0) (Position 1 25)) "main = putStrLn \"hello\""]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton applyRefactPath textEdits)
            Nothing
      testCommand testPlugins act "applyrefact" "applyOne" arg res

    -- ---------------------------------

    it "applies all hints" $ do

      let act = applyAllCmd' arg
          arg = applyRefactPath
          textEdits = List [ TextEdit (Range (Position 1 0) (Position 1 25)) "main = putStrLn \"hello\""
                           , TextEdit (Range (Position 3 0) (Position 3 15)) "foo x = x + 1" ]
          res = IdeResultOk $ WorkspaceEdit
            (Just $ H.singleton applyRefactPath textEdits)
            Nothing
      testCommand testPlugins act "applyrefact" "applyAll" arg res

    -- ---------------------------------

    it "returns hints as diagnostics" $ do

      let act = lintCmd' arg
          arg = applyRefactPath
          res = IdeResultOk
            PublishDiagnosticsParams
             { _uri = applyRefactPath
             , _diagnostics = List $
               [ Diagnostic (Range (Position 1 7) (Position 1 25))
                            (Just DsHint)
                            (Just "Redundant bracket")
                            (Just "hlint")
                            "Redundant bracket\nFound:\n  (putStrLn \"hello\")\nWhy not:\n  putStrLn \"hello\"\n"
                            Nothing
               , Diagnostic (Range (Position 3 8) (Position 3 15))
                            (Just DsHint)
                            (Just "Redundant bracket")
                            (Just "hlint")
                            "Redundant bracket\nFound:\n  (x + 1)\nWhy not:\n  x + 1\n"
                            Nothing
               ]}
      liftIO $ putStrLn "returns hints as diagnostics:1"
      testCommand testPlugins act "applyrefact" "lint" arg res
      liftIO $ putStrLn "returns hints as diagnostics:2"

    -- ---------------------------------

    it "returns hlint parse error as DsInfo ignored diagnostic" $ do
      filePath  <- filePathToUri <$> makeAbsolute "./test/testdata/HlintParseFail.hs"

      let act = lintCmd' arg
          arg = filePath
          res = IdeResultOk
            PublishDiagnosticsParams
             { _uri = filePath
             , _diagnostics = List
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,2,2,0)))
               [Diagnostic {_range = Range { _start = Position {_line = 13, _character = 0}
                                           , _end = Position {_line = 13, _character = 100000}}
                           , _severity = Just DsInfo
                           , _code = Just "parser"
                           , _source = Just "hlint"
                           , _message = "Parse error: virtual }\n  data instance Sing (z :: (a :~: b)) where\n      SRefl :: Sing Refl +\n> \n\n"
                           , _relatedInformation = Nothing }]}
#else
               [Diagnostic {_range = Range { _start = Position {_line = 11, _character = 28}
                                           , _end = Position {_line = 11, _character = 100000}}
                           , _severity = Just DsInfo
                           , _code = Just "parser"
                           , _source = Just "hlint"
                           , _message = "Parse error: :~:\n  import           Data.Type.Equality            ((:~:) (..), (:~~:) (..))\n  \n> data instance Sing (z :: (a :~: b)) where\n      SRefl :: Sing Refl +\n\n"
                           , _relatedInformation = Nothing }]}
#endif
      liftIO $ putStrLn "returns hlint parse error as DsInfo ignored diagnostic:1"
      testCommand testPlugins act "applyrefact" "lint" arg res
      liftIO $ putStrLn "returns hlint parse error as DsInfo ignored diagnostic:2"

    -- ---------------------------------

    it "respects hlint pragmas in the source file" $ do
      filePath  <- filePathToUri <$> makeAbsolute "./test/testdata/HlintPragma.hs"

      let req = lintCmd' filePath
      r <- runIGM testPlugins req
      r `shouldBe`
        (IdeResultOk
           (PublishDiagnosticsParams
            { _uri = filePath
            , _diagnostics = List
              [ Diagnostic (Range (Position 3 11) (Position 3 20))
                           (Just DsInfo)
                           (Just "Redundant bracket")
                           (Just "hlint")
                           "Redundant bracket\nFound:\n  (\"hello\")\nWhy not:\n  \"hello\"\n"
                           Nothing
              ]
            }
           ))

    -- ---------------------------------

    it "respects hlint config files in project root dir" $ do
      filePath  <- filePathToUri <$> makeAbsolute "./test/testdata/HlintPragma.hs"

      let req = lintCmd' filePath
      r <- withCurrentDirectory "./test/testdata" $ runIGM testPlugins req
      r `shouldBe`
        (IdeResultOk
           (PublishDiagnosticsParams
            -- { _uri = filePathToUri "./HlintPragma.hs"
            { _uri = filePath
            , _diagnostics = List []
            }
           ))

    -- ---------------------------------

    it "reports error without crash" $ do
      filePath  <- filePathToUri <$> makeAbsolute "./test/testdata/ApplyRefactError.hs"

      let req = applyAllCmd' filePath
          isExpectedError (IdeResultFail (IdeError PluginError err _)) =
              "Illegal symbol '.' in type" `T.isInfixOf` err
          isExpectedError _ = False
      r <- withCurrentDirectory "./test/testdata" $ runIGM testPlugins req
      r `shouldSatisfy` isExpectedError
