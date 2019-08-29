{-# LANGUAGE OverloadedStrings #-}
module ContextSpec where


import           Test.Hspec

import           GHC                            ( tm_parsed_module )
import           System.Directory

import           Haskell.Ide.Engine.PluginApi
import           Haskell.Ide.Engine.PluginsIdeMonads
import           Haskell.Ide.Engine.PluginUtils
import           Haskell.Ide.Engine.ModuleCache
import           Haskell.Ide.Engine.Context

import           TestUtils

spec :: Spec
spec = describe "Context of different cursor positions" $ do
    it "can set the module as type checked"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp <- makeAbsolute "./ExampleContext.hs"
              let arg = filePathToUri fp
              let res = IdeResultOk (Nothing :: Maybe Context)
              actual <- runSingle (IdePlugins mempty) $ do
                  _ <- setTypecheckedModule arg
                  return $ IdeResultOk Nothing

              actual `shouldBe` res

    it "module header context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp_ <- makeAbsolute "./ExampleContext.hs"
              let res = IdeResultOk (Just (ModuleContext "ExampleContext"))

              actual <- getContextAt fp_ (toPos (1, 10))

              actual `shouldBe` res


    it "module export list context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp_ <- makeAbsolute "./ExampleContext.hs"
              let res = IdeResultOk (Just ExportContext)
              actual <- getContextAt fp_ (toPos (1, 24))

              actual `shouldBe` res

    it "value context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp_ <- makeAbsolute "./ExampleContext.hs"
        let res = IdeResultOk (Just ValueContext)
        actual <- getContextAt fp_ (toPos (7, 6))

        actual `shouldBe` res

    it "value context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp_ <- makeAbsolute "./ExampleContext.hs"
        let res = IdeResultOk (Just ValueContext)
        actual <- getContextAt fp_ (toPos (7, 12))

        actual `shouldBe` res

    it "import context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp_ <- makeAbsolute "./ExampleContext.hs"
        let res = IdeResultOk (Just (ImportContext "Data.List"))
        actual <- getContextAt fp_ (toPos (3, 8))

        actual `shouldBe` res

    it "function declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp_ <- makeAbsolute "./ExampleContext.hs"
              let res = IdeResultOk (Just TypeContext)
              actual <- getContextAt fp_ (toPos (6, 1))

              actual `shouldBe` res


    it "function definition context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp_ <- makeAbsolute "./ExampleContext.hs"
              let res = IdeResultOk (Just ValueContext)
              actual <- getContextAt fp_ (toPos (7, 1))
              actual `shouldBe` res

    it "function signature context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp_ <- makeAbsolute "./ExampleContext.hs"
              let res = IdeResultOk (Just TypeContext)
              actual <- getContextAt fp_ (toPos (6, 8))
              actual `shouldBe` res

    it "nothing" $ withCurrentDirectory "./test/testdata/context" $ do
        fp_ <- makeAbsolute "./ExampleContext.hs"
        let res = IdeResultOk Nothing
        actual <- getContextAt fp_ (toPos (2, 1))
        actual `shouldBe` res

getContextAt :: [Char] -> Position -> IO (IdeResult (Maybe Context))
getContextAt fp_ pos = do
    let arg = filePathToUri fp_
    runSingle (IdePlugins mempty) $ do
        _ <- setTypecheckedModule arg
        pluginGetFile "getContext: " arg $ \fp ->
            ifCachedModuleAndData fp (IdeResultOk Nothing) $ \tm _ () ->
                return $ IdeResultOk $ getContext pos (tm_parsed_module tm)
