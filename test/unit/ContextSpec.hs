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

    it "value addition context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp_ <- makeAbsolute "./ExampleContext.hs"
        let res = IdeResultOk (Just ValueContext)
        actual <- getContextAt fp_ (toPos (7, 12))

        actual `shouldBe` res

    it "import context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp_ <- makeAbsolute "./ExampleContext.hs"
        let res = IdeResultOk (Just (ImportContext "Data.List"))
        actual <- getContextAt fp_ (toPos (3, 8))

        actual `shouldBe` res

    it "import list context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp_ <- makeAbsolute "./ExampleContext.hs"
        let res = IdeResultOk (Just (ImportListContext "Data.List"))
        actual <- getContextAt fp_ (toPos (3, 20))

        actual `shouldBe` res

    it "import hiding context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp_ <- makeAbsolute "./ExampleContext.hs"
        let res = IdeResultOk (Just (ImportHidingContext "Control.Monad"))
        actual <- getContextAt fp_ (toPos (4, 32))

        actual `shouldBe` res

    it "function declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp_ <- makeAbsolute "./ExampleContext.hs"
              let res = IdeResultOk (Just TypeContext)
              actual <- getContextAt fp_ (toPos (6, 1))

              actual `shouldBe` res
              
    it "function signature context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk (Just TypeContext)
            actual <- getContextAt fp_ (toPos (6, 8))
            actual `shouldBe` res


    it "function definition context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp_ <- makeAbsolute "./ExampleContext.hs"
              let res = IdeResultOk (Just ValueContext)
              actual <- getContextAt fp_ (toPos (7, 1))
              actual `shouldBe` res

    -- This is interesting, the context for this is assumed to be ValueContext
    -- although the cursor is at the signature of a function in a where clause.
    -- Reason is probably that we only traverse the AST until we know that
    -- that we are in a ValueContext, however, within a ValueContext, another
    -- TypeContext may arise, like in this case.
    it "inner function declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk (Just ValueContext)
            actual <- getContextAt fp_ (toPos (9, 10))
            actual `shouldBe` res

    it "inner function value context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk (Just ValueContext)
            actual <- getContextAt fp_ (toPos (10, 10))
            actual `shouldBe` res


    -- Declare a datatype, is Nothing, could be DataContext
    it "data declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk Nothing
            actual <- getContextAt fp_ (toPos (12, 8))
            actual `shouldBe` res

    -- Define a datatype.
    it "data definition context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk (Just TypeContext)
            actual <- getContextAt fp_ (toPos (12, 18))
            actual `shouldBe` res

    it "class declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk (Just ClassContext)
            actual <- getContextAt fp_ (toPos (15, 8))
            actual `shouldBe` res

    it "class declaration function sig context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk (Just ClassContext)
            actual <- getContextAt fp_ (toPos (16, 7))
            actual `shouldBe` res

    it "instance declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk (Just InstanceContext)
            actual <- getContextAt fp_ (toPos (18, 7))
            actual `shouldBe` res

    -- Function definition
    it "instance declaration function def context"
            $ withCurrentDirectory "./test/testdata/context"
            $ do
                fp_ <- makeAbsolute "./ExampleContext.hs"
                let res = IdeResultOk (Just InstanceContext)
                actual <- getContextAt fp_ (toPos (19, 6))
                actual `shouldBe` res

    -- This seems plain wrong, if the cursor is on the String "deriving",
    -- we would expect the context to be DerivingContext, but it is not.
    -- May require investigation if this is important.
    it "deriving context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk Nothing
            actual <- getContextAt fp_ (toPos (13, 9))
            actual `shouldBe` res

    -- Cursor is directly before the open parenthesis of a deriving clause.
    -- E.g. deriving (...)
    --               ^---- cursor is here
    -- Context is still Nothing.
    it "deriving parenthesis context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk Nothing
            actual <- getContextAt fp_ (toPos (13, 14))
            actual `shouldBe` res

    -- Cursor is directly after the open parenthesis of a deriving clause.
    -- E.g. deriving (...)
    --                ^---- cursor is here
    -- Context is now Type. This makes sense, but an extension may be to be
    -- aware of the context of a deriving clause, thus offering only Type Classes
    -- as a completion.
    it "deriving parenthesis context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk (Just TypeContext)
            actual <- getContextAt fp_ (toPos (13, 15))
            actual `shouldBe` res

    it "deriving typeclass context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp_ <- makeAbsolute "./ExampleContext.hs"
            let res = IdeResultOk (Just TypeContext)
            actual <- getContextAt fp_ (toPos (13, 18))
            actual `shouldBe` res

    -- Point at an empty line.
    -- There is no context
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
