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
              let res = Right (Nothing :: Maybe Context)
              actual <- runSingle (IdePlugins mempty) fp $ do
                  _ <- setTypecheckedModule arg
                  return $ Right Nothing

              actual `shouldBe` res

    it "module header context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp <- makeAbsolute "./ExampleContext.hs"
              let res = Right $ Just $ ModuleContext "ExampleContext"

              actual <- getContextAt fp (toPos (1, 10))

              actual `shouldBe` res


    it "module export list context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp <- makeAbsolute "./ExampleContext.hs"
              let res = Right $ Just ExportContext
              actual <- getContextAt fp (toPos (1, 24))

              actual `shouldBe` res

    it "value context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp <- makeAbsolute "./ExampleContext.hs"
        let res = Right $ Just ValueContext
        actual <- getContextAt fp (toPos (7, 6))

        actual `shouldBe` res

    it "value addition context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp <- makeAbsolute "./ExampleContext.hs"
        let res = Right $ Just ValueContext
        actual <- getContextAt fp (toPos (7, 12))

        actual `shouldBe` res

    it "import context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp <- makeAbsolute "./ExampleContext.hs"
        let res = Right $ Just $ ImportContext "Data.List"
        actual <- getContextAt fp (toPos (3, 8))

        actual `shouldBe` res

    it "import list context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp <- makeAbsolute "./ExampleContext.hs"
        let res = Right $ Just $ ImportListContext "Data.List"
        actual <- getContextAt fp (toPos (3, 20))

        actual `shouldBe` res

    it "import hiding context" $ withCurrentDirectory "./test/testdata/context" $ do
        fp <- makeAbsolute "./ExampleContext.hs"
        let res = Right $ Just $ ImportHidingContext "Control.Monad"
        actual <- getContextAt fp (toPos (4, 32))

        actual `shouldBe` res

    it "function declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp <- makeAbsolute "./ExampleContext.hs"
              let res = Right $ Just TypeContext
              actual <- getContextAt fp (toPos (6, 1))

              actual `shouldBe` res

    it "function signature context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right $ Just TypeContext
            actual <- getContextAt fp (toPos (6, 8))
            actual `shouldBe` res


    it "function definition context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
              fp <- makeAbsolute "./ExampleContext.hs"
              let res = Right $ Just ValueContext
              actual <- getContextAt fp (toPos (7, 1))
              actual `shouldBe` res

    -- This is interesting, the context for this is assumed to be ValueContext
    -- although the cursor is at the signature of a function in a where clause.
    -- Reason is probably that we only traverse the AST until we know that
    -- that we are in a ValueContext, however, within a ValueContext, another
    -- TypeContext may arise, like in this case.
    it "inner function declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right $ Just ValueContext
            actual <- getContextAt fp (toPos (9, 10))
            actual `shouldBe` res

    it "inner function value context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right $ Just ValueContext
            actual <- getContextAt fp (toPos (10, 10))
            actual `shouldBe` res


    -- Declare a datatype, is Nothing, could be DataContext
    it "data declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right Nothing
            actual <- getContextAt fp (toPos (12, 8))
            actual `shouldBe` res

    -- Define a datatype.
    it "data definition context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right $ Just TypeContext
            actual <- getContextAt fp (toPos (12, 18))
            actual `shouldBe` res

    -- Declaration of a class. Should be something with types.
    it "class declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right Nothing
            actual <- getContextAt fp (toPos (15, 8))
            actual `shouldBe` res

    -- Function signature in class declaration.
    -- Ought to be TypeContext
    it "class declaration function sig context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right Nothing
            actual <- getContextAt fp (toPos (16, 7))
            actual `shouldBe` res

    it "instance declaration context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right Nothing
            actual <- getContextAt fp (toPos (18, 7))
            actual `shouldBe` res

    -- Function definition in an instance declaration
    -- Should be ValueContext, but nothing is fine, too for now
    it "instance declaration function def context"
            $ withCurrentDirectory "./test/testdata/context"
            $ do
                fp <- makeAbsolute "./ExampleContext.hs"
                let res = Right Nothing
                actual <- getContextAt fp (toPos (19, 6))
                actual `shouldBe` res

    -- This seems plain wrong, if the cursor is on the String "deriving",
    -- we would expect the context to be DerivingContext, but it is not.
    -- May require investigation if this is important.
    it "deriving context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right Nothing
            actual <- getContextAt fp (toPos (13, 9))
            actual `shouldBe` res

    -- Cursor is directly before the open parenthesis of a deriving clause.
    -- E.g. deriving (...)
    --               ^---- cursor is here
    -- Context is still Nothing.
    it "deriving parenthesis context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right Nothing
            actual <- getContextAt fp (toPos (13, 14))
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
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right (Just TypeContext)
            actual <- getContextAt fp (toPos (13, 15))
            actual `shouldBe` res

    it "deriving typeclass context"
        $ withCurrentDirectory "./test/testdata/context"
        $ do
            fp <- makeAbsolute "./ExampleContext.hs"
            let res = Right (Just TypeContext)
            actual <- getContextAt fp (toPos (13, 18))
            actual `shouldBe` res

    -- Point at an empty line.
    -- There is no context
    it "nothing" $ withCurrentDirectory "./test/testdata/context" $ do
        fp <- makeAbsolute "./ExampleContext.hs"
        let res = Right Nothing
        actual <- getContextAt fp (toPos (2, 1))
        actual `shouldBe` res

getContextAt :: FilePath -> Position -> IO (IdeResult (Maybe Context))
getContextAt fp pos = do
    let arg = filePathToUri fp
    runSingle (IdePlugins mempty) fp $ do
        _ <- setTypecheckedModule arg
        pluginGetFile "getContext: " arg $ \fp_ ->
            ifCachedModuleAndData fp_ (Right Nothing) $ \tm _ () ->
                return $ Right $ getContext pos (tm_parsed_module tm)
