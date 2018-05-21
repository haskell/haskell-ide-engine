module IdeResponseSpec where

import Test.Hspec
import Data.Aeson
import Data.Text
import Haskell.Ide.Engine.PluginsIdeMonads
    
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "IdeResponseOk" $ do
    it "works with >>= IdeResponseOk" $ do
      (IdeResponseOk (42 :: Int) >>= \_ -> IdeResponseOk 32)
        `shouldBe` ((IdeResponseOk 32) :: IdeResponse Int)

    it "works with >>= IdeResponseFail" $ do
      (IdeResponseOk (42 :: Int) >>= \_ -> (IdeResponseFail pluginError) :: IdeResponse Int)
        `shouldBe` ((IdeResponseFail pluginError))

    it "works with >>= IdeResponseDeferred" $ do
      (IdeResponseOk (42 :: Int) >>= \_ ->
        IdeResponseDeferred "path" (\_ -> return $ IdeResponseOk "blah"))
        `shouldSatisfy` \x ->
          case x of
            IdeResponseDeferred "path" _ -> True
            _ -> False

  describe "IdeResponseFail" $ do
    it "works with >>= IdeResponseOk" $ do
      (IdeResponseFail pluginError >>= \_ -> IdeResponseOk 42 :: IdeResponse Int)
        `shouldBe` (IdeResponseFail pluginError)
        
    it "works with >>= IdeResponseDeferred" $ do
      (IdeResponseFail pluginError >>= \_ ->
        IdeResponseDeferred "path" (\_ -> return $ IdeResponseOk "blah"))
        `shouldBe` (IdeResponseFail pluginError)
  
  where pluginError = IdeError OtherError (pack "woops") Null