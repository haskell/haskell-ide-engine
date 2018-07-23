module DefinitionSpec where

import Control.Lens
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "definitions" $
  it "works" $ runSession hieCommand "test/testdata" $ do
    doc <- openDoc "References.hs" "haskell"
    defs <- getDefinitions doc (Position 7 8)
    let expRange = Range (Position 4 0) (Position 4 3)
    liftIO $ defs `shouldBe` [Location (doc ^. uri) expRange]
