module ReferencesSpec where

import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Test.Hspec
import TestUtils
import Control.Monad.IO.Class
import Control.Lens

spec :: Spec
spec = describe "references" $
  it "works with definitions" $ runSession hieCommand "test/testdata" $ do
    doc <- openDoc "References.hs" "haskell"
    let pos = Position 2 7 -- foo = bar <--
    refs <- getReferences doc pos True
    liftIO $ refs `shouldContain` map (Location (doc ^. uri)) [
        mkRange 4 0 4 3
      , mkRange 8 11 8 14
      , mkRange 7 7 7 10
      , mkRange 4 14 4 17
      , mkRange 4 0 4 3
      , mkRange 2 6 2 9
      ]
  -- TODO: Respect withDeclaration parameter
  -- it "works without definitions" $ runSession hieCommand "test/testdata" $ do
  --   doc <- openDoc "References.hs" "haskell"
  --   let pos = Position 2 7 -- foo = bar <--
  --   refs <- getReferences doc pos False
  --   liftIO $ refs `shouldNotContain` [Location (doc ^. uri) (mkRange 4 0 4 3)]
  
  where mkRange sl sc el ec = Range (Position sl sc) (Position el ec)
