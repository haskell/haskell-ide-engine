{-# LANGUAGE OverloadedStrings #-}
module HieBiosSpec where

import Control.Applicative.Combinators
import qualified Data.Text as T
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Messages
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "hie-bios" $
  it "reports errors in hie.yaml" $ runSession hieCommand fullCaps "test/testdata/hieBiosError" $ do
    _ <- openDoc "Foo.hs" "haskell"
    _ <- skipManyTill loggingNotification (satisfy isMessage)
    return ()
  where isMessage (NotShowMessage (NotificationMessage _ _ (ShowMessageParams MtError s))) =
          "Couldn't parse hie.yaml" `T.isInfixOf` s
        isMessage _ = False