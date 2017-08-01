{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Test for JSON serialization
module JsonSpec where

import           Haskell.Ide.Engine.MonadTypes

import           Haskell.Ide.ApplyRefactPlugin
import           Haskell.Ide.GhcModPlugin
import           Haskell.Ide.HaRePlugin

import           Data.Aeson
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck               hiding (Success)
import           Test.QuickCheck.Instances     ()

-- import Debug.Trace
-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "dispatcher" jsonSpec

-- ---------------------------------------------------------------------

jsonSpec :: Spec
jsonSpec = do
  describe "General JSON instances round trip" $ do
  -- Plugin params
    prop "ApplyOneParams" (propertyJsonRoundtrip :: ApplyOneParams -> Bool)
    prop "InfoParams" (propertyJsonRoundtrip :: InfoParams -> Bool)
    prop "TypeParams" (propertyJsonRoundtrip :: TypeParams -> Bool)
    prop "HarePoint" (propertyJsonRoundtrip :: HarePoint -> Bool)
    prop "HarePointWithText" (propertyJsonRoundtrip :: HarePointWithText -> Bool)
    prop "HareRange" (propertyJsonRoundtrip :: HareRange -> Bool)
  -- Plugin Api types
    prop "IdeErrorCode" (propertyJsonRoundtrip :: IdeErrorCode -> Bool)
    prop "IdeError" (propertyJsonRoundtrip :: IdeError -> Bool)

-- ---------------------------------------------------------------------

propertyJsonRoundtrip :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
propertyJsonRoundtrip a = Success a == fromJSON (toJSON a)

-- enough for our needs
instance Arbitrary Value where
  arbitrary = do
    s <- arbitrary
    return $ String s

-- | make lists of maximum length 3 for test performance
smallList :: Gen a -> Gen [a]
smallList = resize 3 . listOf

instance Arbitrary ApplyOneParams where
  arbitrary = AOP <$> arbitrary <*> arbitrary

instance Arbitrary TypeParams where
  arbitrary = TP <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InfoParams where
  arbitrary = IP <$> arbitrary <*> arbitrary

instance Arbitrary HarePoint where
  arbitrary = HP <$> arbitrary <*> arbitrary

instance Arbitrary HarePointWithText where
  arbitrary = HPT <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary HareRange where
  arbitrary = HR <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Uri where
  arbitrary = filePathToUri <$> arbitrary

instance Arbitrary Range where
  arbitrary = Range <$> arbitrary <*> arbitrary

instance Arbitrary Location where
  arbitrary = Location <$> arbitrary <*> arbitrary

instance Arbitrary TextDocumentIdentifier where
  arbitrary = TextDocumentIdentifier <$> arbitrary

instance Arbitrary TextDocumentPositionParams where
  arbitrary = TextDocumentPositionParams <$> arbitrary <*> arbitrary

instance Arbitrary IdeErrorCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary IdeError where
  arbitrary = IdeError <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Position where
  arbitrary = do
    Positive l <- arbitrary
    Positive c <- arbitrary
    return $ Position l c
