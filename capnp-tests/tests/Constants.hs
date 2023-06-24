{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Constants (tests) where

import Capnp
import Capnp.Gen.Aircraft hiding (Parsed)
import Data.Default
import Test.Hspec

tests :: Spec
tests = do
  describe "parsing defaults" $ do
    defTest "structs" Zdate {year = 0, month = 0, day = 0}
    defTest "struct lists" ([] :: [Parsed Zdate])
    -- defTest "enums" Airport'none
    defTest "enum lists" ([] :: [Airport])
  describe "parsing constants" $ do
    it "should parse enums correctly" $
      constEnum `shouldBe` Airport'jfk
    constTest "structs" constDate Zdate {year = 2015, month = 8, day = 27}
    constTest
      "lists"
      constList
      [ Zdate {year = 2015, month = 8, day = 27},
        Zdate {year = 2015, month = 8, day = 28}
      ]

defTest :: (Default a, Eq a, Show a) => String -> a -> Spec
defTest name expected =
  it ("should parse the default for " ++ name ++ " correctly") $ do
    def `shouldBe` expected

constTest :: (Parse a (Parsed a), Show (Parsed a), Eq (Parsed a)) => String -> Raw a 'Const -> Parsed a -> Spec
constTest name input expected =
  it ("should parse " ++ name ++ " correctly") $ do
    actual <- evalLimitT maxBound $ parse input
    actual `shouldBe` expected
