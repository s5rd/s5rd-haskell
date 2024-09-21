{-# LANGUAGE OverloadedStrings #-}

module Util where

import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit ((@=?), testCase)

import Data.S5rd.Util

test_unit_someIdentifier :: [TestTree]
test_unit_someIdentifier =
  [ testCase "valid identifier" $ do
    True @=? s5rd'isValidIdentifierB "Test"
    True @=? s5rd'isValidIdentifierT "Test"
    True @=? s5rd'isValidIdentifierB ".net-"
    True @=? s5rd'isValidIdentifierT ".net-"
  , testCase "invalid identifier" $ do
    False @=? s5rd'isValidIdentifierB ""
    False @=? s5rd'isValidIdentifierT ""
    False @=? s5rd'isValidIdentifierB "<>"
    False @=? s5rd'isValidIdentifierT "<>"
    False @=? s5rd'isValidIdentifierB "-net."
    False @=? s5rd'isValidIdentifierT "-net."
  ]

test_prop_isValidIdentifier :: [TestTree]
test_prop_isValidIdentifier =
  let genHeadIdentifierChar = Gen.choice [Gen.alphaNum, Gen.element ['.']]
      genIdentifierChar = Gen.choice [Gen.alphaNum, Gen.element ['-', '.', '_']]
   in
  [ testProperty "valid identifier" $ property $ do
    c <- forAll genHeadIdentifierChar
    cs <- forAll $ Gen.list (Range.linear 0 48) genIdentifierChar
    assert $ s5rd'isValidIdentifierT $ T.pack (c : cs)
  , testProperty "invalid identifier" $ property $ do
    c <- forAll $ Gen.element ['-', '_']
    cs <- forAll $ Gen.list (Range.linear 0 48) genIdentifierChar
    assert $ not $ s5rd'isValidIdentifierT $ T.pack (c : cs)
  ]
