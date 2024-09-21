{-# LANGUAGE OverloadedStrings #-}

module To where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Test.Tasty
import Test.Tasty.HUnit

import Data.S5rd.To

test_unit_to :: [TestTree]
test_unit_to =
  [ testCase "failure of toS5rdKey" $ do
    Left ToS5rdKeyError'Text'InvalidIdentifier @=? toS5rdKey ("-" :: TS.Text)
    Left ToS5rdKeyError'Text'InvalidIdentifier @=? toS5rdKey ("-" :: TL.Text)
    Left ToS5rdKeyError'Text'InvalidIdentifier @=? toS5rdKey ("_" :: TS.Text)
    Left ToS5rdKeyError'Text'InvalidIdentifier @=? toS5rdKey ("_" :: TL.Text)
  , testCase "reject negative integer" $ do
    Left ToS5rdError'Number'Negative @=? toS5rd (negate 1 :: Integer)
    Left ToS5rdError'Number'Negative @=? toS5rd (negate 1 :: Int)
    Left ToS5rdKeyError'Number'Negative @=? toS5rdKey (negate 1 :: Integer)
    Left ToS5rdKeyError'Number'Negative @=? toS5rdKey (negate 1 :: Int)
  ]
