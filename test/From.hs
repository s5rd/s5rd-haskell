{-# LANGUAGE OverloadedStrings #-}

module From where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Proxy
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Data.S5rd.From
import Data.S5rd.Type

fromProxy :: FromS5rd a => Proxy a -> S5rd -> Either (FromS5rdError a) a
fromProxy _proxy v = fromS5rd v
fromKeyProxy :: FromS5rdKey a
             => Proxy a -> S5rdKey -> Either (FromS5rdKeyError a) a
fromKeyProxy _proxy v = fromS5rdKey v

s5rd'Binary :: S5rd
s5rd'Binary = S5rd'Binary ""
s5rd'Identifier :: S5rd
s5rd'Identifier = S5rd'Identifier "a"
s5rd'Number :: S5rd
s5rd'Number = S5rd'Number 0
s5rd'Text :: S5rd
s5rd'Text = S5rd'Text ""
s5rd'Array :: S5rd
s5rd'Array = S5rd'Array V.empty
s5rd'KeyValue :: S5rd
s5rd'KeyValue = S5rd'KeyValue s5rdKey'Number s5rd'Number
s5rdKey'Binary :: S5rdKey
s5rdKey'Binary = S5rdKey'Binary ""
s5rdKey'Identifier :: S5rdKey
s5rdKey'Identifier = S5rdKey'Identifier "a"
s5rdKey'Number :: S5rdKey
s5rdKey'Number = S5rdKey'Number 0

proxyBS :: Proxy BS.ByteString
proxyBS = Proxy
proxyBL :: Proxy BL.ByteString
proxyBL = Proxy
proxyTS :: Proxy TS.Text
proxyTS = Proxy
proxyTL :: Proxy TL.Text
proxyTL = Proxy
proxyInteger :: Proxy Integer
proxyInteger = Proxy
proxyInt :: Proxy Int
proxyInt = Proxy

test_unit_from :: [TestTree]
test_unit_from =
  [ testCase "failure of fromS5rd" $ do
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyBS s5rd'Number
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyBS s5rd'Text
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyBS s5rd'Array
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyBS s5rd'KeyValue
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyBL s5rd'Number
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyBL s5rd'Text
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyBL s5rd'Array
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyBL s5rd'KeyValue
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyTS s5rd'Binary
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyTS s5rd'Number
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyTS s5rd'Array
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyTS s5rd'KeyValue
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyTL s5rd'Binary
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyTL s5rd'Number
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyTL s5rd'Array
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyTL s5rd'KeyValue
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyInteger s5rd'Binary
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyInteger s5rd'Identifier
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyInteger s5rd'Text
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyInteger s5rd'Array
    Left FromS5rdError'Common'InvalidValue @=? fromProxy proxyInteger s5rd'KeyValue
    Left FromS5rdError'Number'InvalidValue @=? fromProxy proxyInt s5rd'Binary
    Left FromS5rdError'Number'InvalidValue @=? fromProxy proxyInt s5rd'Identifier
    Left FromS5rdError'Number'InvalidValue @=? fromProxy proxyInt s5rd'Text
    Left FromS5rdError'Number'InvalidValue @=? fromProxy proxyInt s5rd'Array
    Left FromS5rdError'Number'InvalidValue @=? fromProxy proxyInt s5rd'KeyValue
  , testCase "failure of fromS5rdKey" $ do
    Left FromS5rdKeyError'Common'InvalidValue
      @=? fromKeyProxy proxyBS s5rdKey'Number
    Left FromS5rdKeyError'Common'InvalidValue
      @=? fromKeyProxy proxyBL s5rdKey'Number
    Left FromS5rdKeyError'Common'InvalidValue
      @=? fromKeyProxy proxyTS s5rdKey'Number
    Left FromS5rdKeyError'Common'InvalidValue
      @=? fromKeyProxy proxyTS s5rdKey'Binary
    Left FromS5rdKeyError'Common'InvalidValue
      @=? fromKeyProxy proxyTL s5rdKey'Number
    Left FromS5rdKeyError'Common'InvalidValue
      @=? fromKeyProxy proxyTL s5rdKey'Binary
    Left FromS5rdKeyError'Common'InvalidValue
      @=? fromKeyProxy proxyInteger s5rdKey'Binary
    Left FromS5rdKeyError'Common'InvalidValue
      @=? fromKeyProxy proxyInteger s5rdKey'Identifier
    Left FromS5rdKeyError'Number'InvalidValue
      @=? fromKeyProxy proxyInt s5rdKey'Binary
    Left FromS5rdKeyError'Number'InvalidValue
      @=? fromKeyProxy proxyInt s5rdKey'Identifier
  , testCase "reject overflow of Int" $ do
    Left FromS5rdError'Number'Overflow @=? fromProxy proxyInt
      (S5rd'Number $ fromIntegral (maxBound :: Int) + 1)
    Left FromS5rdKeyError'Number'Overflow @=? fromKeyProxy proxyInt
      (S5rdKey'Number $ fromIntegral (maxBound :: Int) + 1)
  ]
