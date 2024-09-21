module Internal where

import qualified Data.ByteString.Short as SBS
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.S5rd

genIdentifier :: Gen SBS.ShortByteString
genIdentifier = do
  c0 <- Gen.choice [Gen.alphaNum, Gen.element ['.']]
  cs <- Gen.list (Range.linear 0 48)
    $ Gen.choice [Gen.alphaNum, Gen.element ['-', '.', '_']]
  return $ SBS.toShort $ TE.encodeUtf8 $ TS.pack $ c0 : cs

intMaxBound :: Integer
intMaxBound = 2 ^ (55 :: Int) -- TODO: 65

s5rdKey'gen :: Gen S5rdKey
s5rdKey'gen = Gen.choice
  [ do
    bs <- Gen.bytes $ Range.linear 0 96
    return $ S5rdKey'Binary bs
  , S5rdKey'Identifier <$> genIdentifier
  , do
    x <- Gen.integral $ Range.exponential 0 intMaxBound
    return $ S5rdKey'Number x
  ]

s5rd'gen :: Gen S5rd
s5rd'gen = fmap s5rd'canonicalize $ Gen.sized $ go . half . Range.unSize
 where
  half x = x `div` 11
  go 0 = Gen.choice
    [ do
      bs <- Gen.bytes $ Range.linear 0 96
      return $ S5rd'Binary bs
    , S5rd'Identifier <$> genIdentifier
    , do
      x <- Gen.integral $ Range.exponential 0 intMaxBound
      return $ S5rd'Number x
    , do
      ts <- Gen.text (Range.linear 0 96) Gen.unicode
      return $ S5rd'Text ts
    ]
  go n = Gen.choice
    [ do
      bs <- Gen.bytes $ Range.linear 0 96
      return $ S5rd'Binary bs
    , S5rd'Identifier <$> genIdentifier
    , do
      x <- Gen.integral $ Range.exponential 0 intMaxBound
      return $ S5rd'Number x
    , do
      ts <- Gen.text (Range.linear 0 96) Gen.unicode
      return $ S5rd'Text ts
    , do
      xs <- Gen.list (Range.linear 0 6) $ go $ pred n
      return $ S5rd'Array $ V.fromList xs
    , do
      xs <- Gen.list (Range.linear 2 6) $ do
        k <- s5rdKey'gen
        v <- go $ pred n
        return $ S5rd'KeyValue k v
      return $ S5rd'Array $ V.fromList xs
    , do
      k <- s5rdKey'gen
      v <- go $ pred n
      return $ S5rd'KeyValue k v
    ]
