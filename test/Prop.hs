module Prop where

import Control.Monad (forM)
import qualified Data.ByteString.Lazy as BL
import Data.List (intersperse)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.S5rd
import Internal

test_prop :: [TestTree]
test_prop =
  [ testProperty "parse _ <=< serializeBinary === Right" $ property $ do
    v0 <- forAll s5rd'gen
    b0 <- evalEither $ serializeBinaryS5rd v0
    v1 <- evalEither $ parseS5rd "" b0
    ParseS5rdResult'One v0 === v1
  , testProperty "parse _ <=< serializeText === Right" $ property $ do
    v0 <- forAll s5rd'gen
    t0 <- evalEither $ serializeTextS5rd v0
    v1 <- evalEither $ parseS5rd "" $ TLE.encodeUtf8 t0
    ParseS5rdResult'One v0 === v1
  , testProperty "parse _ >=> serializeBinary === Right" $ property $ do
    v0 <- forAll s5rd'gen
    b0 <- evalEither $ serializeBinaryS5rd v0
    v1' <- evalEither $ parseS5rd "" b0
    v1 <- evalMaybe $ case v1' of
      ParseS5rdResult'One v -> Just v
      _ -> Nothing
    b1 <- evalEither $ serializeBinaryS5rd v1
    b0 === b1
  , testProperty "parse _ >=> serializeText === Right" $ property $ do
    v0 <- forAll s5rd'gen
    t0 <- evalEither $ serializeTextS5rd v0
    v1' <- evalEither $ parseS5rd "" $ TLE.encodeUtf8 t0
    v1 <- evalMaybe $ case v1' of
      ParseS5rdResult'One v -> Just v
      _ -> Nothing
    t1 <- evalEither $ serializeTextS5rd v1
    t0 === t1
  , testProperty "parse _ <=< prettyText === Right" $ property $ do
    v0 <- forAll s5rd'gen
    t0 <- evalEither $ prettyTextS5rd v0
    v1 <- evalEither $ parseS5rd "" $ TLE.encodeUtf8 t0
    ParseS5rdResult'One v0 === v1
  , testProperty "parse _ >=> prettyText === Right" $ property $ do
    v0 <- forAll s5rd'gen
    t0 <- evalEither $ prettyTextS5rd v0
    v1' <- evalEither $ parseS5rd "" $ TLE.encodeUtf8 t0
    v1 <- evalMaybe $ case v1' of
      ParseS5rdResult'One v -> Just v
      _ -> Nothing
    t1 <- evalEither $ prettyTextS5rd v1
    t0 === t1
  , testProperty "length (serializeBinary _) `mod` 4 === 0" $ property $ do
    v <- forAll s5rd'gen
    b <- evalEither $ serializeBinaryS5rd v
    0 === BL.length b `mod` 4
  ]

renderMultipleDocuments :: Bool -> [S5rd] -> Either PrettyS5rdError'Text TL.Text
renderMultipleDocuments b vs = do
  xs <- forM vs $ prettyTextS5rd' 0
  return $ TB.toLazyText $ (if b then TB.fromString "***\n" else mempty)
    <> mconcat (intersperse (TB.fromString "\n***\n") xs)

test_prop_multiple_documents :: [TestTree]
test_prop_multiple_documents =
  [ testProperty "parse can parse multiple documents" $ property $ do
    vs0 <- forAll $ Gen.list (Range.linear 0 6) s5rd'gen
    b <- forAll Gen.bool_
    t <- evalEither $ renderMultipleDocuments b vs0
    let evs = parseS5rd "" $ TLE.encodeUtf8 t
    case evs of
      Left ParseS5rdError'Empty -> vs0 === []
      Right (ParseS5rdResult'One v) -> vs0 === [v]
      Right (ParseS5rdResult'Multiple vs) -> vs0 === V.toList vs
      _ -> failure
  ]
