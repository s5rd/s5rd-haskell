{-# LANGUAGE OverloadedStrings #-}

module External where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Scientific (toBoundedInteger)
import Data.String
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Yaml as Yaml
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.S5rd
import Internal

-- We use Gen.alphaNum for text generation instead of Gen.unicode.
-- In YAML, "\N" means NEXT LINE (\133). It conflicts with our "\NUL" (\0).

genJsonKey :: Gen Aeson.Key
genJsonKey = fromString . TS.unpack . TE.decodeUtf8 . SBS.fromShort
  <$> genIdentifier

genJsonValue :: Gen Aeson.Value
genJsonValue = Gen.sized $ go . half . Range.unSize
 where
  half x = x `div` 11
  go 0 = Gen.choice
    [ do
      ts <- Gen.text (Range.linear 0 96) Gen.alphaNum
      return $ Aeson.String ts
    , do
      x <- Gen.integral $ Range.exponential 0 intMaxBound
      return $ Aeson.Number $ fromIntegral x
    , do
      b <- Gen.bool_
      return $ Aeson.Bool b
    , return Aeson.Null
    ]
  go n = Gen.choice
    [ do
      ts <- Gen.text (Range.linear 0 96) Gen.alphaNum
      return $ Aeson.String ts
    , do
      x <- Gen.integral $ Range.exponential 0 intMaxBound
      return $ Aeson.Number $ fromIntegral x
    , do
      b <- Gen.bool_
      return $ Aeson.Bool b
    , return Aeson.Null
    , do
      xs <- Gen.list (Range.linear 0 6) $ do
        k <- genJsonKey
        v <- go $ pred n
        return (k, v)
      return $ Aeson.Object $ AesonKeyMap.fromList xs
    , do
      xs <- Gen.list (Range.linear 0 6) $ go $ pred n
      return $ Aeson.Array $ V.fromList xs
    ]

mapJsonToS5rd :: Aeson.Value -> S5rd
mapJsonToS5rd (Aeson.String ts) = S5rd'Text ts
mapJsonToS5rd (Aeson.Number x) = S5rd'Number $ fromIntegral y
 where
  y :: Int
  y = fromJust $ toBoundedInteger x
mapJsonToS5rd (Aeson.Bool b) = S5rd'Identifier $ if b then "true" else "false"
mapJsonToS5rd Aeson.Null = S5rd'Identifier "null"
mapJsonToS5rd (Aeson.Object obj) = S5rd'Array $ V.fromList
  $ flip map (AesonKeyMap.toList obj) $ \(k, v) -> S5rd'KeyValue
    (S5rdKey'Identifier $ SBS.toShort $ TE.encodeUtf8 $ TS.pack
      $ AesonKey.toString k)
    (mapJsonToS5rd v)
mapJsonToS5rd (Aeson.Array xs) = S5rd'Array $ V.map mapJsonToS5rd xs

subMap :: Map.Map SBS.ShortByteString Aeson.Value
subMap = Map.fromList
  [ ("null", Aeson.Null)
  , ("true", Aeson.Bool True)
  , ("false", Aeson.Bool False)
  ]

mapS5rdKeyToJsonKey :: S5rdKey -> Maybe Aeson.Key
mapS5rdKeyToJsonKey (S5rdKey'Binary _) = Nothing
mapS5rdKeyToJsonKey (S5rdKey'Number _) = Nothing
mapS5rdKeyToJsonKey (S5rdKey'Identifier sbs) = Just
  $ AesonKey.fromText $ TE.decodeUtf8 $ SBS.fromShort sbs

mapS5rdToJson :: S5rd -> Maybe Aeson.Value
mapS5rdToJson (S5rd'Binary _) = Nothing
mapS5rdToJson (S5rd'Identifier sbs) = case Map.lookup sbs subMap of
  Just v -> Just v
  Nothing -> Just $ Aeson.String $ TE.decodeUtf8 $ SBS.fromShort sbs
mapS5rdToJson (S5rd'Number x) = Just $ Aeson.Number $ fromInteger x
mapS5rdToJson (S5rd'Text ts) = Just $ Aeson.String ts
mapS5rdToJson (S5rd'Array xs) =
  if V.length xs > 0 && V.all s5rd'isKeyValue xs
    then do
      ys <- V.forM xs $ \kv -> case kv of
        (S5rd'KeyValue k v) -> do
          kr <- mapS5rdKeyToJsonKey k
          vr <- mapS5rdToJson v
          return (kr, vr)
        _ -> error "unreachable"
      return $ Aeson.Object $ AesonKeyMap.fromList $ V.toList ys
    else do
      ys <- V.mapM mapS5rdToJson xs
      return $ Aeson.Array ys
mapS5rdToJson (S5rd'KeyValue _ _) = Nothing

genIdentifierNonNumHead :: Gen SBS.ShortByteString
genIdentifierNonNumHead = do
  c0 <- Gen.choice [Gen.alpha, Gen.element ['.']]
  cs <- Gen.list (Range.linear 0 48)
    $ Gen.choice [Gen.alphaNum, Gen.element ['-', '.', '_']]
  return $ SBS.toShort $ TE.encodeUtf8 $ TS.pack $ c0 : cs

s5rd'genYaml :: Gen S5rd
s5rd'genYaml = Gen.sized $ go . half . Range.unSize
 where
  half x = x `div` 11
  keygen = S5rdKey'Identifier <$> genIdentifierNonNumHead
  go 0 = Gen.choice
    [ S5rd'Identifier <$> genIdentifierNonNumHead
    , do
      x <- Gen.integral $ Range.exponential 0 intMaxBound
      return $ S5rd'Number x
    , do
      ts <- Gen.text (Range.linear 0 96) Gen.alphaNum
      return $ S5rd'Text ts
    , return $ S5rd'Identifier "null"
    , return $ S5rd'Identifier "true"
    , return $ S5rd'Identifier "false"
    ]
  go n = Gen.choice
    [ S5rd'Identifier <$> genIdentifierNonNumHead
    , do
      x <- Gen.integral $ Range.exponential 0 intMaxBound
      return $ S5rd'Number x
    , do
      ts <- Gen.text (Range.linear 0 96) Gen.alphaNum
      return $ S5rd'Text ts
    , return $ S5rd'Identifier "null"
    , return $ S5rd'Identifier "true"
    , return $ S5rd'Identifier "false"
    , do
      xs <- Gen.list (Range.linear 0 6) $ go $ pred n
      return $ S5rd'Array $ V.fromList xs
    , do
      xs <- Gen.list (Range.linear 2 6) $ do
        k <- keygen
        v <- go $ pred n
        return $ S5rd'KeyValue k v
      return $ S5rd'Array $ V.fromList xs
    ]

-- `serializeText` maps identifier key to unquoted text key.
-- So, it is difficult to prepare this test:
--   Aeson.eitherDecode <=< serializeText === Right . mapS5rdToJson

test_prop_json :: [TestTree]
test_prop_json =
  [ testProperty "parse _ . Aeson.encode === Right . mapJsonToS5rd" $ property $ do
    j <- forAll $ genJsonValue
    let bs = Aeson.encode j
    r <- evalEither $ parseS5rd "" bs
    v <- evalMaybe $ case r of
      ParseS5rdResult'One v -> Just v
      _ -> Nothing
    mapJsonToS5rd j === v
  ]

-- @yaml@ library represents text as single-quoted by default.
-- So, it is difficult to prepare this test:
--   parse _ . Yaml.encode === Right . mapJsonToS5rd

test_prop_yaml :: [TestTree]
test_prop_yaml =
  [ testProperty "Yaml.decodeEither' <=< prettyText === Right . mapS5rdToJson" $ property $ do
    s <- forAll $ s5rd'genYaml
    ts <- evalEither $ prettyTextS5rd s
    r <- evalEither $ Yaml.decodeEither' $ TE.encodeUtf8 $ TL.toStrict ts
    mapS5rdToJson s === Just r
  ]
