{-# LANGUAGE DeriveGeneric #-}

module Data.S5rd.Serialize.Text
  ( SerializeS5rdError'Text(..)
  , SerializeS5rdKeyError'Text(..)
  , serializeTextS5rd
  , serializeTextS5rdKey
  , serializeTextS5rd'
  , serializeTextS5rdKey'
  ) where

import Data.Bifunctor (first)
import qualified Data.ByteString.Short as SBS
import qualified Data.Char as Char
import Data.List (intersperse)
import qualified Data.Text as TS
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Generic.Data (Generic)

import Data.S5rd.Internal
import Data.S5rd.Type
import Data.S5rd.Util

data SerializeS5rdKeyError'Text
  = SerializeS5rdKeyError'Text'InvalidIdentifier
  | SerializeS5rdKeyError'Text'NegativeNumber
  deriving (Generic, Eq, Show)

data SerializeS5rdError'Text
  = SerializeS5rdError'Text'InvalidIdentifier
  | SerializeS5rdError'Text'NegativeNumber
  | SerializeS5rdError'Text'KeyError SerializeS5rdKeyError'Text
  deriving (Generic, Eq, Show)

serializeTextS5rdKey' :: S5rdKey -> Either SerializeS5rdKeyError'Text TB.Builder
serializeTextS5rdKey' (S5rdKey'Binary bs) = Right $
  TB.singleton '\'' <> convertBinaryToEscapeSequence bs <> TB.singleton '\''
serializeTextS5rdKey' (S5rdKey'Identifier sbs)
  | not $ s5rd'isValidIdentifierSBS sbs =
    Left SerializeS5rdKeyError'Text'InvalidIdentifier
  | SBS.head sbs `elem` map (fromIntegral . Char.ord) ['0'..'9'] =
    Right $ TB.singleton '`'
      <> (TB.fromText $ TE.decodeUtf8 $ SBS.fromShort sbs)
      <> TB.singleton '`'
  | otherwise = Right $ TB.fromText $ TE.decodeUtf8 $ SBS.fromShort sbs
serializeTextS5rdKey' (S5rdKey'Number x)
  | x < 0 = Left SerializeS5rdKeyError'Text'NegativeNumber
  | otherwise = Right $ TB.fromText $ TS.pack $ show x

serializeTextS5rd' :: S5rd -> Either SerializeS5rdError'Text TB.Builder
serializeTextS5rd' (S5rd'Binary bs) = Right $
  TB.singleton '\'' <> convertBinaryToEscapeSequence bs <> TB.singleton '\''
serializeTextS5rd' (S5rd'Identifier sbs)
  | not $ s5rd'isValidIdentifierSBS sbs =
    Left SerializeS5rdError'Text'InvalidIdentifier
  | SBS.head sbs `elem` map (fromIntegral . Char.ord) ['0'..'9'] =
    Right $ TB.singleton '`'
      <> (TB.fromText $ TE.decodeUtf8 $ SBS.fromShort sbs)
      <> TB.singleton '`'
  | otherwise = Right $ TB.fromText $ TE.decodeUtf8 $ SBS.fromShort sbs
serializeTextS5rd' (S5rd'Number x)
  | x < 0 = Left SerializeS5rdError'Text'NegativeNumber
  | otherwise = Right $ TB.fromText $ TS.pack $ show x
serializeTextS5rd' (S5rd'Text ts) = Right $
  TB.singleton '\"' <> convertTextToEscapeSequence ts <> TB.singleton '\"'
serializeTextS5rd' (S5rd'Array xs) = do
  ys <- mapM serializeTextS5rd' $ V.toList xs
  let (begin, end) = if condition then ('{', '}') else ('[', ']')
      condition = not (V.null xs) && V.all s5rd'isKeyValue xs
  return $ TB.singleton begin
    <> mconcat (intersperse (TB.singleton ',') ys)
    <> TB.singleton end
serializeTextS5rd' (S5rd'KeyValue k v) = do
  kr <- first SerializeS5rdError'Text'KeyError (serializeTextS5rdKey' k)
  vr <- serializeTextS5rd' v
  return $ kr <> TB.singleton ':' <> vr

serializeTextS5rdKey :: S5rdKey -> Either SerializeS5rdKeyError'Text TL.Text
serializeTextS5rdKey = fmap TB.toLazyText . serializeTextS5rdKey'
serializeTextS5rd :: S5rd -> Either SerializeS5rdError'Text TL.Text
serializeTextS5rd = fmap TB.toLazyText . serializeTextS5rd'
