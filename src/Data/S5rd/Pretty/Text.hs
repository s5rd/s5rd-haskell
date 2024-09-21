{-# LANGUAGE DeriveGeneric #-}

module Data.S5rd.Pretty.Text
  ( PrettyS5rdError'Text
  , PrettyS5rdKeyError'Text
  , prettyTextS5rd
  , prettyTextS5rdKey
  , prettyTextS5rd'
  , prettyTextS5rdKey'
  ) where

import Data.Bifunctor (first)
import Data.List (intersperse)
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Generic.Data (Generic)

import Data.S5rd.Serialize.Text
import Data.S5rd.Type
import Data.S5rd.Util

data PrettyS5rdKeyError'Text
  = PrettyS5rdKeyError'Text'InvalidIdentifier
  | PrettyS5rdKeyError'Text'NegativeNumber
  deriving (Generic, Eq, Show)

data PrettyS5rdError'Text
  = PrettyS5rdError'Text'InvalidIdentifier
  | PrettyS5rdError'Text'NegativeNumber
  | PrettyS5rdError'Text'KeyError PrettyS5rdKeyError'Text
  deriving (Generic, Eq, Show)

mapErrorSerializeToPrettyKey :: SerializeS5rdKeyError'Text
                             -> PrettyS5rdKeyError'Text
mapErrorSerializeToPrettyKey SerializeS5rdKeyError'Text'InvalidIdentifier
  = PrettyS5rdKeyError'Text'InvalidIdentifier
mapErrorSerializeToPrettyKey SerializeS5rdKeyError'Text'NegativeNumber
  = PrettyS5rdKeyError'Text'NegativeNumber
mapErrorSerializeToPretty :: SerializeS5rdError'Text -> PrettyS5rdError'Text
mapErrorSerializeToPretty SerializeS5rdError'Text'InvalidIdentifier
  = PrettyS5rdError'Text'InvalidIdentifier
mapErrorSerializeToPretty SerializeS5rdError'Text'NegativeNumber
  = PrettyS5rdError'Text'NegativeNumber
mapErrorSerializeToPretty (SerializeS5rdError'Text'KeyError ek)
  = PrettyS5rdError'Text'KeyError $ mapErrorSerializeToPrettyKey ek

prettyTextS5rdKey' :: S5rdKey -> Either PrettyS5rdKeyError'Text TB.Builder
prettyTextS5rdKey' = first mapErrorSerializeToPrettyKey . serializeTextS5rdKey'

prettyTextS5rd' :: Int -> S5rd -> Either PrettyS5rdError'Text TB.Builder
prettyTextS5rd' n (S5rd'Array xs) =
  let indent = TB.fromString $ '\n' : replicate n ' '
      condition = V.length xs >= 2 && V.all s5rd'isKeyValue xs
      ni = if condition then n else n + 2
   in if V.null xs then return $ TB.fromString "[]" else do
    ys <- V.forM xs $ prettyTextS5rd' ni
    return $ mconcat $ intersperse indent $ V.toList
      $ if condition then ys else V.map (TB.fromString "- " <>) ys
prettyTextS5rd' n (S5rd'KeyValue k v) = do
  kr <- first PrettyS5rdError'Text'KeyError $ prettyTextS5rdKey' k
  if s5rd'isCompound v
    then do
      let ni = n + 2
      vr <- prettyTextS5rd' ni v
      return $ kr <> TB.fromString (":\n" ++ replicate ni ' ') <> vr
    else do
      vr <- prettyTextS5rd' n v
      return $ kr <> TB.fromString ": " <> vr
prettyTextS5rd' _ s = first mapErrorSerializeToPretty $ serializeTextS5rd' s

prettyTextS5rdKey :: S5rdKey -> Either PrettyS5rdKeyError'Text TL.Text
prettyTextS5rdKey = fmap TB.toLazyText . prettyTextS5rdKey'
prettyTextS5rd :: S5rd -> Either PrettyS5rdError'Text TL.Text
prettyTextS5rd = fmap TB.toLazyText . prettyTextS5rd' 0
