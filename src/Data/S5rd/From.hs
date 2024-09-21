{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.S5rd.From
  ( FromS5rd(..)
  , FromS5rdKey(..)
  , FromS5rdError'Common(..)
  , FromS5rdError'Number(..)
  , FromS5rdError'List(..)
  , FromS5rdError'Tuple(..)
  , FromS5rdError'Map(..)
  , FromS5rdKeyError'Common(..)
  , FromS5rdKeyError'Number(..)
  ) where

import Data.Bifunctor (Bifunctor(first))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Kind (Type)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Void (Void)
import Generic.Data

import Data.S5rd.Type

class FromS5rd a where
  type FromS5rdError a :: Type
  fromS5rd :: S5rd -> Either (FromS5rdError a) a
class Ord a => FromS5rdKey a where
  type FromS5rdKeyError a :: Type
  fromS5rdKey :: S5rdKey -> Either (FromS5rdKeyError a) a

instance FromS5rd S5rd where
  type FromS5rdError S5rd = Void
  fromS5rd = Right

data FromS5rdError'Common
  = FromS5rdError'Common'InvalidValue
  deriving (Generic, Eq, Show)

instance FromS5rd BS.ByteString where
  type FromS5rdError BS.ByteString = FromS5rdError'Common
  fromS5rd (S5rd'Binary bs) = Right bs
  fromS5rd (S5rd'Identifier sbs) = Right $ SBS.fromShort sbs
  fromS5rd _ = Left FromS5rdError'Common'InvalidValue
instance FromS5rd BL.ByteString where
  type FromS5rdError BL.ByteString = FromS5rdError'Common
  fromS5rd (S5rd'Binary bs) = Right $ BL.fromStrict bs
  fromS5rd (S5rd'Identifier sbs) = Right $ BL.fromStrict $ SBS.fromShort sbs
  fromS5rd _ = Left FromS5rdError'Common'InvalidValue
instance FromS5rd SBS.ShortByteString where
  type FromS5rdError SBS.ShortByteString = FromS5rdError'Common
  fromS5rd (S5rd'Binary bs) = Right $ SBS.toShort bs
  fromS5rd (S5rd'Identifier sbs) = Right sbs
  fromS5rd _ = Left FromS5rdError'Common'InvalidValue
instance FromS5rd TS.Text where
  type FromS5rdError TS.Text = FromS5rdError'Common
  fromS5rd (S5rd'Text ts) = Right ts
  fromS5rd (S5rd'Identifier sbs) = Right $ TE.decodeUtf8 $ SBS.fromShort sbs
  fromS5rd _ = Left FromS5rdError'Common'InvalidValue
instance FromS5rd TL.Text where
  type FromS5rdError TL.Text = FromS5rdError'Common
  fromS5rd (S5rd'Text ts) = Right $ TL.fromStrict ts
  fromS5rd (S5rd'Identifier sbs) =
    Right $ TL.fromStrict $ TE.decodeUtf8 $ SBS.fromShort sbs
  fromS5rd _ = Left FromS5rdError'Common'InvalidValue

data FromS5rdError'Number
  = FromS5rdError'Number'InvalidValue
  | FromS5rdError'Number'Overflow
  deriving (Generic, Eq, Show)

convertToInt :: e -> Integer -> Either e Int
convertToInt e x = if fromIntegral (maxBound :: Int) < x
  then Left e
  else Right $ fromIntegral x

instance FromS5rd Integer where
  type FromS5rdError Integer = FromS5rdError'Common
  fromS5rd (S5rd'Number x) = Right x
  fromS5rd _ = Left FromS5rdError'Common'InvalidValue
instance FromS5rd Int where
  type FromS5rdError Int = FromS5rdError'Number
  fromS5rd (S5rd'Number x) = convertToInt FromS5rdError'Number'Overflow x
  fromS5rd _ = Left FromS5rdError'Number'InvalidValue

data FromS5rdError'List a
  = FromS5rdError'List'InvalidValue
  | FromS5rdError'List'ElementError (FromS5rdError a)
  deriving (Generic)
instance Eq (FromS5rdError a) => Eq (FromS5rdError'List a) where
  (==) = geq
instance Show (FromS5rdError a) => Show (FromS5rdError'List a) where
  showsPrec = gshowsPrec

instance FromS5rd a => FromS5rd [a] where
  type FromS5rdError [a] = FromS5rdError'List a
  fromS5rd (S5rd'Array xs) = fmap V.toList $ V.forM xs $ \x ->
    first FromS5rdError'List'ElementError $ fromS5rd x
  fromS5rd _ = Left FromS5rdError'List'InvalidValue
instance FromS5rd a => FromS5rd (V.Vector a) where
  type FromS5rdError (V.Vector a) = FromS5rdError'List a
  fromS5rd (S5rd'Array xs) = V.forM xs $ \x ->
    first FromS5rdError'List'ElementError $ fromS5rd x
  fromS5rd _ = Left FromS5rdError'List'InvalidValue

instance FromS5rdKey S5rdKey where
  type FromS5rdKeyError S5rdKey = Void
  fromS5rdKey = Right

data FromS5rdKeyError'Common
  = FromS5rdKeyError'Common'InvalidValue
  deriving (Generic, Eq, Show)

instance FromS5rdKey BS.ByteString where
  type FromS5rdKeyError BS.ByteString = FromS5rdKeyError'Common
  fromS5rdKey (S5rdKey'Binary bs) = Right bs
  fromS5rdKey (S5rdKey'Identifier sbs) = Right $ SBS.fromShort sbs
  fromS5rdKey _ = Left FromS5rdKeyError'Common'InvalidValue
instance FromS5rdKey BL.ByteString where
  type FromS5rdKeyError BL.ByteString = FromS5rdKeyError'Common
  fromS5rdKey (S5rdKey'Binary bs) = Right $ BL.fromStrict bs
  fromS5rdKey (S5rdKey'Identifier sbs) = Right $ BL.fromStrict $ SBS.fromShort sbs
  fromS5rdKey _ = Left FromS5rdKeyError'Common'InvalidValue
instance FromS5rdKey SBS.ShortByteString where
  type FromS5rdKeyError SBS.ShortByteString = FromS5rdKeyError'Common
  fromS5rdKey (S5rdKey'Binary bs) = Right $ SBS.toShort bs
  fromS5rdKey (S5rdKey'Identifier sbs) = Right sbs
  fromS5rdKey _ = Left FromS5rdKeyError'Common'InvalidValue
instance FromS5rdKey TS.Text where
  type FromS5rdKeyError TS.Text = FromS5rdKeyError'Common
  fromS5rdKey (S5rdKey'Identifier sbs) = Right $ TE.decodeUtf8 $ SBS.fromShort sbs
  fromS5rdKey _ = Left FromS5rdKeyError'Common'InvalidValue
instance FromS5rdKey TL.Text where
  type FromS5rdKeyError TL.Text = FromS5rdKeyError'Common
  fromS5rdKey (S5rdKey'Identifier sbs) =
    Right $ TL.fromStrict $ TE.decodeUtf8 $ SBS.fromShort sbs
  fromS5rdKey _ = Left FromS5rdKeyError'Common'InvalidValue

data FromS5rdKeyError'Number
  = FromS5rdKeyError'Number'InvalidValue
  | FromS5rdKeyError'Number'Overflow
  deriving (Generic, Eq, Show)

instance FromS5rdKey Integer where
  type FromS5rdKeyError Integer = FromS5rdKeyError'Common
  fromS5rdKey (S5rdKey'Number x) = Right x
  fromS5rdKey _ = Left FromS5rdKeyError'Common'InvalidValue
instance FromS5rdKey Int where
  type FromS5rdKeyError Int = FromS5rdKeyError'Number
  fromS5rdKey (S5rdKey'Number x) = convertToInt FromS5rdKeyError'Number'Overflow x
  fromS5rdKey _ = Left FromS5rdKeyError'Number'InvalidValue

data FromS5rdError'Tuple k v
  = FromS5rdError'Tuple'InvalidValue
  | FromS5rdError'Tuple'KeyError (FromS5rdKeyError k)
  | FromS5rdError'Tuple'ValueError (FromS5rdError v)
  deriving (Generic)
instance (Eq (FromS5rdKeyError k), Eq (FromS5rdError v))
  => Eq (FromS5rdError'Tuple k v) where
    (==) = geq
instance (Show (FromS5rdKeyError k), Show (FromS5rdError v))
  => Show (FromS5rdError'Tuple k v) where
    showsPrec = gshowsPrec

instance (FromS5rdKey k, FromS5rd v) => FromS5rd (k, v) where
  type FromS5rdError (k, v) = FromS5rdError'Tuple k v
  fromS5rd (S5rd'KeyValue x y) = do
    xr <- first FromS5rdError'Tuple'KeyError $ fromS5rdKey x
    yr <- first FromS5rdError'Tuple'ValueError $ fromS5rd y
    return (xr, yr)
  fromS5rd _ = Left FromS5rdError'Tuple'InvalidValue

data FromS5rdError'Map k v
  = FromS5rdError'Map'InvalidValue
  | FromS5rdError'Map'KeyError (FromS5rdKeyError k)
  | FromS5rdError'Map'ValueError (FromS5rdError v)
  deriving (Generic)
instance (Eq (FromS5rdKeyError k), Eq (FromS5rdError v))
  => Eq (FromS5rdError'Map k v) where
    (==) = geq
instance (Show (FromS5rdKeyError k), Show (FromS5rdError v))
  => Show (FromS5rdError'Map k v) where
    showsPrec = gshowsPrec

errorTupleToMap :: FromS5rdError'Tuple k v -> FromS5rdError'Map k v
errorTupleToMap FromS5rdError'Tuple'InvalidValue
  = FromS5rdError'Map'InvalidValue
errorTupleToMap (FromS5rdError'Tuple'KeyError ek)
  = FromS5rdError'Map'KeyError ek
errorTupleToMap (FromS5rdError'Tuple'ValueError ev)
  = FromS5rdError'Map'ValueError ev

instance FromS5rd v => FromS5rd (IntMap.IntMap v) where
  type FromS5rdError (IntMap.IntMap v) = FromS5rdError'Map IntMap.Key v
  fromS5rd (S5rd'Array xs) =
    fmap (IntMap.fromList . V.toList)
    $ V.forM xs $ first errorTupleToMap . fromS5rd
  fromS5rd _ = Left FromS5rdError'Map'InvalidValue
instance (FromS5rdKey k, FromS5rd v) => FromS5rd (Map.Map k v) where
  type FromS5rdError (Map.Map k v) = FromS5rdError'Map k v
  fromS5rd (S5rd'Array xs) =
    fmap (Map.fromList . V.toList)
    $ V.forM xs $ first errorTupleToMap . fromS5rd
  fromS5rd _ = Left FromS5rdError'Map'InvalidValue
