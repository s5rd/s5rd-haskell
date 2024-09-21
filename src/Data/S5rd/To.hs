{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.S5rd.To
  ( ToS5rd(..)
  , ToS5rdKey(..)
  , ToS5rdError'Number(..)
  , ToS5rdError'List(..)
  , ToS5rdError'Tuple(..)
  , ToS5rdError'Map(..)
  , ToS5rdKeyError'Number(..)
  , ToS5rdKeyError'Text(..)
  ) where

import Data.Bifunctor (Bifunctor(first))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Kind (Type)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Data.Void (Void)
import Generic.Data

import Data.S5rd.Type
import Data.S5rd.Util

class ToS5rd a where
  type ToS5rdError a :: Type
  toS5rd :: a -> Either (ToS5rdError a) S5rd
class Ord a => ToS5rdKey a where
  type ToS5rdKeyError a :: Type
  toS5rdKey :: a -> Either (ToS5rdKeyError a) S5rdKey

instance ToS5rd S5rd where
  type ToS5rdError S5rd = Void
  toS5rd = Right

instance ToS5rd BS.ByteString where
  type ToS5rdError BS.ByteString = Void
  toS5rd = Right . S5rd'Binary
instance ToS5rd BL.ByteString where
  type ToS5rdError BL.ByteString = Void
  toS5rd = Right . S5rd'Binary . BL.toStrict
instance ToS5rd SBS.ShortByteString where
  type ToS5rdError SBS.ShortByteString = Void
  toS5rd = Right . S5rd'Identifier
instance ToS5rd TS.Text where
  type ToS5rdError TS.Text = Void
  toS5rd = Right . S5rd'Text
instance ToS5rd TL.Text where
  type ToS5rdError TL.Text = Void
  toS5rd = Right . S5rd'Text . TL.toStrict

data ToS5rdError'Number
  = ToS5rdError'Number'Negative
  deriving (Generic, Eq, Show)

instance ToS5rd Integer where
  type ToS5rdError Integer = ToS5rdError'Number
  toS5rd x = if x < 0
    then Left ToS5rdError'Number'Negative
    else Right $ S5rd'Number x
instance ToS5rd Int where
  type ToS5rdError Int = ToS5rdError'Number
  toS5rd x = if x < 0
    then Left ToS5rdError'Number'Negative
    else Right $ S5rd'Number $ fromIntegral x

data ToS5rdError'List a
  = ToS5rdError'List'ElementError (ToS5rdError a)
  deriving (Generic)
instance Eq (ToS5rdError a) => Eq (ToS5rdError'List a) where
  (==) = geq
instance Show (ToS5rdError a) => Show (ToS5rdError'List a) where
  showsPrec = gshowsPrec

instance ToS5rd a => ToS5rd [a] where
  type ToS5rdError [a] = ToS5rdError'List a
  toS5rd xs = S5rd'Array <$> V.forM (V.fromList xs)
    (first ToS5rdError'List'ElementError . toS5rd)
instance ToS5rd a => ToS5rd (V.Vector a) where
  type ToS5rdError (V.Vector a) = ToS5rdError'List a
  toS5rd xs = S5rd'Array <$> V.forM xs
    (first ToS5rdError'List'ElementError . toS5rd)

instance ToS5rdKey S5rdKey where
  type ToS5rdKeyError S5rdKey = Void
  toS5rdKey = Right

data ToS5rdKeyError'Text
  = ToS5rdKeyError'Text'InvalidIdentifier
  deriving (Generic, Eq, Show)

instance ToS5rdKey BS.ByteString where
  type ToS5rdKeyError BS.ByteString = Void
  toS5rdKey = Right . S5rdKey'Binary
instance ToS5rdKey BL.ByteString where
  type ToS5rdKeyError BL.ByteString = Void
  toS5rdKey = Right . S5rdKey'Binary . BL.toStrict
instance ToS5rdKey SBS.ShortByteString where
  type ToS5rdKeyError SBS.ShortByteString = Void
  toS5rdKey = Right . S5rdKey'Identifier
instance ToS5rdKey TS.Text where
  type ToS5rdKeyError TS.Text = ToS5rdKeyError'Text
  toS5rdKey ts = if s5rd'isValidIdentifierT ts
    then Right $ S5rdKey'Identifier $ SBS.toShort $ TE.encodeUtf8 ts
    else Left ToS5rdKeyError'Text'InvalidIdentifier
instance ToS5rdKey TL.Text where
  type ToS5rdKeyError TL.Text = ToS5rdKeyError'Text
  toS5rdKey ts0 = let ts = TL.toStrict ts0 in if s5rd'isValidIdentifierT ts
    then Right $ S5rdKey'Identifier $ SBS.toShort $ TE.encodeUtf8 ts
    else Left ToS5rdKeyError'Text'InvalidIdentifier

data ToS5rdKeyError'Number
  = ToS5rdKeyError'Number'Negative
  deriving (Generic, Eq, Show)

instance ToS5rdKey Integer where
  type ToS5rdKeyError Integer = ToS5rdKeyError'Number
  toS5rdKey x = if x < 0
    then Left ToS5rdKeyError'Number'Negative
    else Right $ S5rdKey'Number x
instance ToS5rdKey Int where
  type ToS5rdKeyError Int = ToS5rdKeyError'Number
  toS5rdKey x = if x < 0
    then Left ToS5rdKeyError'Number'Negative
    else Right $ S5rdKey'Number $ fromIntegral x

data ToS5rdError'Tuple k v
  = ToS5rdError'Tuple'KeyError (ToS5rdKeyError k)
  | ToS5rdError'Tuple'ValueError (ToS5rdError v)
  deriving (Generic)
instance (Eq (ToS5rdKeyError k), Eq (ToS5rdError v))
  => Eq (ToS5rdError'Tuple k v) where
    (==) = geq
instance (Show (ToS5rdKeyError k), Show (ToS5rdError v))
  => Show (ToS5rdError'Tuple k v) where
  showsPrec = gshowsPrec

instance (ToS5rdKey k, ToS5rd v) => ToS5rd (k, v) where
  type ToS5rdError (k, v) = ToS5rdError'Tuple k v
  toS5rd (x, y) = do
    xr <- first ToS5rdError'Tuple'KeyError $ toS5rdKey x
    yr <- first ToS5rdError'Tuple'ValueError $ toS5rd y
    return $ S5rd'KeyValue xr yr

data ToS5rdError'Map k v
  = ToS5rdError'Map'KeyError (ToS5rdKeyError k)
  | ToS5rdError'Map'ValueError (ToS5rdError v)
  deriving (Generic)
instance (Eq (ToS5rdKeyError k), Eq (ToS5rdError v))
  => Eq (ToS5rdError'Map k v) where
    (==) = geq
instance (Show (ToS5rdKeyError k), Show (ToS5rdError v))
  => Show (ToS5rdError'Map k v) where
  showsPrec = gshowsPrec

errorTupleToMap :: ToS5rdError'Tuple k v -> ToS5rdError'Map k v
errorTupleToMap (ToS5rdError'Tuple'KeyError ek)
  = ToS5rdError'Map'KeyError ek
errorTupleToMap (ToS5rdError'Tuple'ValueError ev)
  = ToS5rdError'Map'ValueError ev

instance ToS5rd v => ToS5rd (IntMap.IntMap v) where
  type ToS5rdError (IntMap.IntMap v) = ToS5rdError'Map IntMap.Key v
  toS5rd = fmap (S5rd'Array . V.fromList)
    . mapM (first errorTupleToMap . toS5rd)
    . IntMap.toAscList
instance (ToS5rdKey k, ToS5rd v) => ToS5rd (Map.Map k v) where
  type ToS5rdError (Map.Map k v) = ToS5rdError'Map k v
  toS5rd = fmap (S5rd'Array . V.fromList)
    . mapM (first errorTupleToMap . toS5rd) . Map.toAscList
