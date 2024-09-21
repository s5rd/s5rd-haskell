{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.S5rd.Encode where

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Generic.Data

import Data.S5rd.Serialize
import Data.S5rd.To

data EncodeS5rdError'Binary a
  = EncodeS5rdError'Binary'ToError (ToS5rdError a)
  | EncodeS5rdError'Binary'SerializeError SerializeS5rdError'Binary
  deriving (Generic)
instance Eq (ToS5rdError a) => Eq (EncodeS5rdError'Binary a) where
  (==) = geq
instance Show (ToS5rdError a) => Show (EncodeS5rdError'Binary a) where
  showsPrec = gshowsPrec

data EncodeS5rdError'Text a
  = EncodeS5rdError'Text'ToError (ToS5rdError a)
  | EncodeS5rdError'Text'SerializeError SerializeS5rdError'Text
  deriving (Generic)
instance Eq (ToS5rdError a) => Eq (EncodeS5rdError'Text a) where
  (==) = geq
instance Show (ToS5rdError a) => Show (EncodeS5rdError'Text a) where
  showsPrec = gshowsPrec

encodeBinaryS5rd :: ToS5rd a => a
                 -> Either (EncodeS5rdError'Binary a) BL.ByteString
encodeBinaryS5rd x = do
  v <- first EncodeS5rdError'Binary'ToError $ toS5rd x
  first EncodeS5rdError'Binary'SerializeError $ serializeBinaryS5rd v

encodeTextS5rd :: ToS5rd a => a
               -> Either (EncodeS5rdError'Text a) TL.Text
encodeTextS5rd x = do
  v <- first EncodeS5rdError'Text'ToError $ toS5rd x
  first EncodeS5rdError'Text'SerializeError $ serializeTextS5rd v
