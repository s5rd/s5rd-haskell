{-# LANGUAGE DeriveGeneric #-}

module Data.S5rd.Type where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Vector as V
import Generic.Data (Generic)

data S5rdKey
  = S5rdKey'Binary B.ByteString
  | S5rdKey'Identifier SBS.ShortByteString
  | S5rdKey'Number Integer
  deriving (Generic, Eq, Ord, Show)

data S5rd
  = S5rd'Binary B.ByteString
  | S5rd'Identifier SBS.ShortByteString
  | S5rd'Number Integer
  | S5rd'Text T.Text
  | S5rd'Array (V.Vector S5rd)
  | S5rd'KeyValue S5rdKey S5rd
  deriving (Generic, Eq, Ord, Show)
