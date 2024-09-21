{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.S5rd.Decode where

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Generic.Data

import Data.S5rd.From
import Data.S5rd.Parse

data DecodeS5rdError a
  = DecodeS5rdError'ParseError ParseS5rdError
  | DecodeS5rdError'FromError (FromS5rdError a)
  deriving (Generic)
instance Eq (FromS5rdError a) => Eq (DecodeS5rdError a) where
  (==) = geq
instance Show (FromS5rdError a) => Show (DecodeS5rdError a) where
  showsPrec = gshowsPrec

data DecodeS5rdResult a
  = DecodeS5rdResult'One a
  | DecodeS5rdResult'Multiple (V.Vector a)
  deriving (Generic)
instance Eq a => Eq (DecodeS5rdResult a) where
  (==) = geq
instance Show a => Show (DecodeS5rdResult a) where
  showsPrec = gshowsPrec

decodeS5rd :: FromS5rd a
           => String -> BL.ByteString
           -> Either (DecodeS5rdError a) (DecodeS5rdResult a)
decodeS5rd filename bs = do
  r <- first DecodeS5rdError'ParseError $ parseS5rd filename bs
  case r of
    ParseS5rdResult'One v -> do
      x <- first DecodeS5rdError'FromError $ fromS5rd v
      return $ DecodeS5rdResult'One x
    ParseS5rdResult'Multiple vs -> do
      xs <- V.forM vs $ first DecodeS5rdError'FromError . fromS5rd
      return $ DecodeS5rdResult'Multiple xs
