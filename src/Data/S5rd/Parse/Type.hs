{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.S5rd.Parse.Type where

import qualified Data.Vector as V
import Generic.Data
import qualified Text.Megaparsec as M

import Data.S5rd.Type

data ParseS5rdParsecError
  = ParseS5rdParsecError'ReachedRecursionLimit
  | ParseS5rdParsecError'MismatchParity
  | ParseS5rdParsecError'InvalidParity
  | ParseS5rdParsecError'IllegalValue
  | ParseS5rdParsecError'InvalidIdentifier
  | ParseS5rdParsecError'InvalidUtf8Text
  | ParseS5rdParsecError'TooLargeNumericCharLiteral
  deriving (Generic, Eq, Ord, Show)

instance M.ShowErrorComponent ParseS5rdParsecError where
  showErrorComponent = show

data ParseS5rdResultInternal s
  = ParseS5rdResultInternal'Empty
  | ParseS5rdResultInternal'One S5rd
  | ParseS5rdResultInternal'Multiple [S5rd]
  | ParseS5rdResultInternal'Error (M.ParseErrorBundle s ParseS5rdParsecError)
  deriving (Generic)
instance (Eq s, M.Stream s, Eq (M.Token s))
  => Eq (ParseS5rdResultInternal s) where
    (==) = geq
instance (Show s, M.Stream s, Show (M.Token s))
  => Show (ParseS5rdResultInternal s) where
    showsPrec = gshowsPrec

data ParseS5rdError
  = ParseS5rdError'Empty
  | ParseS5rdError'InvalidUtf8Text
  | ParseS5rdError'ParsecError
  deriving (Generic, Eq, Show)

data ParseS5rdResult
  = ParseS5rdResult'One S5rd
  | ParseS5rdResult'Multiple (V.Vector S5rd)
  deriving (Generic, Eq, Show)
