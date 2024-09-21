module Data.S5rd
  ( module X
  , from
  , fromKey
  , to
  , toKey
  , decode
  , encodeBinary
  , encodeText
  , parse
  , prettyText
  , serializeBinary
  , serializeText
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL

import Data.S5rd.Decode as X
import Data.S5rd.Encode as X
import Data.S5rd.From as X
import Data.S5rd.Parse as X
import Data.S5rd.Pretty.Text as X
import Data.S5rd.Serialize as X
import Data.S5rd.To as X
import Data.S5rd.Type as X
import Data.S5rd.Util as X

from :: FromS5rd a => S5rd -> Either (FromS5rdError a) a
from = fromS5rd

fromKey :: FromS5rdKey a => S5rdKey -> Either (FromS5rdKeyError a) a
fromKey = fromS5rdKey

to :: ToS5rd a => a -> Either (ToS5rdError a) S5rd
to = toS5rd

toKey :: ToS5rdKey a => a -> Either (ToS5rdKeyError a) S5rdKey
toKey = toS5rdKey

decode :: FromS5rd a
       => String -> BL.ByteString
       -> Either (DecodeS5rdError a) (DecodeS5rdResult a)
decode = decodeS5rd

encodeBinary :: ToS5rd a => a
             -> Either (EncodeS5rdError'Binary a) BL.ByteString
encodeBinary = encodeBinaryS5rd

encodeText :: ToS5rd a => a
           -> Either (EncodeS5rdError'Text a) TL.Text
encodeText = encodeTextS5rd

parse :: String -> BL.ByteString -> Either ParseS5rdError ParseS5rdResult
parse = parseS5rd

prettyText :: S5rd -> Either PrettyS5rdError'Text TL.Text
prettyText = prettyTextS5rd

serializeBinary :: S5rd
                -> Either SerializeS5rdError'Binary BL.ByteString
serializeBinary = serializeBinaryS5rd

serializeText :: S5rd -> Either SerializeS5rdError'Text TL.Text
serializeText = serializeTextS5rd
