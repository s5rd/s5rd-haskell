module Data.S5rd.Util where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Data.S5rd.Type

s5rd'isKeyValue :: S5rd -> Bool
s5rd'isKeyValue (S5rd'KeyValue _ _) = True
s5rd'isKeyValue _ = False
s5rd'isCompound :: S5rd -> Bool
s5rd'isCompound (S5rd'Array _) = True
s5rd'isCompound (S5rd'KeyValue _ _) = True
s5rd'isCompound _ = False

isHeadIdentifierChar :: Char -> Bool
isHeadIdentifierChar c = Char.isAscii c
  && (Char.isAlphaNum c || c `elem` ['.'])
isHeadIdentifierCharNonNum :: Char -> Bool
isHeadIdentifierCharNonNum c = Char.isAscii c
  && (Char.isAlpha c || c `elem` ['.'])
isIdentifierChar :: Char -> Bool
isIdentifierChar c = Char.isAscii c
  && (Char.isAlphaNum c || c `elem` ['-', '.', '_'])

s5rd'isValidIdentifierB :: B.ByteString -> Bool
s5rd'isValidIdentifierB bs = case TE.decodeUtf8' bs of
  Left _e -> False
  Right ts -> s5rd'isValidIdentifierT ts

s5rd'isValidIdentifierT :: T.Text -> Bool
s5rd'isValidIdentifierT ts = case T.uncons ts of
  Nothing -> False
  Just (c, tts) -> isHeadIdentifierChar c && T.all isIdentifierChar tts

s5rd'isValidIdentifierSBS :: SBS.ShortByteString -> Bool
s5rd'isValidIdentifierSBS = s5rd'isValidIdentifierB . SBS.fromShort

s5rd'isValidKey :: S5rdKey -> Bool
s5rd'isValidKey (S5rdKey'Number x) = x >= 0
s5rd'isValidKey (S5rdKey'Binary _bs) = True
s5rd'isValidKey (S5rdKey'Identifier sbs) = s5rd'isValidIdentifierSBS sbs
s5rd'isValid :: S5rd -> Bool
s5rd'isValid (S5rd'Number x) = x >= 0
s5rd'isValid (S5rd'Binary _bs) = True
s5rd'isValid (S5rd'Text _ts) = True
s5rd'isValid (S5rd'Identifier sbs) = s5rd'isValidIdentifierSBS sbs
s5rd'isValid (S5rd'Array xs) = V.all s5rd'isValid xs
s5rd'isValid (S5rd'KeyValue k v) = s5rd'isValidKey k && s5rd'isValid v

s5rd'canonicalize :: S5rd -> S5rd
s5rd'canonicalize (S5rd'Array xs) = S5rd'Array $ V.map s5rd'canonicalize' xs
 where
  s5rd'canonicalize' (S5rd'Array ys) = S5rd'Array $ V.map s5rd'canonicalize' ys
  s5rd'canonicalize' (S5rd'KeyValue k v) = S5rd'KeyValue k $ s5rd'canonicalize v
  s5rd'canonicalize' v = v
s5rd'canonicalize (S5rd'KeyValue k v)
  = S5rd'Array $ V.singleton $ S5rd'KeyValue k $ s5rd'canonicalize v
s5rd'canonicalize v = v
