{-# LANGUAGE DeriveGeneric #-}

module Data.S5rd.Serialize.Binary
  ( SerializeS5rdError'Binary
  , SerializeS5rdKeyError'Binary
  , serializeBinaryS5rd
  , serializeBinaryS5rdKey
  ) where

import Data.Bits
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8)
import Generic.Data (Generic)

import Data.S5rd.Type
import Data.S5rd.Util

data SerializeS5rdKeyError'Binary
  = SerializeS5rdKeyError'Binary'TooLongBinary
  | SerializeS5rdKeyError'Binary'InvalidIdentifier
  | SerializeS5rdKeyError'Binary'TooLongIdentifier
  | SerializeS5rdKeyError'Binary'NegativeNumber
  deriving (Generic, Eq, Show)

data SerializeS5rdError'Binary
  = SerializeS5rdError'Binary'TooLongBinary
  | SerializeS5rdError'Binary'InvalidIdentifier
  | SerializeS5rdError'Binary'TooLongIdentifier
  | SerializeS5rdError'Binary'TooLongText
  | SerializeS5rdError'Binary'NegativeNumber
  | SerializeS5rdError'Binary'TooLargeArray
  | SerializeS5rdError'Binary'KeyError SerializeS5rdKeyError'Binary
  deriving (Generic, Eq, Show)

word24BE :: Int -> BB.Builder
word24BE x = BB.word8 (fromIntegral $ x `shiftR` 16)
  <> BB.word8 (fromIntegral $ x `shiftR` 8)
  <> BB.word8 (fromIntegral x)

byteStringEscapeSequenceWithPad :: Word8 -> BS.ByteString -> BB.Builder
byteStringEscapeSequenceWithPad x = go . BS.unpack
 where
  go [] = mempty
  go [w0] = if w0 `elem` xs
    then BB.word8 r <> BB.word8 0 <> BB.word8 0 <> BB.word8 0
      <> BB.word8 w0 <> BB.word8 0 <> BB.word8 0 <> BB.word8 0
    else BB.word8 w0 <> BB.word8 0 <> BB.word8 0 <> BB.word8 0
  go [w0,w1] = if w0 `elem` xs
    then BB.word8 r <> BB.word8 w1 <> BB.word8 0 <> BB.word8 0
      <> BB.word8 w0 <> BB.word8 0 <> BB.word8 0 <> BB.word8 0
    else BB.word8 w0 <> BB.word8 w1 <> BB.word8 0 <> BB.word8 0
  go [w0,w1,w2] = if w0 `elem` xs
    then BB.word8 r <> BB.word8 w1 <> BB.word8 w2 <> BB.word8 0
      <> BB.word8 w0 <> BB.word8 0 <> BB.word8 0 <> BB.word8 0
    else BB.word8 w0 <> BB.word8 w1 <> BB.word8 w2 <> BB.word8 0
  go (w0:w1:w2:w3:ws) = if w0 `elem` xs
    then BB.word8 r <> BB.word8 w1 <> BB.word8 w2 <> BB.word8 w3
      <> BB.word8 w0 <> BB.word8 0 <> BB.word8 0 <> BB.word8 0 <> go ws
    else BB.word8 w0 <> BB.word8 w1 <> BB.word8 w2 <> BB.word8 w3 <> go ws
  -- xs = [0xA1, 0xA2, 0xA4, 0xA6, 0xA7, r]
  xs = x : [r]
  r = 0xDC

serializeKey' :: S5rdKey -> Either SerializeS5rdKeyError'Binary BB.Builder
serializeKey' (S5rdKey'Binary bs)
  | BS.null bs = Right $ BB.byteString $ BS.pack [0xAE, 0x00, 0x00, 0x00]
  | BS.length bs <= 11 =
    let l = BS.length bs
        pad = replicate ((11 - l) `mod` 4) 0x00
        h = 0xF0 - 1 + fromIntegral l
     in Right $ BB.word8 h <> BB.byteString bs <> BB.byteString (BS.pack pad)
  | BS.length bs <= 0xFFFFFF =
    let l = BS.length bs
        h = 0xA6
        beginend = BB.word8 h <> word24BE l
     in Right $ beginend
          <> byteStringEscapeSequenceWithPad 0xA6 bs
          <> beginend
  | otherwise = Left SerializeS5rdKeyError'Binary'TooLongBinary
serializeKey' (S5rdKey'Identifier sbs)
  | SBS.null sbs = Left SerializeS5rdKeyError'Binary'InvalidIdentifier
  | SBS.length sbs <= 15 =
    let l = SBS.length sbs
        pad = replicate ((15 - l) `mod` 4) 0x00
        h = 0xE1 - 1 + fromIntegral l
     in Right $ BB.word8 h <> BB.shortByteString sbs <> BB.byteString (BS.pack pad)
  | SBS.length sbs <= 0xFFFFFF =
    let l = SBS.length sbs
        h = 0xA1
        beginend = BB.word8 h <> word24BE l
     in Right $ beginend
          <> byteStringEscapeSequenceWithPad 0xA1 (SBS.fromShort sbs)
          <> beginend
  | otherwise = Left SerializeS5rdKeyError'Binary'TooLongIdentifier
serializeKey' (S5rdKey'Number x)
  | x <= 0xFFFFFF = Right $ BB.byteString $ BS.pack
    [ 0xB1
    , fromIntegral $ x `shiftR` 16
    , fromIntegral $ x `shiftR` 8
    , fromIntegral x
    ]
  | x <= 0xFFFFFFFFFFFFFF = Right $ BB.byteString $ BS.pack
    [ 0xB3
    , fromIntegral $ x `shiftR` 48
    , fromIntegral $ x `shiftR` 40
    , fromIntegral $ x `shiftR` 32
    , fromIntegral $ x `shiftR` 24
    , fromIntegral $ x `shiftR` 16
    , fromIntegral $ x `shiftR` 8
    , fromIntegral x
    ]
  | otherwise = error "serializeKey: not implemented"

serialize' :: S5rd -> Either SerializeS5rdError'Binary BB.Builder
serialize' (S5rd'Binary bs)
  | BS.null bs = Right $ BB.byteString $ BS.pack [0xAF, 0x00, 0x00, 0x00]
  | BS.length bs <= 11 =
    let l = BS.length bs
        pad = replicate ((11 - l) `mod` 4) 0x00
        h = 0xD0 - 1 + fromIntegral l
     in Right $ BB.word8 h <> BB.byteString bs <> BB.byteString (BS.pack pad)
  | BS.length bs <= 0xFFFFFF =
    let l = BS.length bs
        h = 0xA7
        beginend = BB.word8 h <> word24BE l
     in Right $ beginend
          <> byteStringEscapeSequenceWithPad 0xA7 bs
          <> beginend
  | otherwise = Left SerializeS5rdError'Binary'TooLongBinary
serialize' (S5rd'Identifier sbs)
  | SBS.null sbs = Left SerializeS5rdError'Binary'InvalidIdentifier
  | SBS.length sbs <= 15 =
    let l = SBS.length sbs
        pad = replicate ((15 - l) `mod` 4) 0x00
        h = 0xC1 - 1 + fromIntegral l
     in Right $ BB.word8 h <> BB.shortByteString sbs <> BB.byteString (BS.pack pad)
  | SBS.length sbs <= 0xFFFFFF =
    let l = SBS.length sbs
        h = 0xA2
        beginend = BB.word8 h <> word24BE l
     in Right $ beginend
          <> byteStringEscapeSequenceWithPad 0xA2 (SBS.fromShort sbs)
          <> beginend
  | otherwise = Left SerializeS5rdError'Binary'TooLongIdentifier
serialize' (S5rd'Number x)
  | x <= 0xFFFFFF = Right $ BB.byteString $ BS.pack
    [ 0xB0
    , fromIntegral $ x `shiftR` 16
    , fromIntegral $ x `shiftR` 8
    , fromIntegral x
    ]
  | x <= 0xFFFFFFFFFFFFFF = Right $ BB.byteString $ BS.pack
    [ 0xB2
    , fromIntegral $ x `shiftR` 48
    , fromIntegral $ x `shiftR` 40
    , fromIntegral $ x `shiftR` 32
    , fromIntegral $ x `shiftR` 24
    , fromIntegral $ x `shiftR` 16
    , fromIntegral $ x `shiftR` 8
    , fromIntegral x
    ]
  | otherwise = error "serialize: not implemented"
serialize' (S5rd'Text ts)
  | T.null ts = Right $ BB.byteString $ BS.pack [0xAC, 0x00, 0x00, 0x00]
  | otherwise =
    let bs = TE.encodeUtf8 ts
        l = BS.length bs
        h = 0xA4
        beginend = BB.word8 h <> word24BE l
     in if l <= 0xFFFFFF
          then Right $ beginend
            <> byteStringEscapeSequenceWithPad 0xA4 (TE.encodeUtf8 ts)
            <> beginend
          else Left SerializeS5rdError'Binary'TooLongText
serialize' (S5rd'Array xs)
  | V.length xs <= 0xFFFFFF && V.all s5rd'isKeyValue xs = do
    let l = V.length xs
        h = 0xFB
        t = 0xFD
        begin = BB.word8 h <> word24BE l
        end = BB.word8 t <> word24BE l
    ys <- mapM serialize' $ V.toList xs
    return $ begin <> mconcat ys <> end
  | V.length xs <= 0xFFFFFF = do
    let l = V.length xs
        h = 0xDB
        t = 0xDD
        begin = BB.word8 h <> word24BE l
        end = BB.word8 t <> word24BE l
    ys <- mapM serialize' $ V.toList xs
    return $ begin <> mconcat ys <> end
  | otherwise = Left SerializeS5rdError'Binary'TooLargeArray
serialize' (S5rd'KeyValue k v) = do
  kr <- first SerializeS5rdError'Binary'KeyError (serializeKey' k)
  vr <- serialize' v
  return $ kr <> vr

serializeBinaryS5rdKey :: S5rdKey
                       -> Either SerializeS5rdKeyError'Binary BL.ByteString
serializeBinaryS5rdKey = fmap BB.toLazyByteString . serializeKey'

serializeBinaryS5rd :: S5rd
                    -> Either SerializeS5rdError'Binary BL.ByteString
serializeBinaryS5rd = fmap BB.toLazyByteString . serialize'
-- serializeBinaryS5rd = fmap (traceWith f . BB.toLazyByteString) . serialize'
--  where
--   f = T.unpack . TE.decodeUtf8 . BL.toStrict
--     . BB.toLazyByteString . BB.lazyByteStringHex
