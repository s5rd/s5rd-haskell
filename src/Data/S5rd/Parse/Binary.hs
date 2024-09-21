{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.S5rd.Parse.Binary
  ( module X
  , parseBinaryS5rd
  ) where

import Control.Applicative (asum)
import Control.Arrow ((&&&))
import Control.Monad (replicateM, void)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word
import qualified Text.Megaparsec as M

import Data.S5rd.Internal
import Data.S5rd.Parse.Type as X
import Data.S5rd.Type
import Data.S5rd.Util

toBigEndian32 :: [Word8] -> Word32
toBigEndian32 (w0:w1:w2:w3:_) =
  (fromIntegral w0 `shiftL` 24)
  .|. (fromIntegral w1 `shiftL` 16)
  .|. (fromIntegral w2 `shiftL` 8)
  .|. fromIntegral w3
toBigEndian32 _ = error "toBigEndian32: insufficient length"
head8from32 :: Word32 -> Word8
head8from32 w = fromIntegral $ w `shiftR` 24
tail24from32 :: Word32 -> Int
tail24from32 w = fromIntegral $ w .&. 0xFFFFFF
headtail32FromTokens :: forall s. (M.Stream s, M.Token s ~ Word8)
                     => Proxy s -> M.Tokens s -> (Word8, Int)
headtail32FromTokens proxy =
  (head8from32 &&& tail24from32) . toBigEndian32 . M.chunkToTokens proxy
sequenceFromHead24Tail32 :: Int -> Int -> [Word32] -> [Word8]
sequenceFromHead24Tail32 n h ts =
  take n $ drop 1 $ reverse $ foldl f [] $ fromIntegral h : ts
 where
  f acc x = d : c : b : a : acc
   where
    a = fromIntegral $ x `shiftR` 24
    b = fromIntegral $ x `shiftR` 16
    c = fromIntegral $ x `shiftR` 8
    d = fromIntegral x

binaryEscapeSequence :: forall e s m.
  (M.MonadParsec e s m, M.Stream s, M.Token s ~ Word8)
  => Proxy s -> Word8 -> Word8 -> m (M.Tokens s, Int)
binaryEscapeSequence proxy end escape = go []
 where
  go xs = do
    (h0, w0) <- (head8from32 &&& id) . toBigEndian32
      . M.chunkToTokens proxy
      <$> M.takeP Nothing 4
    let t0 = fromIntegral $ w0 .&. 0xFFFFFF
    if h0 == end
      then return (M.tokensToChunk proxy $ reverse xs, t0)
      else if h0 /= escape
        then go (reverse (toBigEndianByteSequence w0) ++ xs)
        else do
          (h1, _t) <- headtail32FromTokens proxy
            <$> M.takeP Nothing 4
          let w = (w0 .&. 0xFFFFFF) .|. (fromIntegral h1 `shiftL` 24)
          go (reverse (toBigEndianByteSequence w) ++ xs)

parsecBinaryS5rd' :: forall e s m.
  (M.MonadParsec e s m, M.Stream s, M.Token s ~ Word8, e ~ ParseS5rdParsecError)
  => Proxy s -> m S5rd -> m S5rd
parsecBinaryS5rd' proxy m = asum $ map M.try
  [ do
    void $ M.chunk $ M.tokensToChunk proxy [0xAA, 0xAA, 0xAA, 0x8A]
    S5rd'Array . V.fromList <$> repeatParsec m
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xDB
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xDB
      else do
        x <- S5rd'Array . V.fromList <$> replicateM t0 m
        (h1, t1) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
        if h1 /= 0xDD
          then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xDD
          else if t0 /= t1
            then M.customFailure ParseS5rdParsecError'MismatchParity
            else return x
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xFB
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xFB
      else do
        xs <- V.fromList <$> replicateM t0 m
        (h1, t1) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
        let x = S5rd'Array xs
            condition = V.all s5rd'isKeyValue xs
        if h1 /= 0xFD
          then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xFD
          else if t0 /= t1
            then M.customFailure ParseS5rdParsecError'MismatchParity
            else if not condition
              then M.customFailure ParseS5rdParsecError'IllegalValue
              else return x
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xB0
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xB0
      else return $ S5rd'Number $ fromIntegral t0
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xB1
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xB1
      else do
        v <- m
        return $ S5rd'KeyValue (S5rdKey'Number $ fromIntegral t0) v
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xB2
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xB2
      else do
        t1 <- toBigEndian32 . M.chunkToTokens proxy
          <$> M.takeP Nothing 4
        let x :: Word64
            x = (fromIntegral t0 `shiftL` 32) .|. fromIntegral t1
        return $ S5rd'Number $ fromIntegral x
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xB3
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xB3
      else do
        t1 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
        let x :: Word64
            x = (fromIntegral t0 `shiftL` 32) .|. fromIntegral t1
        v <- m
        return $ S5rd'KeyValue (S5rdKey'Number $ fromIntegral x) v
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    let l0 = fromIntegral $ h0 - 0xC1 + 1
        returnB ws = do
          let bs = B.pack $ sequenceFromHead24Tail32 l0 t0 ws
              condition = s5rd'isValidIdentifierB bs
          if not condition
            then M.customFailure ParseS5rdParsecError'InvalidIdentifier
            else return $ S5rd'Identifier $ SBS.toShort bs
    if not $ 0xC1 <= h0 && l0 <= 15
      then M.failure Nothing $ Set.fromList $
        flip map [0xC1..0xCF] $ \c -> M.Tokens $ NE.singleton c
      else if l0 <= 3 then returnB [] else do
        w1 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
        if l0 <= 7 then returnB [w1] else do
          w2 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
          if l0 <= 11 then returnB [w1, w2] else do
            w3 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
            returnB [w1, w2, w3]
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    let l0 = fromIntegral $ h0 - 0xD0 + 1
        returnB :: [Word32] -> m S5rd
        returnB ws = return $ S5rd'Binary
          $ B.pack $ sequenceFromHead24Tail32 l0 t0 ws
    if not $ 0xD0 <= h0 && l0 <= 11
      then M.failure Nothing $ Set.fromList $
        flip map [0xD0..0xDA] $ \c -> M.Tokens $ NE.singleton c
      else if l0 <= 3 then returnB [] else do
        w1 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
        if l0 <= 7 then returnB [w1] else do
          w2 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
          returnB [w1, w2]
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    let l0 = fromIntegral $ h0 - 0xE1 + 1
        returnB :: [Word32] -> m S5rd
        returnB ws = do
          let bs = B.pack $ sequenceFromHead24Tail32 l0 t0 ws
              condition = s5rd'isValidIdentifierB bs
          if not condition
            then M.customFailure ParseS5rdParsecError'InvalidIdentifier
            else do
              v <- m
              return $ S5rd'KeyValue (S5rdKey'Identifier $ SBS.toShort bs) v
    if not $ 0xE1 <= h0 && l0 <= 15
      then M.failure Nothing $ Set.fromList $
        flip map [0xE1..0xEF] $ \c -> M.Tokens $ NE.singleton c
      else if l0 <= 3 then returnB [] else do
        w1 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
        if l0 <= 7 then returnB [w1] else do
          w2 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
          if l0 <= 11 then returnB [w1, w2] else do
            w3 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
            returnB [w1, w2, w3]
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    let l0 = fromIntegral $ h0 - 0xF0 + 1
        returnB ws = do
          let bs = B.pack $ sequenceFromHead24Tail32 l0 t0 ws
          v <- m
          return $ S5rd'KeyValue (S5rdKey'Binary bs) v
    if not $ 0xF0 <= h0 && l0 <= 11
      then M.failure Nothing $ Set.fromList $
        flip map [0xF0..0xFA] $ \c -> M.Tokens $ NE.singleton c
      else if l0 <= 3 then returnB [] else do
        w1 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
        if l0 <= 7 then returnB [w1] else do
          w2 <- toBigEndian32 . M.chunkToTokens proxy <$> M.takeP Nothing 4
          returnB [w1, w2]
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xA1
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xA1
      else do
        (ts, t1) <- binaryEscapeSequence proxy 0xA1 0xDC
        let bs0 = B.pack $ M.chunkToTokens proxy ts
            l0 = B.length bs0
            bs = B.take t0 bs0
            condition = t0 <= l0 && l0 <= t0 + 4
            conditionId = s5rd'isValidIdentifierB bs
        if t0 /= t1
          then M.customFailure ParseS5rdParsecError'MismatchParity
          else if not condition
            then M.customFailure ParseS5rdParsecError'InvalidParity
            else if not conditionId
              then M.customFailure ParseS5rdParsecError'InvalidIdentifier
              else do
                v <- m
                return $ S5rd'KeyValue (S5rdKey'Identifier $ SBS.toShort bs) v
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xA2
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xA2
      else do
        (ts, t1) <- binaryEscapeSequence proxy 0xA2 0xDC
        let bs0 = B.pack $ M.chunkToTokens proxy ts
            l0 = B.length bs0
            bs = B.take t0 bs0
            condition = t0 <= l0 && l0 <= t0 + 4
            conditionId = s5rd'isValidIdentifierB bs
        if t0 /= t1
          then M.customFailure ParseS5rdParsecError'MismatchParity
          else if not condition
            then M.customFailure ParseS5rdParsecError'InvalidParity
            else if not conditionId
              then M.customFailure ParseS5rdParsecError'InvalidIdentifier
              else return $ S5rd'Identifier $ SBS.toShort bs
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xA4
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xA4
      else do
        (ts, t1) <- binaryEscapeSequence proxy 0xA4 0xDC
        let bs0 = B.pack $ M.chunkToTokens proxy ts
            l0 = B.length bs0
            bs = B.take t0 bs0
            condition = t0 <= l0 && l0 <= t0 + 4
        if t0 /= t1
          then M.customFailure ParseS5rdParsecError'MismatchParity
          else if not condition
            then M.customFailure ParseS5rdParsecError'InvalidParity
            else case TE.decodeUtf8' bs of
              Left _e -> M.customFailure ParseS5rdParsecError'InvalidUtf8Text
              Right t -> return $ S5rd'Text t
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xA6
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xA6
      else do
        (ts, t1) <- binaryEscapeSequence proxy 0xA6 0xDC
        let bs0 = B.pack $ M.chunkToTokens proxy ts
            l0 = B.length bs0
            bs = B.take t0 bs0
            condition = t0 <= l0 && l0 <= t0 + 4
        if t0 /= t1
          then M.customFailure ParseS5rdParsecError'MismatchParity
          else if not condition
            then M.customFailure ParseS5rdParsecError'InvalidParity
            else do
              v <- m
              return $ S5rd'KeyValue (S5rdKey'Binary bs) v
  , do
    (h0, t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xA7
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xA7
      else do
        (ts, t1) <- binaryEscapeSequence proxy 0xA7 0xDC
        let bs0 = B.pack $ M.chunkToTokens proxy ts
            l0 = B.length bs0
            bs = B.take t0 bs0
            condition = t0 <= l0 && l0 <= t0 + 4
        if t0 /= t1
          then M.customFailure ParseS5rdParsecError'MismatchParity
          else if not condition
            then M.customFailure ParseS5rdParsecError'InvalidParity
            else return $ S5rd'Binary bs
  , do
    (h0, _t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xAC
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xAC
      else return $ S5rd'Text T.empty
  , do
    (h0, _t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xAF
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xAE
      else return $ S5rd'Binary B.empty
  , do
    (h0, _t0) <- headtail32FromTokens proxy <$> M.takeP Nothing 4
    if h0 /= 0xAE
      then M.failure Nothing $ Set.singleton $ M.Tokens $ NE.singleton 0xAF
      else do
        v <- m
        return $ S5rd'KeyValue (S5rdKey'Binary B.empty) v
  ]

parsecBinaryS5rd :: forall e s m.
  (M.MonadParsec e s m, M.Stream s, M.Token s ~ Word8, e ~ ParseS5rdParsecError)
  => Proxy s -> m [S5rd]
parsecBinaryS5rd proxy =
  repeatParsec $ limitedRecursion 24 $ parsecBinaryS5rd' proxy

parseBinaryS5rd :: (M.Stream s, M.Token s ~ Word8)
                => String -> s -> ParseS5rdResultInternal s
parseBinaryS5rd filename bs =
  case M.parse (parsecBinaryS5rd (Proxy :: Proxy s)) filename bs of
    Left bundle -> ParseS5rdResultInternal'Error bundle
    Right [] -> ParseS5rdResultInternal'Empty
    Right [s] -> ParseS5rdResultInternal'One s
    Right ss -> ParseS5rdResultInternal'Multiple ss
