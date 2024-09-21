{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Data.S5rd.Internal where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Bifunctor (first, second)
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import Data.Tuple (swap)
import Data.Word
import qualified Numeric (showHex)
import qualified Text.Megaparsec as M

import Data.S5rd.Parse.Type

limitedRecursion :: (M.MonadParsec e s m, e ~ ParseS5rdParsecError)
                 => Int -> (m a -> m a) -> m a
limitedRecursion n f
  | n < 0 = M.customFailure ParseS5rdParsecError'ReachedRecursionLimit
  | otherwise = g n
 where
  g 0 = M.customFailure ParseS5rdParsecError'ReachedRecursionLimit
  g x = f (g (pred x))

limitedRecursion1 :: (M.MonadParsec e s m, e ~ ParseS5rdParsecError)
                 => Int -> ((b -> m a) -> b -> m a) -> b -> m a
limitedRecursion1 n f b
  | n < 0 = M.customFailure ParseS5rdParsecError'ReachedRecursionLimit
  | otherwise = g n b
 where
  g 0 _ = M.customFailure ParseS5rdParsecError'ReachedRecursionLimit
  g x b0 = f (g (pred x)) b0

repeatParsec :: M.MonadParsec e s m => m a -> m [a]
repeatParsec m = go []
 where
  go xs = do
    b <- M.atEnd
    if b then return $ reverse xs else do
      x <- m
      go (x : xs)

toBigEndianByteSequence :: Word32 -> [Word8]
toBigEndianByteSequence w = [a,b,c,d]
 where
  a = fromIntegral $ w `shiftR` 24
  b = fromIntegral $ w `shiftR` 16
  c = fromIntegral $ w `shiftR` 8
  d = fromIntegral w

hexDigitWithBase :: Num a => Int -> Char -> Maybe a
hexDigitWithBase n
  | 0 < n && n <= 10 = \c -> let w = Char.ord c in
    if ord_0 <= w && w - ord_0 < 10
      then Just $ fromIntegral $ w - ord_0
      else Nothing
  | 10 < n && n <= 36 = \c -> let w = Char.ord c in
    if ord_0 <= w && w - ord_0 < 10
      then Just $ fromIntegral $ w - ord_0
      else if ord_A <= w && w - ord_A < n - 10
        then Just $ fromIntegral $ w - ord_A + 10
        else if ord_a <= w && w - ord_a < n - 10
          then Just $ fromIntegral $ w - ord_a + 10
          else Nothing
  | otherwise = const Nothing
 where
  ord_0 = fromIntegral $ Char.ord '0'
  ord_A = fromIntegral $ Char.ord 'A'
  ord_a = fromIntegral $ Char.ord 'a'

isHexDigitWithBase :: Int -> Char -> Bool
isHexDigitWithBase n c = isJust $ (hexDigitWithBase n c :: Maybe Int)

fromHexDigitWithBase :: Num a => Int -> Char -> a
fromHexDigitWithBase n c = fromJust $ hexDigitWithBase n c

convertBinaryToEscapeSequence :: B.ByteString -> TB.Builder
convertBinaryToEscapeSequence bs0 = case B.uncons bs0 of
  Nothing -> mempty
  Just (w, bs1)
    | w >= 0x80 -> case B.uncons bs1 of
      Just (isHexDigitWithBase 16 . Char.chr . fromIntegral -> True, _bs2) ->
        TB.fromString ("\\0x" <> Numeric.showHex w "" <> "\\&")
        <> convertBinaryToEscapeSequence bs1
      _ -> TB.fromString ("\\0x" <> Numeric.showHex w "")
        <> convertBinaryToEscapeSequence bs1
    | otherwise -> case Map.lookup (fromIntegral w) subMap1 of
      Nothing -> TB.singleton (Char.chr $ fromIntegral w)
        <> convertBinaryToEscapeSequence bs1
      Just "SO" -> case B.uncons bs1 of
        Just (Char.chr . fromIntegral -> 'H', bs2) ->
          TB.fromString "\\SO\\&H"
          <> convertBinaryToEscapeSequence bs2
        _ -> TB.fromString "\\SO"
          <> convertBinaryToEscapeSequence bs1
      Just str -> TB.fromString ('\\' : str)
        <> convertBinaryToEscapeSequence bs1

convertTextToEscapeSequence :: T.Text -> TB.Builder
convertTextToEscapeSequence ts0 = case T.uncons ts0 of
  Nothing -> mempty
  Just (c, ts1) -> case Map.lookup (Char.ord c) subMap1 of
    Nothing -> TB.singleton c
      <> convertTextToEscapeSequence ts1
    Just "SO" -> case T.uncons ts1 of
      Just ('H', ts2) -> TB.fromString "\\SO\\&H"
        <> convertTextToEscapeSequence ts2
      _ -> TB.fromString "\\SO"
        <> convertTextToEscapeSequence ts1
    Just str -> TB.fromString ('\\' : str)
      <> convertTextToEscapeSequence ts1

subMap1 :: Map.Map Int String
subMap1 = Map.fromList $ flip map subMapList1 $ first Char.ord
subMap2 :: Map.Map String [Word8]
subMap2 = Map.fromList subMapList2

subMapList1 :: [(Char, String)]
subMapList1 =
  [ ('\\', "\\")
  , ('\"', "\"")
  , ('\'', "\'")
  , ('\NUL', "NUL")
  , ('\SOH', "SOH")
  , ('\STX', "STX")
  , ('\ETX', "ETX")
  , ('\EOT', "EOT")
  , ('\ENQ', "ENQ")
  , ('\ACK', "ACK")
  , ('\BEL', "BEL")
  , ('\BS', "BS")
  , ('\HT', "HT")
  , ('\LF', "LF")
  , ('\VT', "VT")
  , ('\FF', "FF")
  , ('\CR', "CR")
  , ('\SO', "SO")
  , ('\SI', "SI")
  , ('\DLE', "DLE")
  , ('\DC1', "DC1")
  , ('\DC2', "DC2")
  , ('\DC3', "DC3")
  , ('\DC4', "DC4")
  , ('\NAK', "NAK")
  , ('\SYN', "SYN")
  , ('\ETB', "ETB")
  , ('\CAN', "CAN")
  , ('\EM', "EM")
  , ('\SUB', "SUB")
  , ('\ESC', "ESC")
  , ('\FS', "FS")
  , ('\GS', "GS")
  , ('\RS', "RS")
  , ('\US', "US")
  , ('\DEL', "DEL")
  ]
subMapList2 :: [(String, [Word8])]
subMapList2 = (map (second UTF8.encodeChar) $ map swap subMapList1 ++
  [ ("SP", ' ')
  , ("a", '\a')
  , ("b", '\b')
  , ("e", '\ESC')
  , ("f", '\f')
  , ("n", '\n')
  , ("r", '\r')
  , ("t", '\t')
  , ("v", '\v')
  , ("^@", '\^@')
  , ("^A", '\^A')
  , ("^B", '\^B')
  , ("^C", '\^C')
  , ("^D", '\^D')
  , ("^E", '\^E')
  , ("^F", '\^F')
  , ("^G", '\^G')
  , ("^H", '\^H')
  , ("^I", '\^I')
  , ("^J", '\^J')
  , ("^K", '\^K')
  , ("^L", '\^L')
  , ("^M", '\^M')
  , ("^N", '\^N')
  , ("^O", '\^O')
  , ("^P", '\^P')
  , ("^Q", '\^Q')
  , ("^R", '\^R')
  , ("^S", '\^S')
  , ("^T", '\^T')
  , ("^U", '\^U')
  , ("^V", '\^V')
  , ("^W", '\^W')
  , ("^X", '\^X')
  , ("^Y", '\^Y')
  , ("^Z", '\^Z')
  , ("^[", '\^[')
  , ("^\\", '\^\')
  , ("^]", '\^]')
  , ("^^", '\^^')
  , ("^_", '\^_')
  , ("^?", '\DEL')
  ]) ++ [("&", [])]
