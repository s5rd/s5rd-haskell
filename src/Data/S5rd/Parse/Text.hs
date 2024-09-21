{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.S5rd.Parse.Text
  ( module X
  , parseTextS5rd
  ) where

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Applicative (asum, empty, (<|>))
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import qualified Data.Char as Char
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing)
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word
import Generic.Data (Generic)
import Lens.Micro.Platform
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Char.Lexer as ML

import Data.S5rd.Internal
import Data.S5rd.Parse.Type as X
import Data.S5rd.Type
import Data.S5rd.Util

lineComment :: (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char)
            => Proxy s -> m ()
lineComment proxy = ML.skipLineComment $ M.tokensToChunk proxy "--"

blockComment :: (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char)
             => Proxy s -> m ()
blockComment proxy = ML.skipBlockCommentNested
  (M.tokensToChunk proxy "{-")
  (M.tokensToChunk proxy "-}")

whitespace :: (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char)
           => Proxy s -> m ()
whitespace proxy = ML.space sc (lineComment proxy) (blockComment proxy)
 where
  sc = void $ M.takeWhile1P (Just "whitespace")
    (`elem` [' ', '\n', '\t', ','])

str :: (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char)
    => Proxy s -> String -> m ()
str proxy = void . M.string . M.tokensToChunk proxy

sym :: (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char)
    => Proxy s -> String -> m ()
sym proxy = void . ML.symbol (whitespace proxy) . M.tokensToChunk proxy

data ParseS5rdText'HoldIndent
  = ParseS5rdText'HoldIndent'EndOfInput
  | ParseS5rdText'HoldIndent'IncorrectIndent
  deriving (Generic, Eq, Show)

repeatParsecHoldIndent
  :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char)
  => m () -> M.Pos -> m c -> m a
  -> m (Either ParseS5rdText'HoldIndent c, [a])
repeatParsecHoldIndent sc pos0 end m = sc >> go Nothing []
 where
  go mpos xs = do
    b <- M.atEnd
    if b then return (Left ParseS5rdText'HoldIndent'EndOfInput, reverse xs) else do
      pos <- ML.indentLevel
      if not $ f pos0 mpos pos
        then return (Left ParseS5rdText'HoldIndent'IncorrectIndent, reverse xs)
        else do
          ee <- M.lookAhead $ M.observing end
          case ee of
            Right c -> return (Right c, reverse xs)
            Left _ -> do
              x <- m
              sc
              go (case mpos of { Nothing -> Just pos; Just _ -> mpos }) (x : xs)
  f p0 mp p = case mp of
    Nothing -> p0 <= p
    Just p1 -> p0 <= p && p1 <= p

data ParseS5rdText'Sep
  = ParseS5rdText'Sep'EndOfInput
  | ParseS5rdText'Sep'IncorrectIndent
  deriving (Generic, Eq, Show)

repeatParsecSep
  :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char)
  => m () -> M.Pos -> m c -> m () -> m a
  -> m (Either ParseS5rdText'Sep c, [a])
repeatParsecSep sc pos0 end sep m = sc >> go []
 where
  go xs = do
    x <- m
    let xxs = x : xs
    sc
    b <- M.atEnd
    if b then return (Left ParseS5rdText'Sep'EndOfInput, reverse xxs) else do
      ee <- M.lookAhead $ M.observing end
      case ee of
        Right c -> return (Right c, reverse xxs)
        Left _ -> do
          pos <- ML.indentLevel
          if pos /= pos0
            then return (Left ParseS5rdText'Sep'IncorrectIndent, reverse xxs)
            else do
              sep
              sc
              go xxs

repeatParsecWithEndParsec :: (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char)
                          => m c -> m a -> m (Maybe c, [a])
repeatParsecWithEndParsec end m = go []
 where
  go xs = do
    b <- M.atEnd
    if b then return (Nothing, reverse xs) else do
      ee <- M.observing end
      case ee of
        Right c -> return (Just c, reverse xs)
        Left _ -> do
          x <- m
          go (x : xs)

repeatParsecWithEndChar :: (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char)
                        => Char -> m a -> m [a]
repeatParsecWithEndChar c m = do
  (mc, xs) <- repeatParsecWithEndParsec (M.char c) m
  if isNothing mc
    then M.failure
      (Just M.EndOfInput)
      (Set.singleton $ M.Tokens $ NE.singleton c)
    else return xs

avoidEnd :: M.MonadParsec e s m => m r -> m (Maybe r)
avoidEnd m = do
  b <- M.atEnd
  if b then return Nothing else Just <$> m

data ParseS5rdTextLook
  = ParseS5rdTextLook'EndOfInput
  | ParseS5rdTextLook'Colon
  | ParseS5rdTextLook'ColonNewline
  | ParseS5rdTextLook'Otherwise
  deriving (Generic, Eq, Show)

lookColon :: (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char)
          => Proxy s -> m ParseS5rdTextLook
lookColon proxy = do
  la1 <- avoidEnd (M.lookAhead $ M.observing $ M.anySingle)
  case la1 of
    Nothing -> return $ ParseS5rdTextLook'EndOfInput
    Just (Right ':') -> do
      la2 <- fmap (fmap (M.chunkToTokens proxy)) <$> avoidEnd
        (M.lookAhead $ M.observing $ M.takeP Nothing 2)
      return $ case la2 of
         Just (Right [':', '\n']) -> ParseS5rdTextLook'ColonNewline
         _ -> ParseS5rdTextLook'Colon
    Just _ -> return $ ParseS5rdTextLook'Otherwise

checkAheadColon :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char)
                => Proxy s -> M.Pos -> m S5rd -> m a -> (S5rd -> m a) -> m a
checkAheadColon proxy pos0 m ma f = do
  la <- lookColon proxy
  case la of
    ParseS5rdTextLook'Colon -> do
      void $ M.char ':'
      whitespace proxy
      v <- m
      whitespace proxy
      f v
    ParseS5rdTextLook'ColonNewline -> do
      void $ M.char ':'
      (_ee, vs) <- repeatParsecHoldIndent (whitespace proxy) pos0 empty m
      v <- case vs of
        [] -> M.failure
          (Just M.EndOfInput)
          (Set.singleton $ M.Label $ NE.fromList "any value")
        [kv@(S5rd'KeyValue _ _)] -> return $ S5rd'Array $ V.singleton kv
        [v] -> return v
        _ -> return $ S5rd'Array $ V.fromList vs
      f v
    _ -> ma

nonzeroHeadDecimal :: (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char, Num a)
                   => Proxy s -> m a
nonzeroHeadDecimal proxy = do
  i0 <- M.satisfy (`elem` ['1'..'9'])
  is <- M.takeWhileP Nothing (`elem` ['0'..'9'])
  let step acc c = acc * 10 + (fromIntegral $ Char.ord c - Char.ord '0')
  return $ foldl' step 0 $ i0 : M.chunkToTokens proxy is

textEscapeSequence :: forall e s m.
  (M.MonadParsec e s m, M.Stream s, M.Token s ~ Char, e ~ ParseS5rdParsecError)
  => Proxy s -> Char -> Char -> m B.ByteString
textEscapeSequence proxy cbegin cend =
  B.pack . concat <$> (M.char cbegin >> M.manyTill charLiteral (M.char cend))
 where
  charLiteral :: m [Word8]
  charLiteral = asum $ map M.try
    [ do
      void $ M.char '\\'
      asum $ map M.try
        [ do
          void $ M.takeWhile1P (Just "whitespace in escape sequence")
            (`elem` [' ', '\n', '\t'])
          return []
        , do
          -- TODO: restrict digit count
          x <- asum $ map M.try
            [ str proxy "0b" >> ML.binary
            , str proxy "0o" >> ML.octal
            , str proxy "0x" >> ML.hexadecimal
            -- , M.char 'u' >> hexadecimal 4
            -- , M.char 'U' >> hexadecimal 8
            , nonzeroHeadDecimal proxy
            ]
          let _y :: Integer
              _y = x
          if x <= 0xFF
            then return [fromIntegral x]
            else M.customFailure ParseS5rdParsecError'TooLargeNumericCharLiteral
        , do
          asum $ flip map (Map.toDescList subMap2)
            $ \(k, v) -> M.try $ v <$ str proxy k
        ]
    , UTF8.encodeChar <$> M.anySingle
    ]

data ParseS5rdTextTerminalSymbol
  = ParseS5rdTextTerminalSymbol'None
  | ParseS5rdTextTerminalSymbol'Brace
  | ParseS5rdTextTerminalSymbol'Bracket
  | ParseS5rdTextTerminalSymbol'Parenthesis
  | ParseS5rdTextTerminalSymbol'Asterisks
  deriving (Generic, Eq, Show)

data ParseS5rdTextState = ParseS5rdTextState
  { _parseS5rdTextState'ExpectTerminalSymbol :: ParseS5rdTextTerminalSymbol
  }

$(makeLenses ''ParseS5rdTextState)

parsecTextS5rd' :: forall e s m.
  ( M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char,
    e ~ ParseS5rdParsecError)
  => Proxy s -> (ParseS5rdTextState -> m S5rd) -> ParseS5rdTextState -> m S5rd
parsecTextS5rd' proxy m state = ML.lexeme (whitespace proxy) $ asum $ map M.try
  [ do
    sym proxy "["
    let newState = state
          & parseS5rdTextState'ExpectTerminalSymbol
            .~ ParseS5rdTextTerminalSymbol'Bracket
    xs <- V.fromList <$> repeatParsecWithEndChar ']' (m newState)
    return $ S5rd'Array xs
  , do
    sym proxy "{"
    let newState = state
          & parseS5rdTextState'ExpectTerminalSymbol
            .~ ParseS5rdTextTerminalSymbol'Brace
    xs <- V.fromList <$> repeatParsecWithEndChar '}' (m newState)
    let condition = V.all s5rd'isKeyValue xs
    if not condition
      then M.customFailure ParseS5rdParsecError'IllegalValue
      else return $ S5rd'Array xs
  , do
    sym proxy "("
    let newState = state
          & parseS5rdTextState'ExpectTerminalSymbol
            .~ ParseS5rdTextTerminalSymbol'Parenthesis
    v <- m newState
    sym proxy ")"
    return v
  , do
    pos <- ML.indentLevel
    sym proxy "- "
    let end' = case state ^. parseS5rdTextState'ExpectTerminalSymbol of
          ParseS5rdTextTerminalSymbol'None -> empty
          ParseS5rdTextTerminalSymbol'Brace -> sym proxy "}"
          ParseS5rdTextTerminalSymbol'Bracket -> sym proxy "]"
          ParseS5rdTextTerminalSymbol'Parenthesis -> sym proxy ")"
          ParseS5rdTextTerminalSymbol'Asterisks -> sym proxy "***"
        nextItem = do
          _ <- ML.indentGuard (whitespace proxy) EQ pos
          sym proxy "- "
        end = nextItem <|> end'
        newState = state
          & parseS5rdTextState'ExpectTerminalSymbol
            .~ ParseS5rdTextTerminalSymbol'None
        go = do
          (_ets, xs) <- repeatParsecHoldIndent (whitespace proxy) pos end (m newState)
          case xs of
            [] -> M.failure
              (Just M.EndOfInput)
              (Set.singleton $ M.Label $ NE.fromList "any value")
            [x] -> return x
            _ -> return $ S5rd'Array $ V.fromList xs
    (_et, xs) <- repeatParsecSep (whitespace proxy) pos end' (sym proxy "- ") go
    return $ S5rd'Array $ V.fromList xs
  , do
    pos <- ML.indentLevel
    bs <- textEscapeSequence proxy '\"' '\"'
    case TE.decodeUtf8' bs of
      Left _ -> M.customFailure ParseS5rdParsecError'InvalidUtf8Text
      Right ts -> checkAheadColon proxy pos (m state)
        (return $ S5rd'Text ts)
        (\v -> do
          if not $ s5rd'isValidIdentifierT ts
            then M.customFailure ParseS5rdParsecError'InvalidIdentifier
            else return $ S5rd'KeyValue (S5rdKey'Identifier $ SBS.toShort bs) v)
  , do
    pos <- ML.indentLevel
    bs <- textEscapeSequence proxy '\'' '\''
    checkAheadColon proxy pos (m state)
      (return $ S5rd'Binary bs)
      (\v -> return $ S5rd'KeyValue (S5rdKey'Binary bs) v)
  , do
    pos <- ML.indentLevel
    bs <- textEscapeSequence proxy '`' '`'
    case TE.decodeUtf8' bs of
      Left _ -> M.customFailure ParseS5rdParsecError'InvalidUtf8Text
      Right ts -> if not $ s5rd'isValidIdentifierT ts
        then M.customFailure ParseS5rdParsecError'InvalidIdentifier
        else checkAheadColon proxy pos (m state)
          (return $ S5rd'Identifier $ SBS.toShort bs)
          (\v -> return $ S5rd'KeyValue (S5rdKey'Identifier $ SBS.toShort bs) v)
  , do
    pos <- ML.indentLevel
    c <- M.satisfy isHeadIdentifierCharNonNum M.<?> "non number head of identifier"
    cs <- M.takeWhileP (Just "identifier character") isIdentifierChar
    let sbs = SBS.toShort $ TE.encodeUtf8 $ T.pack $ c : M.chunkToTokens proxy cs
    checkAheadColon proxy pos (m state)
      (return $ S5rd'Identifier sbs)
      (\v -> return $ S5rd'KeyValue (S5rdKey'Identifier sbs) v)
  , do
    pos <- ML.indentLevel
    x <- asum $ map M.try
      [ str proxy "0b" >> ML.binary
      , str proxy "0o" >> ML.octal
      , str proxy "0x" >> ML.hexadecimal
      , ML.decimal
      ]
    checkAheadColon proxy pos (m state)
      (return $ S5rd'Number x)
      (\v -> return $ S5rd'KeyValue (S5rdKey'Number x) v)
  ]

parsecTextS5rd
  :: (M.MonadParsec e s m, M.TraversableStream s, M.Token s ~ Char,
    e ~ ParseS5rdParsecError)
  => Proxy s -> m [S5rd]
parsecTextS5rd proxy = asum $ map M.try
  [ do
    pos <- ML.indentLevel
    sym proxy "***"
    whitespace proxy
    go pos
  , do
    whitespace proxy
    pos <- ML.indentLevel
    go pos
  ]
 where
  parsec = limitedRecursion1 24 (parsecTextS5rd' proxy)
    $ ParseS5rdTextState ParseS5rdTextTerminalSymbol'Asterisks
  end pos = do
    _ <- ML.indentGuard (whitespace proxy) EQ pos
    sym proxy "***"
  go pos = do
    (_e, vs) <- repeatParsecSep
      (whitespace proxy) pos empty (sym proxy "***") (h pos)
    return $ catMaybes vs
  h pos = do
    (_ee, vs) <- repeatParsecHoldIndent (whitespace proxy) pos (end pos) parsec
    return $ case vs of
      [] -> Nothing
      [kv@(S5rd'KeyValue _ _)] -> Just $ S5rd'Array $ V.singleton kv
      [y] -> Just $ y
      _ -> Just $ S5rd'Array $ V.fromList vs

parseTextS5rd :: forall s. (M.TraversableStream s, M.Token s ~ Char)
                => String -> s -> ParseS5rdResultInternal s
parseTextS5rd filename ts =
  case M.parse (parsecTextS5rd (Proxy :: Proxy s)) filename ts of
    Left bundle -> ParseS5rdResultInternal'Error bundle
    Right [] -> ParseS5rdResultInternal'Empty
    Right [s] -> ParseS5rdResultInternal'One s
    Right ss -> ParseS5rdResultInternal'Multiple ss
