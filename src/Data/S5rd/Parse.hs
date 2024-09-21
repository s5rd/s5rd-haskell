module Data.S5rd.Parse
  ( module X
  , parseS5rd
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V

import Data.S5rd.Parse.Binary (parseBinaryS5rd)
import Data.S5rd.Parse.Text (parseTextS5rd)
import Data.S5rd.Parse.Type as X

parseS5rd :: String -> BL.ByteString -> Either ParseS5rdError ParseS5rdResult
parseS5rd filename bs = case BL.uncons bs of
  Nothing -> Left ParseS5rdError'Empty
  Just (h, _) -> if h >= 0x80
    then case parseBinaryS5rd filename bs of
      ParseS5rdResultInternal'Empty -> Left ParseS5rdError'Empty
      (ParseS5rdResultInternal'One v) -> Right $ ParseS5rdResult'One v
      (ParseS5rdResultInternal'Multiple vs) ->
        Right $ ParseS5rdResult'Multiple $ V.fromList vs
      (ParseS5rdResultInternal'Error _e) -> Left ParseS5rdError'ParsecError
    else case TLE.decodeUtf8' bs of
      Left _ -> Left ParseS5rdError'InvalidUtf8Text
      Right ts -> case parseTextS5rd filename ts of
        ParseS5rdResultInternal'Empty -> Left ParseS5rdError'Empty
        (ParseS5rdResultInternal'One v) -> Right $ ParseS5rdResult'One v
        (ParseS5rdResultInternal'Multiple vs) ->
          Right $ ParseS5rdResult'Multiple $ V.fromList vs
        (ParseS5rdResultInternal'Error _e) -> Left ParseS5rdError'ParsecError
