module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.S5rd as S5rd
import System.Exit

main :: IO ()
main = do
  bs <- BL.getContents
  case S5rd.parse "-" bs of
    Left e -> die $ show e
    Right (S5rd.ParseS5rdResult'Multiple _) ->
      die "error: detected multiple documents"
    Right (S5rd.ParseS5rdResult'One s) -> do
      case S5rd.prettyText s of
        Left e -> die $ show e
        Right ts -> TLIO.putStrLn ts
