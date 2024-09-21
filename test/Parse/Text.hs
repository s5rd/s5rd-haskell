{-# LANGUAGE OverloadedStrings #-}

module Parse.Text where

import Test.Tasty
import Test.Tasty.HUnit ((@=?), testCase)

import Data.S5rd.Parse
import Data.S5rd.Type

test_unit_escapeSequence :: [TestTree]
test_unit_escapeSequence =
  [ testCase "text" $ do
    Right (ParseS5rdResult'One (S5rd'Text "")) @=? parseS5rd "" "\"\""

    Right (ParseS5rdResult'One (S5rd'Text "\NUL")) @=? parseS5rd "" "\"\\NUL\""
    Right (ParseS5rdResult'One (S5rd'Text "\SOH")) @=? parseS5rd "" "\"\\SOH\""
    Right (ParseS5rdResult'One (S5rd'Text "\STX")) @=? parseS5rd "" "\"\\STX\""
    Right (ParseS5rdResult'One (S5rd'Text "\ETX")) @=? parseS5rd "" "\"\\ETX\""
    Right (ParseS5rdResult'One (S5rd'Text "\EOT")) @=? parseS5rd "" "\"\\EOT\""
    Right (ParseS5rdResult'One (S5rd'Text "\ENQ")) @=? parseS5rd "" "\"\\ENQ\""
    Right (ParseS5rdResult'One (S5rd'Text "\ACK")) @=? parseS5rd "" "\"\\ACK\""
    Right (ParseS5rdResult'One (S5rd'Text "\BEL")) @=? parseS5rd "" "\"\\BEL\""
    Right (ParseS5rdResult'One (S5rd'Text "\BS")) @=? parseS5rd "" "\"\\BS\""
    Right (ParseS5rdResult'One (S5rd'Text "\HT")) @=? parseS5rd "" "\"\\HT\""
    Right (ParseS5rdResult'One (S5rd'Text "\LF")) @=? parseS5rd "" "\"\\LF\""
    Right (ParseS5rdResult'One (S5rd'Text "\VT")) @=? parseS5rd "" "\"\\VT\""
    Right (ParseS5rdResult'One (S5rd'Text "\FF")) @=? parseS5rd "" "\"\\FF\""
    Right (ParseS5rdResult'One (S5rd'Text "\CR")) @=? parseS5rd "" "\"\\CR\""
    Right (ParseS5rdResult'One (S5rd'Text "\SO")) @=? parseS5rd "" "\"\\SO\""
    Right (ParseS5rdResult'One (S5rd'Text "\SI")) @=? parseS5rd "" "\"\\SI\""
    Right (ParseS5rdResult'One (S5rd'Text "\DLE")) @=? parseS5rd "" "\"\\DLE\""
    Right (ParseS5rdResult'One (S5rd'Text "\DC1")) @=? parseS5rd "" "\"\\DC1\""
    Right (ParseS5rdResult'One (S5rd'Text "\DC2")) @=? parseS5rd "" "\"\\DC2\""
    Right (ParseS5rdResult'One (S5rd'Text "\DC3")) @=? parseS5rd "" "\"\\DC3\""
    Right (ParseS5rdResult'One (S5rd'Text "\DC4")) @=? parseS5rd "" "\"\\DC4\""
    Right (ParseS5rdResult'One (S5rd'Text "\NAK")) @=? parseS5rd "" "\"\\NAK\""
    Right (ParseS5rdResult'One (S5rd'Text "\SYN")) @=? parseS5rd "" "\"\\SYN\""
    Right (ParseS5rdResult'One (S5rd'Text "\ETB")) @=? parseS5rd "" "\"\\ETB\""
    Right (ParseS5rdResult'One (S5rd'Text "\CAN")) @=? parseS5rd "" "\"\\CAN\""
    Right (ParseS5rdResult'One (S5rd'Text "\EM")) @=? parseS5rd "" "\"\\EM\""
    Right (ParseS5rdResult'One (S5rd'Text "\SUB")) @=? parseS5rd "" "\"\\SUB\""
    Right (ParseS5rdResult'One (S5rd'Text "\ESC")) @=? parseS5rd "" "\"\\ESC\""
    Right (ParseS5rdResult'One (S5rd'Text "\FS")) @=? parseS5rd "" "\"\\FS\""
    Right (ParseS5rdResult'One (S5rd'Text "\GS")) @=? parseS5rd "" "\"\\GS\""
    Right (ParseS5rdResult'One (S5rd'Text "\RS")) @=? parseS5rd "" "\"\\RS\""
    Right (ParseS5rdResult'One (S5rd'Text "\US")) @=? parseS5rd "" "\"\\US\""
    Right (ParseS5rdResult'One (S5rd'Text " ")) @=? parseS5rd "" "\"\\SP\""
    Right (ParseS5rdResult'One (S5rd'Text "\DEL")) @=? parseS5rd "" "\"\\DEL\""

    Right (ParseS5rdResult'One (S5rd'Text "\a")) @=? parseS5rd "" "\"\\a\""
    Right (ParseS5rdResult'One (S5rd'Text "\b")) @=? parseS5rd "" "\"\\b\""
    Right (ParseS5rdResult'One (S5rd'Text "\ESC")) @=? parseS5rd "" "\"\\e\""
    Right (ParseS5rdResult'One (S5rd'Text "\f")) @=? parseS5rd "" "\"\\f\""
    Right (ParseS5rdResult'One (S5rd'Text "\n")) @=? parseS5rd "" "\"\\n\""
    Right (ParseS5rdResult'One (S5rd'Text "\r")) @=? parseS5rd "" "\"\\r\""
    Right (ParseS5rdResult'One (S5rd'Text "\t")) @=? parseS5rd "" "\"\\t\""
    Right (ParseS5rdResult'One (S5rd'Text "\v")) @=? parseS5rd "" "\"\\v\""

    Right (ParseS5rdResult'One (S5rd'Text "\\")) @=? parseS5rd "" "\"\\\\\""
    Right (ParseS5rdResult'One (S5rd'Text "\"")) @=? parseS5rd "" "\"\\\"\""
    Right (ParseS5rdResult'One (S5rd'Text "\'")) @=? parseS5rd "" "\"\\\'\""

    Right (ParseS5rdResult'One (S5rd'Text "\^@")) @=? parseS5rd "" "\"\\^@\""
    Right (ParseS5rdResult'One (S5rd'Text "\^A")) @=? parseS5rd "" "\"\\^A\""
    Right (ParseS5rdResult'One (S5rd'Text "\^B")) @=? parseS5rd "" "\"\\^B\""
    Right (ParseS5rdResult'One (S5rd'Text "\^C")) @=? parseS5rd "" "\"\\^C\""
    Right (ParseS5rdResult'One (S5rd'Text "\^D")) @=? parseS5rd "" "\"\\^D\""
    Right (ParseS5rdResult'One (S5rd'Text "\^E")) @=? parseS5rd "" "\"\\^E\""
    Right (ParseS5rdResult'One (S5rd'Text "\^F")) @=? parseS5rd "" "\"\\^F\""
    Right (ParseS5rdResult'One (S5rd'Text "\^G")) @=? parseS5rd "" "\"\\^G\""
    Right (ParseS5rdResult'One (S5rd'Text "\^H")) @=? parseS5rd "" "\"\\^H\""
    Right (ParseS5rdResult'One (S5rd'Text "\^I")) @=? parseS5rd "" "\"\\^I\""
    Right (ParseS5rdResult'One (S5rd'Text "\^J")) @=? parseS5rd "" "\"\\^J\""
    Right (ParseS5rdResult'One (S5rd'Text "\^K")) @=? parseS5rd "" "\"\\^K\""
    Right (ParseS5rdResult'One (S5rd'Text "\^L")) @=? parseS5rd "" "\"\\^L\""
    Right (ParseS5rdResult'One (S5rd'Text "\^M")) @=? parseS5rd "" "\"\\^M\""
    Right (ParseS5rdResult'One (S5rd'Text "\^N")) @=? parseS5rd "" "\"\\^N\""
    Right (ParseS5rdResult'One (S5rd'Text "\^O")) @=? parseS5rd "" "\"\\^O\""
    Right (ParseS5rdResult'One (S5rd'Text "\^P")) @=? parseS5rd "" "\"\\^P\""
    Right (ParseS5rdResult'One (S5rd'Text "\^Q")) @=? parseS5rd "" "\"\\^Q\""
    Right (ParseS5rdResult'One (S5rd'Text "\^R")) @=? parseS5rd "" "\"\\^R\""
    Right (ParseS5rdResult'One (S5rd'Text "\^S")) @=? parseS5rd "" "\"\\^S\""
    Right (ParseS5rdResult'One (S5rd'Text "\^T")) @=? parseS5rd "" "\"\\^T\""
    Right (ParseS5rdResult'One (S5rd'Text "\^U")) @=? parseS5rd "" "\"\\^U\""
    Right (ParseS5rdResult'One (S5rd'Text "\^V")) @=? parseS5rd "" "\"\\^V\""
    Right (ParseS5rdResult'One (S5rd'Text "\^W")) @=? parseS5rd "" "\"\\^W\""
    Right (ParseS5rdResult'One (S5rd'Text "\^X")) @=? parseS5rd "" "\"\\^X\""
    Right (ParseS5rdResult'One (S5rd'Text "\^Y")) @=? parseS5rd "" "\"\\^Y\""
    Right (ParseS5rdResult'One (S5rd'Text "\^Z")) @=? parseS5rd "" "\"\\^Z\""
    Right (ParseS5rdResult'One (S5rd'Text "\^[")) @=? parseS5rd "" "\"\\^[\""
    Right (ParseS5rdResult'One (S5rd'Text "\^\")) @=? parseS5rd "" "\"\\^\\\""
    Right (ParseS5rdResult'One (S5rd'Text "\^]")) @=? parseS5rd "" "\"\\^]\""
    Right (ParseS5rdResult'One (S5rd'Text "\^^")) @=? parseS5rd "" "\"\\^^\""
    Right (ParseS5rdResult'One (S5rd'Text "\^_")) @=? parseS5rd "" "\"\\^_\""
    Right (ParseS5rdResult'One (S5rd'Text "\DEL")) @=? parseS5rd "" "\"\\^?\""

    Right (ParseS5rdResult'One (S5rd'Text "\&")) @=? parseS5rd "" "\"\\&\""
    Right (ParseS5rdResult'One (S5rd'Text "\SO\&H")) @=? parseS5rd "" "\"\\SO\\&H\""

    Right (ParseS5rdResult'One (S5rd'Text "")) @=? parseS5rd "" "\"\\ \""
    Right (ParseS5rdResult'One (S5rd'Text "")) @=? parseS5rd "" "\"\\\n\""
    Right (ParseS5rdResult'One (S5rd'Text "")) @=? parseS5rd "" "\"\\\t\""
    Right (ParseS5rdResult'One (S5rd'Text "\& ")) @=? parseS5rd "" "\"\\  \\& \""

  , testCase "binary" $ do
    Right (ParseS5rdResult'One (S5rd'Binary "")) @=? parseS5rd "" "\'\'"

    Right (ParseS5rdResult'One (S5rd'Binary "\NUL")) @=? parseS5rd "" "\'\\NUL\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\SOH")) @=? parseS5rd "" "\'\\SOH\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\STX")) @=? parseS5rd "" "\'\\STX\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\ETX")) @=? parseS5rd "" "\'\\ETX\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\EOT")) @=? parseS5rd "" "\'\\EOT\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\ENQ")) @=? parseS5rd "" "\'\\ENQ\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\ACK")) @=? parseS5rd "" "\'\\ACK\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\BEL")) @=? parseS5rd "" "\'\\BEL\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\BS")) @=? parseS5rd "" "\'\\BS\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\HT")) @=? parseS5rd "" "\'\\HT\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\LF")) @=? parseS5rd "" "\'\\LF\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\VT")) @=? parseS5rd "" "\'\\VT\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\FF")) @=? parseS5rd "" "\'\\FF\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\CR")) @=? parseS5rd "" "\'\\CR\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\SO")) @=? parseS5rd "" "\'\\SO\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\SI")) @=? parseS5rd "" "\'\\SI\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\DLE")) @=? parseS5rd "" "\'\\DLE\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\DC1")) @=? parseS5rd "" "\'\\DC1\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\DC2")) @=? parseS5rd "" "\'\\DC2\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\DC3")) @=? parseS5rd "" "\'\\DC3\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\DC4")) @=? parseS5rd "" "\'\\DC4\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\NAK")) @=? parseS5rd "" "\'\\NAK\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\SYN")) @=? parseS5rd "" "\'\\SYN\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\ETB")) @=? parseS5rd "" "\'\\ETB\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\CAN")) @=? parseS5rd "" "\'\\CAN\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\EM")) @=? parseS5rd "" "\'\\EM\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\SUB")) @=? parseS5rd "" "\'\\SUB\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\ESC")) @=? parseS5rd "" "\'\\ESC\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\FS")) @=? parseS5rd "" "\'\\FS\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\GS")) @=? parseS5rd "" "\'\\GS\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\RS")) @=? parseS5rd "" "\'\\RS\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\US")) @=? parseS5rd "" "\'\\US\'"
    Right (ParseS5rdResult'One (S5rd'Binary " ")) @=? parseS5rd "" "\'\\SP\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\DEL")) @=? parseS5rd "" "\'\\DEL\'"

    Right (ParseS5rdResult'One (S5rd'Binary "\a")) @=? parseS5rd "" "\'\\a\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\b")) @=? parseS5rd "" "\'\\b\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\ESC")) @=? parseS5rd "" "\'\\e\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\f")) @=? parseS5rd "" "\'\\f\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\n")) @=? parseS5rd "" "\'\\n\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\r")) @=? parseS5rd "" "\'\\r\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\t")) @=? parseS5rd "" "\'\\t\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\v")) @=? parseS5rd "" "\'\\v\'"

    Right (ParseS5rdResult'One (S5rd'Binary "\\")) @=? parseS5rd "" "\'\\\\\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\"")) @=? parseS5rd "" "\'\\\"\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\'")) @=? parseS5rd "" "\'\\\'\'"

    Right (ParseS5rdResult'One (S5rd'Binary "\^@")) @=? parseS5rd "" "\'\\^@\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^A")) @=? parseS5rd "" "\'\\^A\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^B")) @=? parseS5rd "" "\'\\^B\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^C")) @=? parseS5rd "" "\'\\^C\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^D")) @=? parseS5rd "" "\'\\^D\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^E")) @=? parseS5rd "" "\'\\^E\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^F")) @=? parseS5rd "" "\'\\^F\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^G")) @=? parseS5rd "" "\'\\^G\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^H")) @=? parseS5rd "" "\'\\^H\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^I")) @=? parseS5rd "" "\'\\^I\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^J")) @=? parseS5rd "" "\'\\^J\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^K")) @=? parseS5rd "" "\'\\^K\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^L")) @=? parseS5rd "" "\'\\^L\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^M")) @=? parseS5rd "" "\'\\^M\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^N")) @=? parseS5rd "" "\'\\^N\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^O")) @=? parseS5rd "" "\'\\^O\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^P")) @=? parseS5rd "" "\'\\^P\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^Q")) @=? parseS5rd "" "\'\\^Q\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^R")) @=? parseS5rd "" "\'\\^R\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^S")) @=? parseS5rd "" "\'\\^S\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^T")) @=? parseS5rd "" "\'\\^T\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^U")) @=? parseS5rd "" "\'\\^U\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^V")) @=? parseS5rd "" "\'\\^V\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^W")) @=? parseS5rd "" "\'\\^W\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^X")) @=? parseS5rd "" "\'\\^X\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^Y")) @=? parseS5rd "" "\'\\^Y\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^Z")) @=? parseS5rd "" "\'\\^Z\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^[")) @=? parseS5rd "" "\'\\^[\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^\")) @=? parseS5rd "" "\'\\^\\\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^]")) @=? parseS5rd "" "\'\\^]\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^^")) @=? parseS5rd "" "\'\\^^\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\^_")) @=? parseS5rd "" "\'\\^_\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\DEL")) @=? parseS5rd "" "\'\\^?\'"

    Right (ParseS5rdResult'One (S5rd'Binary "\&")) @=? parseS5rd "" "\'\\&\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\SO\&H")) @=? parseS5rd "" "\'\\SO\\&H\'"

    Right (ParseS5rdResult'One (S5rd'Binary "")) @=? parseS5rd "" "\'\\ \'"
    Right (ParseS5rdResult'One (S5rd'Binary "")) @=? parseS5rd "" "\'\\\n\'"
    Right (ParseS5rdResult'One (S5rd'Binary "")) @=? parseS5rd "" "\'\\\t\'"
    Right (ParseS5rdResult'One (S5rd'Binary "\& ")) @=? parseS5rd "" "\'\\  \\& \'"
  ]
