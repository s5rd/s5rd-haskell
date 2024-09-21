# s5rd

s5rd (/ˈstændərd/, numeronym of the word "standard")
is lightweight recursive binary/text data format.
s5rd-haskell is one reference implementation of s5rd.

## Usage

```haskell
import Data.S5rd

from :: FromS5rd a => S5rd -> Either (FromS5rdError a) a
fromKey :: FromS5rdKey a => S5rdKey -> Either (FromS5rdKeyError a) a
to :: ToS5rd a => a -> Either (ToS5rdError a) S5rd
toKey :: ToS5rdKey a => a -> Either (ToS5rdKeyError a) S5rdKey

parse :: String -> LazyByteString -> Either ParseS5rdError ParseS5rdResult
serializeBinary :: S5rd -> Either SerializeS5rdError'Binary LazyByteString
serializeText :: S5rd -> Either SerializeS5rdError'Text LazyText
prettyText :: S5rd -> Either PrettyS5rdError'Text LazyText

decode :: FromS5rd a
       => String -> BL.ByteString
       -> Either (DecodeS5rdError a) (DecodeS5rdResult a)
decode filename ~= from <=< parse filename

encodeBinary :: ToS5rd a => a -> Either (EncodeS5rdError'Binary a) LazyByteString
encodeBinary ~= serializeBinary <=< to

encodeText :: ToS5rd a => a -> Either (EncodeS5rdError'Text a) LazyText
encodeText ~= serializeText <=< to
```

Here, `~=` means "approximately equal".

The first argument of `parse` and `decode` is filename.
The filename only affects `ParseS5rdError`.

## Property

`parse` functions and `serialize`/`pretty` functions are the inverse of each other.
See <test/Prop.hs>.

## Treat as JSON or YAML

parseS5rdText accepts JSON-like and YAML-like format.
See <test/External.hs>.
