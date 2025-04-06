module Text.Cassette.Char where

import Data.Char
import Prelude hiding ((<>))
import Text.Cassette.Combinator
import Text.Cassette.Prim

-- | Succeeds if the current character is in the supplied list of characters.
-- See also 'satisfy'.
--
-- > vowel = oneOf "aeiou"
oneOf :: [Char] -> PP Char
oneOf xs = satisfy (`elem` xs)

-- | Dual of 'oneOf'.
noneOf :: [Char] -> PP Char
noneOf xs = satisfy (not . (`elem` xs))

-- | The 'satisfy' combinator, unshifted.
skip :: (Char -> Bool) -> Char -> PP0
skip p x = unshift x $ satisfy p

-- The next three combinators take their specification from the
-- invertible-syntax package.

-- | 'skipSpace' marks a position where whitespace is allowed to
-- occur. It accepts arbitrary space while parsing, and produces
-- no space while printing.
skipSpace :: PP0
skipSpace = unshift "" $ many (satisfy isSpace)

-- | 'optSpace' marks a position where whitespace is desired to occur.
-- It accepts arbitrary space while parsing, and produces a
-- single space character while printing.
optSpace :: PP0
optSpace = unshift " " $ many (satisfy isSpace)

-- | 'sepSpace' marks a position where whitespace is required to
-- occur. It requires one or more space characters while parsing,
-- and produces a single space character while printing.
sepSpace :: PP0
sepSpace = string " " <> skipSpace

-- | Parses a space character (\' \').
space :: PP0
space = char ' '

-- | Parses a newline character (\'\\n\').
newline :: PP0
newline = char '\n'

-- | Parses a tab character (\'\\t\').
tab :: PP0
tab = char '\t'

upper, lower, alphaNum, letter, digit, hexDigit, octDigit, anyChar :: PP Char

upper = satisfy isUpper
lower = satisfy isLower
alphaNum = satisfy isAlphaNum
letter = satisfy isAlpha
digit = satisfy isDigit
hexDigit = satisfy isHexDigit
octDigit = satisfy isOctDigit

-- | Any character.
anyChar = satisfy (const True)

-- | A specific character.
char :: Char -> PP0
char x = string [x]
