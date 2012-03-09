module Text.Cassette.Char where

import Text.Cassette.Prim
import Data.Char


oneOf :: [Char] -> PP Char
oneOf xs = satisfy (`elem` xs)

noneOf :: [Char] -> PP Char
noneOf xs = satisfy (not . (`elem` xs))

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
sepSpace = lit " " <> skipSpace

newline, tab :: PP0

newline = char '\n'
tab = char '\t'

upper, lower, alphaNum, letter, digit, hexDigit, octDigit, anyChar :: PP Char

upper = satisfy isUpper
lower = satisfy isLower
alphaNum = satisfy isAlphaNum
letter = satisfy isAlpha
digit = satisfy isDigit
hexDigit = satisfy isHexDigit
octDigit = satisfy isOctDigit
anyChar = satisfy (const True)

char :: Char -> PP0
char x = lit [x]
