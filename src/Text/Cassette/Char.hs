{-# LANGUAGE RankNTypes #-}

-- | Commonly used character-oriented combinators.

module Text.Cassette.Char where

import Control.Category ((.))
import Data.Char
import Prelude hiding ((.))
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

-- | The 'satisfy' combinator, but unset.
skip :: (Char -> Bool) -> Char -> PP0
skip p x = unset x $ satisfy p

-- The next three combinators take their specification from the
-- invertible-syntax package.

-- | 'skipSpace' marks a position where whitespace is allowed to occur. It
-- accepts arbitrary space while parsing, and produces no space while printing.
skipSpace :: PP0
skipSpace = unset "" $ many (satisfy isSpace)

-- | 'optSpace' marks a position where whitespace is desired to occur. It
-- accepts arbitrary space while parsing, and produces a single space character
-- while printing.
optSpace :: PP0
optSpace = unset " " $ many (satisfy isSpace)

-- | 'sepSpace' marks a position where whitespace is required to occur. It
-- requires one or more space characters while parsing, and produces a single
-- space character while printing.
sepSpace :: PP0
sepSpace = string " " . skipSpace

-- | A single space character (\' \').
space :: PP0
space = char ' '

-- | A single newline character (\'\\n\').
newline :: PP0
newline = char '\n'

-- | A single tab character (\'\\t\').
tab :: PP0
tab = char '\t'

upper, lower, alphaNum, letter, digit, hexDigit, octDigit, anyChar :: PP Char

-- | An upper-case Unicode character.
upper = satisfy isUpper

-- | A lower-case Unicode character.
lower = satisfy isLower

-- | An alphabetic or numeric Unicode character.
alphaNum = satisfy isAlphaNum

-- | An alphabetic Unicode character.
letter = satisfy isAlpha

-- | An ASCII digit.
digit = satisfy isDigit

-- | An ASCII hexadecimal digit.
hexDigit = satisfy isHexDigit

-- | An ASCII octal digit.
octDigit = satisfy isOctDigit

-- | Any character.
anyChar = satisfy (const True)

-- | A specific character.
char :: Char -> PP0
char x = string [x]
