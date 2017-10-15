-- | This module exports combinators for parsing number literals.

module Text.Cassette.Number where

import Text.Cassette.Prim
import Text.Cassette.Lead
import Text.Cassette.Combinator
import Text.Cassette.Char

-- | An integer literal, positive or negative.
int :: PP Int
int =
    intL --> many1 digit <|>
    intL --> consL --> satisfy (== '-') <> many1 digit
  where
    intL = liftL $ Sym $ K7 read show
