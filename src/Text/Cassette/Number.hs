-- | This module exports combinators for parsing number literals.

module Text.Cassette.Number where

import Control.Category ((.))
import Prelude hiding ((.))
import Text.Cassette.Char
import Text.Cassette.Combinator
import Text.Cassette.Lead
import Text.Cassette.Prim

-- | An integer literal, positive or negative.
int :: PP Int
int =
    intL --> many1 digit <|>
    intL --> consL --> satisfy (== '-') . many1 digit
  where
    intL = liftL read show
