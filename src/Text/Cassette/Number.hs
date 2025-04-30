-- | Combinators to handle number literals.

module Text.Cassette.Number where

import Control.Category ((.))
import Control.Lens qualified as Lens
import Prelude hiding ((.))
import Text.Cassette.Char
import Text.Cassette.Combinator
import Text.Cassette.Lead
import Text.Cassette.Prim

-- | An integer literal, positive or negative.
int :: PP Int
int =
    intL --> many1 digit <>
    intL --> consL --> satisfy (== '-') . many1 digit
  where
    intL = isoL (Lens.iso show read)
