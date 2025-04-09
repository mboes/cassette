{-# LANGUAGE RankNTypes #-}

module Text.Cassette.Prim
  ( -- * Data types
    K7(..)
  , C
  , PP
  , PP0
    -- * Composition
  , (<>)
  , (-->)
  , (<|>)
    -- * Extraction
  , play
  , flip
  , parse
  , pretty
    -- * Primitive combinators
  , empty
  , nothing
  , shift
  , unshift
  , string
  , satisfy
  , lookAhead
  , eof
  ) where

import Control.Category (Category(..))
import Data.List (stripPrefix)
import qualified Prelude
import Prelude hiding (flip, id, (.), (<>))

-- | A cassette consists of two tracks, represented by functions. The functions
-- on each track are inverses of each other.
data K7 a b = K7 { sideA :: a -> b, sideB :: b -> a }

infixr 6 <>

-- | Tape splicing operator. Functions on each track are composed pairwise.
(<>) :: K7 b c -> K7 a b -> K7 a c
-- Irrefutable patterns to support definitions of combinators by coinduction.
~(K7 f f') <> ~(K7 g g') = K7 (f . g) (g' . f')

instance Category K7 where
  id = K7 id id
  (.) = (<>)

infixr 5 -->

-- | A synonym to '(<>)' with its arguments flipped and with lower precedence.
(-->) :: K7 a b -> K7 b c -> K7 a c
(-->) = Prelude.flip (<>)

-- | The type of string transformers in CPS, /i.e./ functions from strings to
-- strings.
type C r = (String -> r) -> String -> r

-- | The type of cassettes with a string transformer on each side. The A-side
-- produces a value in addition to transforming the string, /i.e./ it is a
-- parser. The B-side consumes a value to transform the string, /i.e./ it is a
-- printer.
type PP a = forall r. K7 (C (a -> r)) (C r)

-- | The type of cassettes only useful for their effect on the input or output
-- strings, but do not produce/consume any value.
type PP0  = forall r. K7 (C r) (C r)

-- | Select the A-side.
play :: K7 a b -> a -> b
play csst = sideA csst

-- | Switch the A-side and B-side around.
flip :: K7 a b -> K7 b a
flip (K7 f g) = K7 g f

-- | Extract the parser from a cassette.
parse :: PP a -> String -> Maybe a
parse csst = play csst (\_ _ x -> Just x) (const Nothing)

-- | Flip the cassette around to extract the pretty printer.
pretty :: PP a -> a -> Maybe String
pretty csst = play (flip csst) (const Just) (\_ _ -> Nothing) ""

-- Use same priority and associativity as in base.
infixl 3 <|>

-- | Choice operator. If the first cassette fails, then try the second parser.
-- Note that this is an unrestricted backtracking operator: it never commits
-- to any particular choice.
(<|>) :: PP a -> PP a -> PP a
K7 f f' <|> K7 g g' =
  K7 (\k k' s -> f k (\_ -> g k k' s) s)
     (\k k' s x -> f' k (\_ -> g' k k' s) s x)

-- | Always fail. This combinator does not produce/consume any value,
-- but has a more general type than 'PP0' because it furthermore never
-- succeeds.
empty :: K7 (C r) (C r')
empty = K7 (\_ k' s -> k' s) (\_ k' s -> k' s)

-- | Do nothing.
nothing :: PP0
nothing = K7 id id

-- | Turn the given pure transformer into a parsing/printing pair. That is,
-- return a cassette that produces and output on the one side, and consumes an
-- input on the other, in addition to the string transformations of the given
-- pure transformer. @shift x p@ produces @x@ as the output of @p@ on the
-- parsing side, and on the printing side accepts an input that is ignored.
shift :: a -> PP0 -> PP a
shift x ~(K7 f f') =
  K7 (\k k' -> f (\k' s -> k (\s _ -> k' s) s x) k')
     (\k k' s x -> f' k (\s -> k' s x) s)

-- | Turn the given cassette into a pure string transformer. That is, return a
-- cassette that does not produce an output or consume an input. @unshift x p@
-- throws away the output of @p@ on the parsing side, and on the printing side
-- sets the input to @x@.
unshift :: a -> PP a -> PP0
unshift x ~(K7 f f') =
  K7 (\k k' -> f (\k' s x -> k (\s -> k' s x) s) k')
     (\k k' s -> f' k (\s _ -> k' s) s x)

write :: (a -> String) -> C r -> C (a -> r)
write f = \k k' s x -> k (\s -> k' s x) (f x ++ s)

write0 :: String -> C r -> C r
write0 x = \k k' s -> write id k (\s _ -> k' s) s x

-- | Strip/add the given string from/to the output string.
string :: String -> PP0
-- We could implement 'string' in terms of many, satisfy, char and unshift,
-- but don't, purely to reduce unnecessary choice points during parsing.
string x = K7 (\k k' s -> maybe (k' s) (k k') $ stripPrefix x s) (write0 x)

-- | Successful only if predicate holds.
satisfy :: (Char -> Bool) -> PP Char
satisfy p = K7 f g where
  f k k' (x:xs)
    | p x = k (\s _ -> k' s) xs x
  f _ k' s = k' s
  g k k' s x
    | p x = k (\s -> k' s x) (x:s)
    | otherwise = k' s x

-- | Parse/print without consuming/producing any input.
lookAhead :: PP a -> PP a
lookAhead (K7 f f') = K7 (\k k' s -> f (\k' _ -> k k' s) k' s) (\k k' s -> f' (\k' _ -> k k' s) k' s)

-- | Succeeds if input string is empty.
eof :: PP0
eof = K7 isEmpty isEmpty where
  isEmpty k k' "" = k k' ""
  isEmpty _ k' s  = k' s
