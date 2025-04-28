{-# LANGUAGE RankNTypes #-}

module Text.Cassette.Prim
  ( -- * Data types
    K7(..)
  , PP
  , PP0
    -- * Composition
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
  , set
  , unset
  , string
  , satisfy
  , lookAhead
  , eof
  ) where

import Control.Category (Category(..))
import Data.List (stripPrefix)
import Prelude hiding (flip, id, (.))
import Prelude qualified
import Text.Cassette.Internal.Tr (Tr(..))
import Text.Cassette.Internal.Tr qualified as Tr

-- | A cassette consists of two tracks, represented by profunctors. The
-- functions on each track are inverses of each other.
data K7 p a b = K7 { sideA :: p a b, sideB :: p b a }

instance Category p => Category (K7 p) where
  id = K7 id id
  -- Irrefutable patterns to support definitions of combinators by coinduction.
  ~(K7 f f') . ~(K7 g g') = K7 (f . g) (g' . f')

infixr 5 -->

-- | A synonym to '(.)' with its arguments flipped and with lower precedence,
-- but higher precedence than '(<|>)'.
(-->) :: Category p => K7 p a b -> K7 p b c -> K7 p a c
(-->) = Prelude.flip (.)

-- | Select the A-side.
play :: K7 p a b -> p a b
play csst = sideA csst

-- | Switch the A-side and B-side around.
flip :: K7 p a b -> K7 p b a
flip (K7 f g) = K7 g f

-- | The type of cassettes with a string transformer on each side. The A-side
-- produces a value in addition to transforming the string, /i.e./ it is
-- a parser. The B-side consumes a value to transform the string, /i.e./ it is
-- a printer.
type PP a = forall r. K7 Tr (a -> r) r

-- | The type of cassettes only useful for their effect on the input or output
-- strings, but do not produce\/consume any value.
type PP0 = forall r. K7 Tr r r

-- | Extract the parser from a cassette.
parse :: PP a -> String -> Maybe a
parse csst = unTr (play csst) (\_ _ x -> Just x) (const Nothing)

-- | Flip the cassette around to extract the pretty printer.
pretty :: PP a -> a -> Maybe String
pretty csst = unTr (play (flip csst)) (const Just) (\_ _ -> Nothing) ""

-- Use same priority and associativity as in base.
infixl 3 <|>

-- | Choice operator. If the first cassette fails, then try the second parser.
-- Note that this is an unrestricted backtracking operator: it never commits to
-- any particular choice.
--
-- >>> import Data.Char
-- >>> let alphaNum = satisfy isDigit <|> satisfy isAlpha
-- >>> parse alphaNum "ABCD"
-- Just 'A'
--
-- >>> parse (satisfy isAlpha <|> set 'B' empty) "ABCD"
-- Just 'A'
--
-- >>> parse (set 'B' empty <|> satisfy isAlpha) "ABCD"
-- Just 'A'
(<|>) :: K7 Tr r r' -> K7 Tr r r' -> K7 Tr r r'
K7 f f' <|> K7 g g' =
  K7 (Tr $ \k k' s -> unTr f k (\_ -> unTr g k k' s) s)
     (Tr $ \k k' s -> unTr f' k (\_ -> unTr g' k k' s) s)

-- | Always fail. This combinator does not produce\/consume any value, but has
-- a more general type than 'PP0' because it furthermore never succeeds.
--
-- >>> parse (set () empty) ""
-- Nothing
--
-- >>> pretty (set () empty) ()
-- Nothing
empty :: K7 Tr r r'
empty = K7 (Tr $ \_ k' s -> k' s) (Tr $ \_ k' s -> k' s)

-- | Do nothing.
--
-- >>> pretty (set () nothing) ()
-- Just ""
nothing :: PP0
nothing = K7 id id

-- | Turn the given pure transformer into a parsing\/printing pair. That is,
-- return a cassette that provides an output on the one side, and consumes an
-- input on the other, in addition to the string transformations of the given
-- pure transformer. @'set' x p@ provides @x@ as the output of @p@ on the
-- parsing side, and on the printing side accepts an input that is ignored.
set :: a -> PP0 -> PP a
set x ~(K7 f f') = K7 (f . Tr.push x) (Tr.pop . f')

-- | Turn the given parsing\/printing pair into a pure string transformer. That
-- is, return a cassette that does not produce an output or consume an input.
-- @'unset' x p@ throws away the output of @p@ on the parsing side, and on the
-- printing side sets the input to @x@.
unset :: a -> PP a -> PP0
unset x ~(K7 f f') = K7 (f . Tr.pop) (Tr.push x . f')

write :: (a -> String) -> Tr r (a -> r)
write f = Tr $ \k k' s x -> k (\s -> k' s x) (f x ++ s)

write0 :: String -> Tr r r
write0 x = Tr $ \k k' s -> unTr (write id) k (\s _ -> k' s) s x

-- | Strip\/add the given string from\/to the output string.
string :: String -> PP0
-- We could implement 'string' in terms of many, satisfy, char and unshift, but
-- don't, purely to reduce unnecessary choice points during parsing.
string x = K7 (Tr $ \k k' s -> maybe (k' s) (k k') $ stripPrefix x s) (write0 x)

-- | Successful only if predicate holds.
satisfy :: (Char -> Bool) -> PP Char
satisfy p = K7 (Tr f) (Tr g) where
  f k k' (x:xs)
    | p x = k (\s _ -> k' s) xs x
  f _ k' s = k' s
  g k k' s x
    | p x = k (\s -> k' s x) (x:s)
    | otherwise = k' s x

-- | Parse\/print without consuming\/producing any input.
--
-- >>> let spec = lookAhead (satisfy (=='A')) . string "A"
-- >>> parse spec "ABCD"
-- Just 'A'
lookAhead :: PP a -> PP a
lookAhead (K7 f f') =
  K7 (Tr $ \k k' s -> unTr f (\k' _ -> k k' s) k' s)
     (Tr $ \k k' s -> unTr f' (\k' _ -> k k' s) k' s)

-- | Succeeds if input string is empty.
--
-- >>> parse (set () eof) ""
-- Just ()
--
-- >>> parse (set () eof) "ABCD"
-- Nothing
eof :: PP0
eof = K7 (Tr isEmpty) (Tr isEmpty) where
  isEmpty k k' "" = k k' ""
  isEmpty _ k' s  = k' s
