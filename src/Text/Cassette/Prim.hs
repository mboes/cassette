{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

-- | The primitive parser combinators.

module Text.Cassette.Prim
  ( -- * Data types
    K7(..)
  , Tr
  , PP
  , PP0
    -- * Composition
  , (-->)
    -- * Extraction
  , parse
  , pretty
  , sscanf
  , sprintf
    -- * Primitive combinators
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
import GHC.Stack (HasCallStack)
import Prelude hiding (flip, id, (.))
import Text.Cassette.Internal.Tr (Tr(..))
import Text.Cassette.Internal.Tr qualified as Tr

-- | A cassette consists of two tracks, represented by profunctors. The
-- functions on each track are inverses of each other.
data K7 p a b = K7
  { sideA :: p a b
  , sideB :: forall t. p (a -> t) (b -> t)
  }

instance (forall r r'. Semigroup (p r r')) => Semigroup (K7 p r r') where
  K7 f f' <> K7 g g' = K7 (f <> g) (f' <> g')

instance (forall r r'. Monoid (p r r')) => Monoid (K7 p r r') where
  mempty = K7 mempty mempty

instance Category p => Category (K7 p) where
  id = K7 id id
  -- Irrefutable patterns to support definitions of combinators by coinduction.
  ~(K7 f f') . ~(K7 g g') = K7 (f . g) (f' . g')

infixr 9 -->

-- | A synonym to '(.)'
(-->) :: Category p => K7 p b c -> K7 p a b -> K7 p a c
(-->) = (.)

-- | The type of cassettes with a string transformer on each side. The A-side
-- produces a value in addition to transforming the string, /i.e./ it is
-- a parser. The B-side consumes a value to transform the string, /i.e./ it is
-- a printer.
type PP a = forall r. K7 Tr r (a -> r)

-- | The type of cassettes only useful for their effect on the input or output
-- strings, but do not produce\/consume any value.
type PP0 = forall r. K7 Tr r r

-- | Extract the parser from a cassette.
parse :: PP a -> String -> Maybe a
parse (K7 _ f') s = unTr f' (\_ _ x -> Just x) (\_ _ -> Nothing) s id

-- | Flip the cassette around to extract the pretty printer.
pretty :: PP a -> a -> Maybe String
pretty (K7 f _) = unTr f (const Just) (\_ _ -> Nothing) ""

-- | An equivalent to @sscanf()@ in C: @'sscanf' fmt k s@ extracts data from
-- string @s@ according to format descriptor @fmt@ and hands the data to
-- continuation @k@.
--
-- >>> spec = satisfy (=='A') . satisfy (=='B') . satisfy (=='C')
-- >>> sscanf spec (,,) "ABC"
-- ('A','B','C')
sscanf :: HasCallStack => K7 Tr r r' -> r' -> String -> r
sscanf (K7 _ f') k s = unTr f' (\_ _ -> id) (\_ _ -> error msg) s k
  where
    msg = "sscanf: formatting error"

-- | An equivalent to @sprintf()@ in C: @'sprintf' fmt@ returns a function that
-- returns a string and whose number of arguments depends on format descriptor
-- @fmt@.
--
-- >>> spec = satisfy (=='A') . satisfy (=='B') . satisfy (=='C')
-- >>> sprintf spec 'A' 'B' 'C'
-- "ABC"
sprintf :: HasCallStack => K7 Tr String r -> r
sprintf (K7 f _) = unTr f (\_ -> id) (\_ -> error msg) ""
  where
    msg = "sprintf: formatting error"

-- | Do nothing.
--
-- >>> pretty (set () nothing) ()
-- Just ""
nothing :: PP0
nothing = id

-- | Turn the given pure transformer into a parsing\/printing pair. That is,
-- return a cassette that provides an output on the one side, and consumes an
-- input on the other, in addition to the string transformations of the given
-- pure transformer. @'set' x p@ provides @x@ as the output of @p@ on the
-- parsing side, and on the printing side accepts an input that is ignored.
set :: a -> PP0 -> PP a
set x ~(K7 f f') = K7 (Tr.pop . f) (Tr.push' x . f')

-- | Turn the given parsing\/printing pair into a pure string transformer. That
-- is, return a cassette that does not produce an output or consume an input.
-- @'unset' x p@ throws away the output of @p@ on the parsing side, and on the
-- printing side sets the input to @x@.
unset :: a -> PP a -> PP0
unset x ~(K7 f f') = K7 (Tr.push x . f) (Tr.pop' . f')

write :: (a -> String) -> Tr r (a -> r)
write f = Tr $ \k k' s x -> k (\s -> k' s x) (s ++ f x)

write0 :: String -> Tr r r
write0 x = Tr $ \k k' s -> unTr (write id) k (\s _ -> k' s) s x

-- | Strip\/add the given string from\/to the output string.
string :: String -> PP0
-- We could implement 'string' in terms of many, satisfy, char and unshift, but
-- don't, purely to reduce unnecessary choice points during parsing.
string x = K7 (write0 x) (Tr $ \k k' s -> maybe (k' s) (k k') $ stripPrefix x s)

-- | Successful only if predicate holds.
satisfy :: (Char -> Bool) -> PP Char
satisfy p = K7 (Tr f) (Tr f')
  where
    f k k' s x
      | p x = k (\s -> k' s x) (s ++ [x])
      | otherwise = k' s x
    f' k k' (c:cs) u
      | p c = k (\cs _ -> k' cs u) cs (u c)
    f' _ k' s u = k' s u

-- | Parse\/print without consuming\/producing any input.
--
-- >>> let spec = lookAhead (satisfy (=='A')) . string "A"
-- >>> parse spec "ABCD"
-- Just 'A'
lookAhead :: PP a -> PP a
lookAhead csst = K7 (Tr g) (Tr g')
  where
    g k k' s = let K7 (Tr f) _ = csst in f (\k' _ -> k k' s) k' s
    g' k k' s = let K7 _ (Tr f') = csst in f' (\k' _ -> k k' s) k' s

-- | Succeeds if input string is empty.
--
-- >>> parse (set () eof) ""
-- Just ()
--
-- >>> parse (set () eof) "ABCD"
-- Nothing
eof :: PP0
eof = K7 (Tr isEmpty) (Tr isEmpty)
  where
    isEmpty k k' "" = k k' ""
    isEmpty _ k' s  = k' s
