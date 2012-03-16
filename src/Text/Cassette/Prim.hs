{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, ScopedTypeVariables #-}
module Text.Cassette.Prim where

import Data.List (stripPrefix)
import Control.Category
import Prelude hiding (flip, id, (.))
import qualified Prelude


data K7 a b c d = K7 { sideA :: a -> b, sideB :: d -> c }

newtype SK7 a b = SK7 { unSK7 :: K7 a b a b }

infixr 9 <>

-- | Composing tapes.
(<>) :: K7 b c b' c' -> K7 a b a' b' -> K7 a c a' c'
-- Irrefutable patterns to support definitions of combinators by coinduction.
~(K7 f f') <> ~(K7 g g') = K7 (f . g) (g' . f')

instance Category SK7 where
  id = SK7 $ K7 id id
  SK7 csst1 . SK7 csst2 = SK7 $ csst1 <> csst2

infixr 8 -->
(-->) = Prelude.flip (<>)

type C r = (String -> r) -> String -> r

type PP a = forall r r'. K7 (C (a -> r)) (C r) (C (a -> r')) (C r')
type PP0  = forall r r'. K7 (C r) (C r) (C r') (C r')

-- | Select the A-side.
play :: K7 a b c d -> a -> b
play csst = sideA csst

-- | Switch the A-side and B-side around.
flip :: K7 a b c d -> K7 d c b a
flip (K7 f g) = K7 g f

parse :: PP a -> String -> Maybe a
parse csst = play csst (\_ _ x -> Just x) (const Nothing)

pretty :: PP a -> a -> Maybe String
pretty csst = play (flip csst) (const Just) (\_ _ -> Nothing) ""

-- Use same priority and associativity as in Parsec.
infixr 1 <|>

(<|>) :: PP a -> PP a -> PP a
K7 f f' <|> K7 g g' =
  K7 (\k k' s -> f k (\s' -> g k k' s) s)
     (\k k' s x -> f' k (\s' -> g' k k' s) s x)

-- | Always fail.
empty :: PP0
empty = K7 (\k k' s -> k' s) (\k k' s -> k' s)

write :: (a -> String) -> C r -> C (a -> r)
write f = \k k' s x -> k (\s -> k' s x) (f x ++ s)

write0 :: String -> C r -> C r
write0 x = \k k' s -> write id k (\s _ -> k' s) s x

unshift :: a -> PP a -> PP0
unshift x ~(K7 f f') =
  K7 (\k k' -> f (\k' s x -> k (\s -> k' s x) s) k')
     (\k k' s -> f' k (\s _ -> k' s) s x)

shift :: a -> PP0 -> PP a
shift x ~(K7 f f') =
  K7 (\k k' -> f (\k' s -> k (\s _ -> k' s) s x) k')
     (\k k' s x -> f' k (\s -> k' s x) s)

consL :: K7 (C ([a] -> r)) (C ([a] -> a -> r))
            (C ([a] -> r')) (C ([a] -> a -> r'))
consL = K7 (\k k' s xs' x -> k (\s _ -> k' s xs' x) s (x:xs'))
           (\k k' s xs -> case xs of
               x:xs' -> k (\s _ _ -> k' s xs) s xs' x
               _ -> k' s xs)

nilL :: PP [a]
nilL = K7 (\k k' s -> k (\s _ -> k' s) s [])
          (\k k' s xs -> case xs of
              [] -> k (\s -> k' s xs) s
              _ -> k' s xs)

many :: PP a -> PP [a]
many b = (consL --> b <> many b) <|> nilL

-- We could implement lit in terms of many, satisfy, char and unshift, but
-- don't, purely to reduce unnecessary choice points during parsing.
lit :: String -> PP0
lit x = K7 (\k k' s -> maybe (k' s) (k k') $ stripPrefix x s) (write0 x)

-- | Successful only if predicate holds.
satisfy :: (Char -> Bool) -> PP Char
satisfy p = K7 f g where
  f k k' (x:xs) | p x = k (\s _ -> k' s) xs x
  f k k' s = k' s
  g k k' s x | p x = k (\s -> k' s x) (x:s)
             | otherwise = k' s x

-- | Parse/print without consuming/producing any input.
lookAhead :: PP a -> PP a
lookAhead (K7 f f') = K7 (\k k' s -> f (\k' _ -> k k' s) k' s) (\k k' s -> f' (\k' _ -> k k' s) k' s)
