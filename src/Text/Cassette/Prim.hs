{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, ScopedTypeVariables #-}
module Text.Cassette.Prim where

import Data.List (stripPrefix)
import Prelude hiding (flip)


data K7 a b c d = K7 { sideA :: a -> b, sideB :: d -> c }

type SK7 a b = K7 a b b a

infixr 9 <>

-- | Composing tapes.
(<>) :: K7 b c b' c' -> K7 a b a' b' -> K7 a c a' c'
-- Irrefutable patterns to support definitions of combinators by coinduction.
~(K7 f f') <> ~(K7 g g') = K7 (f . g) (g' . f')

infixr 8 -->
(-->) = (<>)

type C r = (String -> r) -> String -> r

type PP a = forall r r'. K7 (C (a -> r)) (C r) (C (a -> r')) (C r')
type PP0  = forall r r'. K7 (C r) (C r) (C r') (C r')

-- Use same priority and associativity as in Parsec.
infixr 1 <|>

(<|>) :: PP a -> PP a -> PP a
K7 f f' <|> K7 g g' =
  K7 (\k k' s -> f k (\s' -> g k k' s) s)
     (\k k' s x -> f' k (\s' -> g' k k' s) s x)

empty :: PP0
empty = K7 (\k k' s -> k' s) (\k k' s -> k' s)

-- get :: Cont r String
-- get = \k k' s -> k (const k') s s
--
-- set :: String -> Cont0 r
-- set s' = \k k' s -> k k' s'
--
write :: (a -> String) -> C r -> C (a -> r)
write f = \k k' s x -> k (\s -> k' s x) (f x ++ s)

write0 :: String -> C r -> C r
write0 x = \k k' s -> write id k (\s -> const (k' s)) s x

kcons :: K7 (C ([a] -> r)) (C ([a] -> a -> r))
            (C ([a] -> r')) (C ([a] -> a -> r'))
kcons = K7 (\k k' s xs' x -> k (\s -> const (k' s xs' x)) s (x:xs'))
           (\k k' s xs -> case xs of
               x:xs' -> k (\s _ _ -> k' s xs) s xs' x
               _ -> k' s xs)

knil :: PP [a]
knil = K7 (\k k' s -> k (\s -> const (k' s)) s [])
          (\k k' s xs -> case xs of
              [] -> k (\s -> k' s xs) s
              _ -> k' s xs)

idk = K7 id id

many :: PP a -> PP [a]
many b = ((b <> many b) --> kcons) <|> knil

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
