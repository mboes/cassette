{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, ScopedTypeVariables #-}
module Text.Cassette.Prim where

import Data.List (stripPrefix)


data K7 a b = K7 { sideA :: a, sideB :: b }

infixr 9 <>

-- | Composing tapes.
(<>) :: K7 (b -> c) (c' -> b') -> K7 (a -> b) (b' -> a') -> K7 (a -> c) (c' -> a')
-- Irrefutable patterns to support definitions of combinators by coinduction.
~(K7 f f') <> ~(K7 g g') = K7 (f . g) (g' . f')

infixr 8 -->
(-->) = (<>)

type Cont r a  = ((a -> r) -> String -> a -> r) -> (r -> String -> r)
type RCont r a = (r -> String -> r) -> ((a -> r) -> String -> a -> r)

type PP a = forall r r'. K7 (Cont r a) (RCont r' a)

type Cont0 r  = (r -> String -> r) -> (r -> String -> r)
type RCont0 r = Cont0 r

type PP0 = forall r r'. K7 (Cont0 r) (RCont0 r')

-- Use same priority and associativity as in Parsec.
infixr 1 <|>

(<|>) :: PP a -> PP a -> PP a
K7 f f' <|> K7 g g' =
  K7 (\k k' s -> f k (g k k' s) s)
     (\k k' s x -> f' k (g' k k' s) s x)

empty :: PP0
empty = K7 (\k k' s -> k') (\k k' s -> k')

get :: Cont r String
get = \k k' s -> k (const k') s s

set :: String -> Cont0 r
set s' = \k k' s -> k k' s'

write :: (a -> String) -> RCont r a
write f = \k k' s x -> k (k' x) (f x ++ s)

write0 :: String -> RCont0 r
write0 x = \k k' s -> write id k (const k') s x

kcons :: K7 ((([a] -> r) -> String -> [a] -> r) -> ([a] -> a -> r) -> String -> [a] -> a -> r)
             ((([a] -> a -> r') -> String -> [a] -> a -> r') -> ([a] -> r') -> String -> [a] -> r')
kcons = K7 (\k k' s xs' x -> k (const (k' xs' x)) s (x:xs'))
           (\k k' s xs -> case xs of
               x:xs' -> k (\_ _ -> k' xs) s xs' x
               _ -> k' xs)

knil :: PP [a]
knil = K7 (\k k' s -> k (const k') s [])
          (\k k' s xs -> case xs of
              [] -> k (k' xs) s
              _ -> k' xs)

idk = K7 id id

-- infixr <$, $>
--
-- x <$ pp = unshift x pp
--
-- x $> pp = shift x pp
--
-- kcons' = K7
--              (\xs -> case xs of
--                  x:xs' -> sideB (xs $> x <$ xs' <$ idk)
--                  _ -> sideB (xs $> error "wrong constructor"))

-- run $ reset ((shift (\k -> ret (\x xs -> k (x:xs)))))
-- a -> [a] -> [a]

unshiftA f = (\k k' -> f (\k' s x -> k (k' x) s) k')
unshiftB x f' = (\k k' s -> f' k (const k') s x)

shiftA x f = (\k k' -> f (\k' s -> k (const k') s x) k')
shiftB f' = (\k k' s x -> f' k (k' x) s)


--unshift :: a -> PP a -> PP0
unshift x ~(K7 f f') = K7 (\k k' -> f (\k' s x -> k (k' x) s) k') (\k k' s -> f' k (const k') s x)

--shift :: a -> PP0 -> PP a
shift x ~(K7 f f') = K7 (\k k' -> f (\k' s -> k (const k') s x) k') (\k k' s x -> f' k (k' x) s)

many :: PP a -> PP [a]
many b = ((b <> many b) --> kcons) <|> knil

-- We could implement lit in terms of many, satisfy, char and unshift, but
-- don't, purely to reduce unnecessary choice points during parsing.
lit :: String -> PP0
lit x = K7 (\k k' -> maybe k' (k k') . stripPrefix x) (write0 x)

-- | Successful only if predicate holds.
satisfy :: (Char -> Bool) -> PP Char
satisfy p = K7 f g where
  f k k' (x:xs) | p x = k (const k') xs x
  f k k' _ = k'
  g k k' s x | p x = write (:[]) k k' s x -- k (k' x) (x:s)
             | otherwise = k' x

-- | Ornamentally select a side.
play :: (K7 a b -> c) -> K7 a b -> c
play side csst = side csst

parse :: PP a -> String -> Maybe a
parse csst = play sideA csst (\_ _ x -> Just x) Nothing

pretty :: PP a -> a -> Maybe String
pretty csst = play sideB csst (const Just) (const Nothing) ""
