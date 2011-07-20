module Bi where

import Data.List


data Bi a b = Bi { first :: a, second :: b }

infixr 9 <>

(<>) :: Bi (b -> c) (c' -> b') -> Bi (a -> b) (b' -> a') -> Bi (a -> c) (c' -> a')
Bi f f' <> Bi g g' = Bi (f . g) (g' . f')

infixr 8 -->
(-->) = (<>)

type ContT r a  = ((a -> r) -> String -> a -> r) -> (r -> String -> r)
type RContT r a = (r -> String -> r) -> ((a -> r) -> String -> a -> r)

type PP a = forall r r'. Bi (ContT r a) (RContT r' a)

type ContT0 r  = (r -> String -> r) -> (r -> String -> r)
type RContT0 r = ContT0 r

type PP0 = forall r r'. Bi (ContT0 r) (RContT0 r')

infixl 3 <|>

(<|>) :: PP a -> PP a -> PP a
Bi f f' <|> Bi g (g' :: RContT r a) =
  Bi (\k k' s -> f k (g k k' s) s)
     (\k k' s x -> f' k (g' (k :: r -> String -> r) (k' :: a -> r) s) s x)

empty :: PP0
empty = Bi (\k k' s -> k') (\k k' s -> k')

get :: ContT r String
get = \k k' s -> k (const k') s s

set :: String -> ContT r ()
set s' = \k k' s -> k (const k') s' ()

write :: (a -> String) -> RContT r a
write f = \k k' s x -> k (k' x) (s ++ f x)

write0 :: String -> RContT0 r
write0 x = \k k' s -> write id k (const k') s x

bicons :: Bi ((([a] -> r) -> String -> [a] -> r) -> ([a] -> a -> r) -> String -> [a] -> a -> r)
             ((([a] -> a -> r') -> String -> [a] -> a -> r') -> ([a] -> r') -> String -> [a] -> r')
bicons = Bi (\k k' s xs x -> k (const (k' xs x)) s (x:xs)) (\k k' s xs -> case xs of
                                              x:xs -> k (\_ _ -> k' xs) s xs x
                                              _ -> k' xs)

binil :: PP [a]
binil = Bi (\k k' s -> k (const k') s []) (\k k' s xs -> case xs of
                                                [] -> k (k' xs) s
                                                _ -> k' xs)

many :: PP a -> PP [a]
--many b = ((b <> many b) --> bicons) <|> binil
many b =
  ((Bi (first b . first (many b))
       (second (many b) . second b)) --> bicons) <|> binil

lit :: String -> PP0
lit x = Bi (\k k' -> maybe k' (k k') . stripPrefix x) (write0 x)

char :: PP Char
char = Bi (\k k' s -> if null s then k' else k (const k') (tail s) (head s))
          (write (:[]))

int :: PP Int
int = Bi undefined undefined

parse m = first m (\_ x -> x)

pretty m = second m (\x -> x) ""
