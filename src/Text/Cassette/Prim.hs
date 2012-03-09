module Text.Cassette.Prim where

import Data.List


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
K7 f f' <|> K7 g (g' :: RCont r a) =
  K7 (\k k' s -> f k (g k k' s) s)
     (\k k' s x -> f' k (g' (k :: r -> String -> r) (k' :: a -> r) s) s x)

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
kcons = K7 (\k k' s xs x -> k (const (k' xs x)) s (x:xs)) (\k k' s xs -> case xs of
                                              x:xs -> k (\_ _ -> k' xs) s xs x
                                              _ -> k' xs)

knil :: PP [a]
knil = K7 (\k k' s -> k (const k') s []) (\k k' s xs -> case xs of
                                                [] -> k (k' xs) s
                                                _ -> k' xs)

unshift :: a -> PP a -> PP0
unshift x ~(K7 f f') = K7 (\k k' -> f (\k' s x -> k (k' x) s) k') (\k k' s -> f' k (const k') s x)

shift :: a -> PP0 -> PP a
shift x ~(K7 f f') = K7 (\k k' -> f (\k' s -> k (const k') s x) k') (\k k' s x -> f' k (k' x) s)

many :: PP a -> PP [a]
many b = ((b <> many b) --> kcons) <|> knil

lit :: String -> PP0
lit x = K7 (\k k' -> maybe k' (k k') . stripPrefix x) (write0 x)

char :: PP Char
char = K7 (\k k' s -> if null s then k' else k (const k') (tail s) (head s))
          (write (:[]))

int :: PP Int
int = K7 undefined undefined

-- | Ornamentally select a side.
play :: (K7 a b -> c) -> K7 a b -> c
play side csst = side csst

parse :: PP a -> String -> Maybe a
parse csst = play sideA csst (\_ _ x -> Just x) Nothing

pretty :: PP a -> a -> Maybe String
pretty csst = play sideB csst (const Just) (const Nothing) ""
