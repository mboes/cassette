module Bi where

import Control.Monad
import Data.Maybe
import Data.List


data Bi a b = Bi { first :: a, second :: b }

infixr 9 <>

-- (<>) :: Bi (b -> c) (c' -> b') -> Bi (a -> b) (b' -> a') -> Bi (a -> c) (c' -> a')
-- Bi f f' <> Bi g g' = Bi (f . g) (g' . f')

--Bi f f' <> Bi g g' = Bi (f . g) (g' . f')
Bi f f' <> Bi g g' = Bi (\k -> f k . g (const k))
                        (\k -> g' (const k) . f' k)

infixr 8 -->
Bi f f' --> Bi g g' = Bi (\k -> f k . g k) (\k -> g' k . f' k)

--newtype ContT r m a  = ContT { runContT  :: (String -> a -> m r) -> (String -> m r) }
--newtype RContT r m a = ContT { runRContT :: (String -> m r) -> (String -> a -> m r) }

type ContT r a  = r -> (String -> a -> r) -> (String -> r)
type RContT r a = r -> (String -> r) -> (String -> a -> r)

type PP a = forall r r'. Bi (ContT r a) (RContT r' a)

type ContT0 r  = r -> (String -> r) -> (String -> r)
type RContT0 r = ContT0 r

type PP0 = forall r r'. Bi (ContT0 r) (RContT0 r')

-- instance Monad (ContT r m) where
--   return x = ContT $ \k s -> k s x
--   m >>= f = ContT $ \k s -> runContT m (\s' x -> runContT (f x) k s') s

infixl 3 <|>

(<|>) :: PP a -> PP a -> PP a
Bi f f' <|> Bi g g' = Bi (\k k' s -> f (g k k' s) k' s)
                         (\k k' s x -> f' (g' k k' s x) k' s x)

empty :: PP0
empty = Bi (\k k' s -> k) (\k k' s -> k)

get :: ContT r String
get = \k k' s -> k' s s

set :: String -> ContT r ()
set s' = \k k' s -> k' s' ()

write :: (a -> String) -> RContT r a
write f = \k k' s x -> k' (s ++ f x)

write0 :: String -> RContT0 r
write0 x = \k k' s -> write id k k' s x

bicons = Bi (\k k' s xs x -> k' s (x:xs)) (\k k' s xs -> case xs of
                                              x:xs -> k' s xs x
                                              _ -> k)

binil = Bi (\k k' s -> k' s []) (\k k' s xs -> case xs of
                                                [] -> k' s
                                                _ -> k)

many :: PP a -> PP [a]
--many b = ((b <> many b) --> bicons) <|> binil
many b =
  ((Bi (\k -> first b k . first (many b) (const k))
       (\k -> second (many b) (const k) . second b k)) --> bicons) <|> binil

lit :: String -> PP0
lit x = Bi (\k k' -> maybe k k' . stripPrefix x) (write0 x)

char :: PP Char
char = Bi (\k k' s -> if null s then k else k' (tail s) (head s))
          (write (:[]))

int :: PP Int
int = Bi undefined undefined

parse m = first m (\_ x -> x)

pretty m = second m (\x -> x) ""
