{-# LANGUAGE RankNTypes #-}
module Text.Cassette.Prim where

data K7 a b c d = K7 { sideA :: a -> b, sideB :: d -> c }

type C r = (String -> r) -> String -> r

type PP a = forall r r'. K7 (C (a -> r)) (C r) (C (a -> r')) (C r')
type PP0  = forall r r'. K7 (C r) (C r) (C r') (C r')

nothing :: PP0
unshift :: a -> PP a -> PP0
shift :: a -> PP0 -> PP a
