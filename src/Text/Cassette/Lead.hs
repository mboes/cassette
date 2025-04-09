{-# LANGUAGE RankNTypes #-}

module Text.Cassette.Lead where

import Text.Cassette.Prim

-- | The type of unary leads, parameterized by the type of the operand and the
-- type of the result.
type UnL a b = forall r. K7 (C (b -> r)) (C (a -> r))

-- | The type of binary leads, parameterized by the type of the left operand,
-- the right operand, and the type of the result.
type BinL a b c = forall r. K7 (C (c -> r)) (C (b -> a -> r))

-- | Lift a pair of symmetric functions to a lead.
liftL :: (a -> b) -> (b -> a) -> UnL a b
liftL f f' =
    K7 (\k k' s x -> k (\s _ -> k' s x) s (f x))
       (\k k' s y -> k (\s _ -> k' s y) s (f' y))

-- | Iterates a one step construction function (resp. deconstruction)
-- function, i.e. a lead, thus obtaining a right fold (resp. unfold). The
-- resulting lead is a catamorphism on one side and an anamorpism on the
-- other, hence the name. The type of this function is the same as that of
-- 'foldr', lifted to cassettes.
catanar :: BinL a b b -> BinL b [a] b
catanar (K7 f f') = K7 g g' where
    g k k' s xs@[]      z = k (\s _ -> k' s xs z) s z
    g k k' s xs@(x:xs') z =
      g (\k' s z -> f k (\s _ _ -> k' s z) s z x) (\s _ _ -> k' s xs z) s xs' z
    g' k k' s z =
      f' (\k' s z x -> g' (\k' s xs' z -> k k' s (x:xs') z) (\s _ -> k' s z x) s z)
         (\s _ -> k (\s _ _ -> k' s z) s [] z) s z

-- | Iterates a one step construction function (resp. deconstruction)
-- function, i.e. a lead, thus obtaining a left fold (resp. unfold). The
-- resulting lead is a catamorphism on one side and an anamorpism on the
-- other, hence the name. The type of this function is the same as that of
-- 'foldl', lifted to cassettes.
catanal :: BinL a b a -> BinL a [b] a
catanal (K7 f f') = K7 g (g' []) where
    g k k' s xs@[]      z = k (\s _ -> k' s xs z) s z
    g k k' s xs@(x:xs') z =
      f (\k' s z -> g k (\s _ _ -> k' s z) s xs' z) (\s _ _ -> k' s xs z) s x z
    g' xs' k k' s z =
      f' (\k' s x z -> g' (x:xs') k (\s _ -> k' s x z) s z) (\s _ -> k (\s _ _ -> k' s z) s xs' z) s z

consL :: BinL a [a] [a]
consL =
  K7 (\k k' s xs' x -> k (\s _ -> k' s xs' x) s (x:xs'))
     (\k k' s xs -> case xs of
         x:xs' -> k (\s _ _ -> k' s xs) s xs' x
         _ -> k' s xs)

nilL :: PP [a]
nilL = shift [] nothing

justL :: UnL a (Maybe a)
justL =
  K7 (\k k' s x -> k (\s _ -> k' s x) s (Just x))
     (\k k' s mb -> maybe (k' s mb) (k (\s _ -> k' s mb) s) mb)

nothingL :: PP (Maybe a)
nothingL = shift Nothing nothing

pairL :: BinL a b (a, b)
pairL =
  K7 (\k k' s x2 x1 -> k (\s _ -> k' s x2 x1) s (x1, x2))
     (\k k' s t@(x1, x2) -> k (\s _ _ -> k' s t) s x2 x1)

tripleL :: K7 (C ((a,b,c) -> r)) (C (c -> b -> a -> r))
tripleL =
  K7 (\k k' s x3 x2 x1 -> k (\s _ -> k' s x3 x2 x1) s (x1, x2, x3))
     (\k k' s t@(x1, x2, x3) -> k (\s _ _ _ -> k' s t) s x3 x2 x1)

quadrupleL :: K7 (C ((a,b,c,d) -> r)) (C (d -> c -> b -> a -> r))
quadrupleL =
  K7 (\k k' s x4 x3 x2 x1 -> k (\s _ -> k' s x4 x3 x2 x1) s (x1, x2, x3, x4))
     (\k k' s t@(x1, x2, x3, x4) -> k (\s _ _ _ _ -> k' s t) s x4 x3 x2 x1)
