{-# LANGUAGE RankNTypes #-}
module Text.Cassette.Leads where

import Text.Cassette.Prim


-- | The type of binary leads, parameterized by the type of the left operand,
-- the right operand, and the type of the result.
type UnL a b =
  forall r r'. K7 (C (b -> r))  (C (a -> r))
                  (C (b -> r')) (C (a -> r'))

-- | The type of binary leads, parameterized by the type of the left operand,
-- the right operand, and the type of the result.
type BinL a b c =
  forall r r'. K7 (C (c -> r))  (C (b -> a -> r))
                  (C (c -> r')) (C (b -> a -> r'))

catanal :: BinL a b a -> BinL a [b] a
catanal (K7 f f') = K7 g (g' []) where
  g k k' s xs@[]      z = k (\s _ -> k' s xs z) s z
  g k k' s xs@(x:xs') z =
    f (\k' s z -> g k (\s _ _ -> k' s z) s xs' z) (\s _ _ -> k' s xs z) s x z
  g' xs' k k' s z =
    f' (\k' s x z -> g' (x:xs') k (\s _ -> k' s x z) s z) (\s _ -> k (\s _ _ -> k' s z) s xs' z) s z

catanar :: BinL a b b -> BinL b [a] b
catanar (K7 f f') = K7 g g' where
  g k k' s xs@[]      z = k (\s _ -> k' s xs z) s z
  g k k' s xs@(x:xs') z =
    g (\k' s z -> f k (\s _ _ -> k' s z) s z x) (\s _ _ -> k' s xs z) s xs' z
  g' k k' s z =
    f' (\k' s z x -> g' (\k' s xs' z -> k k' s (x:xs') z) (\s _ -> k' s z x) s z)
       (\s _ -> k (\s _ _ -> k' s z) s [] z) s z

consL :: BinL a [a] [a]
consL = K7 (\k k' s xs' x -> k (\s _ -> k' s xs' x) s (x:xs'))
           (\k k' s xs -> case xs of
               x:xs' -> k (\s _ _ -> k' s xs) s xs' x
               _ -> k' s xs)

nilL :: PP [a]
nilL = shift [] nothing

justL :: UnL a (Maybe a)
justL = K7 (\k k' s x -> k (\s _ -> k' s x) s (Just x))
           (\k k' s mb -> maybe (k' s mb) (k (\s _ -> k' s mb) s) mb)

nothingL :: PP (Maybe a)
nothingL = shift Nothing nothing
