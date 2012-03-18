{-# LANGUAGE RankNTypes #-}
module Text.Cassette.Leads where

import Text.Cassette.Prim


-- | The type of binary leads, parameterized by the type of the left operand,
-- the right operand, and the type of the result.
type BinL a b c =
  forall r r'. K7 (C (c -> r))  (C (b -> a -> r))
                  (C (c -> r')) (C (b -> a -> r'))

consL :: BinL a [a] [a]
consL = K7 (\k k' s xs' x -> k (\s _ -> k' s xs' x) s (x:xs'))
           (\k k' s xs -> case xs of
               x:xs' -> k (\s _ _ -> k' s xs) s xs' x
               _ -> k' s xs)

nilL :: PP [a]
nilL = shift [] nothing

-- | The type of binary leads, parameterized by the type of the left operand,
-- the right operand, and the type of the result.
type UnL a b =
  forall r r'. K7 (C (b -> r))  (C (a -> r))
                  (C (b -> r')) (C (a -> r'))

justL :: UnL a (Maybe a)
justL = K7 (\k k' s x -> k (\s _ -> k' s x) s (Just x))
           (\k k' s mb -> maybe (k' s mb) (k (\s _ -> k' s mb) s) mb)

nothingL :: PP (Maybe a)
nothingL = shift Nothing nothing
