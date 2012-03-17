module Text.Cassette.Leads where

import {-# SOURCE #-} Text.Cassette.Prim


consL :: K7 (C ([a] -> r)) (C ([a] -> a -> r))
            (C ([a] -> r')) (C ([a] -> a -> r'))
consL = K7 (\k k' s xs' x -> k (\s _ -> k' s xs' x) s (x:xs'))
           (\k k' s xs -> case xs of
               x:xs' -> k (\s _ _ -> k' s xs) s xs' x
               _ -> k' s xs)

nilL :: PP [a]
nilL = shift [] nothing

justL :: K7 (C (Maybe a -> r)) (C (a -> r))
            (C (Maybe a -> r')) (C (a -> r'))
justL = K7 (\k k' s x -> k (\s _ -> k' s x) s (Just x))
           (\k k' s mb -> maybe (k' s mb) (k (\s _ -> k' s mb) s) mb)

nothingL :: PP (Maybe a)
nothingL = shift Nothing nothing
