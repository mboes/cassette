{-# LANGUAGE RankNTypes #-}

module Text.Cassette.Lead where

import Control.Lens qualified as Lens
import Text.Cassette.Internal.Tr (Tr(..))
import Text.Cassette.Prim

-- | Unary leads. A lead of type @'UnL' s a@ projects a component @a@ from outer
-- type @s@.
type UnL s a = forall r. K7 Tr (s -> r) (a -> r)

-- | Binary leads. A lead of type @'BinL' s a b@ projects components @a@, @b@
-- from outer type @s@.
type BinL s a b = forall r. K7 Tr (s -> r) (b -> a -> r)

-- | Lift an isomorphism (see the [lens](https://hackage.haskell.org/package/lens) library) to a lead.
isoL :: Lens.Iso s s a a -> UnL s a
isoL l =
  K7 (Tr $ \k k' s x -> k (\s _ -> k' s x) s (Lens.view (Lens.from l) x))
     (Tr $ \k k' s y -> k (\s _ -> k' s y) s (Lens.view l y))

-- | Lift a prism (see [lens](https://hackage.haskell.org/package/lens) library) to a lead.
prismL :: Lens.Prism s s a a -> UnL s a
prismL l = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s x = k (\s _ -> k' s x) s (Lens.review l x)
    leadin k k' s t = case Lens.preview l t of
      Nothing -> k' s t
      Just x -> k (\s _ -> k' s t) s x

-- | Iterates a one step construction function (resp. deconstruction) function,
-- i.e. a lead, thus obtaining a right fold (resp. unfold). The resulting lead
-- is a catamorphism on one side and an anamorpism on the other, hence the name.
-- The type of this function is the same as that of 'foldr', lifted to
-- cassettes.
catanar :: BinL b a b -> BinL b b [a]
catanar (K7 f f') = K7 (Tr g) (Tr g') where
    g k k' s xs@[]      z = k (\s _ -> k' s xs z) s z
    g k k' s xs@(x:xs') z =
      g (\k' s z -> unTr f k (\s _ _ -> k' s z) s z x) (\s _ _ -> k' s xs z) s xs' z
    g' k k' s z =
      unTr f' (\k' s z x -> g' (\k' s xs' z -> k k' s (x:xs') z) (\s _ -> k' s z x) s z) (\s _ -> k (\s _ _ -> k' s z) s [] z) s z

-- | Iterates a one step construction function (resp. deconstruction) function,
-- i.e. a lead, thus obtaining a left fold (resp. unfold). The resulting lead is
-- a catamorphism on one side and an anamorpism on the other, hence the name.
-- The type of this function is the same as that of 'foldl', lifted to
-- cassettes.
catanal :: BinL a a b -> BinL a a [b]
catanal (K7 f f') = K7 (Tr g) (Tr $ g' []) where
    g k k' s xs@[]      z = k (\s _ -> k' s xs z) s z
    g k k' s xs@(x:xs') z =
      unTr f (\k' s z -> g k (\s _ _ -> k' s z) s xs' z) (\s _ _ -> k' s xs z) s x z
    g' xs' k k' s z =
      unTr f' (\k' s x z -> g' (x:xs') k (\s _ -> k' s x z) s z) (\s _ -> k (\s _ _ -> k' s z) s xs' z) s z

consL :: BinL [a] a [a]
consL =
  K7 (Tr $ \k k' s xs' x -> k (\s _ -> k' s xs' x) s (x:xs'))
     (Tr $ \k k' s xs -> case xs of
         x:xs' -> k (\s _ _ -> k' s xs) s xs' x
         _ -> k' s xs)

nilL :: PP [a]
nilL = set [] nothing

justL :: UnL (Maybe a) a 
justL =
  K7 (Tr $ \k k' s x -> k (\s _ -> k' s x) s (Just x))
     (Tr $ \k k' s mb -> maybe (k' s mb) (k (\s _ -> k' s mb) s) mb)

nothingL :: PP (Maybe a)
nothingL = set Nothing nothing

pairL :: BinL (a, b) a b
pairL =
  K7 (Tr $ \k k' s x2 x1 -> k (\s _ -> k' s x2 x1) s (x1, x2))
     (Tr $ \k k' s t@(x1, x2) -> k (\s _ _ -> k' s t) s x2 x1)

tripleL :: K7 Tr ((a,b,c) -> r) (c -> b -> a -> r)
tripleL =
  K7 (Tr $ \k k' s x3 x2 x1 -> k (\s _ -> k' s x3 x2 x1) s (x1, x2, x3))
     (Tr $ \k k' s t@(x1, x2, x3) -> k (\s _ _ _ -> k' s t) s x3 x2 x1)

quadrupleL :: K7 Tr ((a,b,c,d) -> r) (d -> c -> b -> a -> r)
quadrupleL =
  K7 (Tr $ \k k' s x4 x3 x2 x1 -> k (\s _ -> k' s x4 x3 x2 x1) s (x1, x2, x3, x4))
     (Tr $ \k k' s t@(x1, x2, x3, x4) -> k (\s _ _ _ _ -> k' s t) s x4 x3 x2 x1)
