{-# LANGUAGE RankNTypes #-}

module Text.Cassette.Lead where

import Control.Lens qualified as Lens
import Text.Cassette.Internal.Tr (Tr(..))
import Text.Cassette.Prim

-- | Nullary leads. Synonym to 'PP'.
type NullL s = forall r. K7 Tr (s -> r) r

-- | Unary leads. A lead of type @'UnL' s a@ projects/injects a component @a@
-- from/into outer type @s@.
type UnL s a = forall r. K7 Tr (s -> r) (a -> r)

-- | Binary leads. A lead of type @'BinL' s a b@ projects/injects components
-- @a@, @b@ from/into outer type @s@.
type BinL s a b = forall r. K7 Tr (s -> r) (b -> a -> r)

-- | Ternary leads. A lead of type @'TernL' s a b c@ projects/injects components
-- @a@, @b@, @c@ from/into outer type @s@.
type TernL s a b c = forall r. K7 Tr (s -> r) (c -> b -> a -> r)

-- | Quaternary leads. A lead of type @'QuaternL' s a b c d@ projects/injects
-- components @a@, @b@, @c@, @d@ from/into outer type @s@.
type QuaternL s a b c d = forall r. K7 Tr (s -> r) (d -> c -> b -> a -> r)

-- | Lift an isomorphism (see the
-- [lens](https://hackage.haskell.org/package/lens) library) to a lead.
isoL :: forall s a. Lens.Iso s s a a -> UnL s a
isoL l = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s u = k (\s _ -> k' s u) s (\x -> u (Lens.view (Lens.from l) x))
    leadin k k' s t = k (\s _ -> k' s t) s (Lens.view l t)

-- | Lift a prism (see [lens](https://hackage.haskell.org/package/lens) library)
-- to a lead.
prismL :: Lens.Prism s s a a -> UnL s a
prismL l = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s u = k (\s _ -> k' s u) s (\x -> u (Lens.review l x))
    leadin k k' s t = case Lens.preview l t of
      Nothing -> k' s t
      Just x -> k (\s _ -> k' s t) s x

-- | Iterates a one step construction function (resp. deconstruction) function,
-- i.e. a lead, thus obtaining a right fold (resp. unfold). The resulting lead
-- is a catamorphism on one side and an anamorpism on the other, hence the name.
-- The type of this function is the same as that of 'foldr', lifted to
-- cassettes.
catanar :: BinL b a b -> BinL b b [a]
catanar _ = error "unimplemented"
-- catanar (K7 (Tr f) (Tr f')) = K7 (Tr g) (Tr g')
--   where
--     g k k' s xs@[]      z = k (\s _ -> k' s xs z) s z
--     g k k' s xs@(x:xs') z =
--       g (\k' s z -> f k (\s _ _ -> k' s z) s z x) (\s _ _ -> k' s xs z) s xs' z
--     g' k k' s z =
--       f' (\k' s z x -> g' (\k' s xs' z -> k k' s (x:xs') z) (\s _ -> k' s z x) s z) (\s _ -> k (\s _ _ -> k' s z) s [] z) s z

-- | Iterates a one step construction function (resp. deconstruction) function,
-- i.e. a lead, thus obtaining a left fold (resp. unfold). The resulting lead is
-- a catamorphism on one side and an anamorpism on the other, hence the name.
-- The type of this function is the same as that of 'foldl', lifted to
-- cassettes.
catanal :: BinL a a b -> BinL a a [b]
catanal _ = error "unimplemented"
-- catanal (K7 (Tr f) (Tr f')) = K7 (Tr g) (Tr (g' []))
--   where
--     g k k' s xs@[]      z = k (\s _ -> k' s xs z) s z
--     g k k' s xs@(x:xs') z =
--       f (\k' s z -> g k (\s _ _ -> k' s z) s xs' z) (\s _ _ -> k' s xs z) s x z
--     g' xs' k k' s z =
--       f' (\k' s x z -> g' (x:xs') k (\s _ -> k' s x z) s z) (\s _ -> k (\s _ _ -> k' s z) s xs' z) s z

-- | '(:)' lead.
consL :: BinL [a] a [a]
consL = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s u = k (\s _ -> k' s u) s (\xs' x -> u (x:xs'))
    leadin k k' s xs@(x:xs') = k (\s _ _ -> k' s xs) s xs' x
    leadin _ k' s xs = k' s xs

-- | '[]' lead.
nilL :: PP [a]
nilL = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s u = k (\s _ -> k' s u) s (u [])
    leadin k k' s xs@[] = k (\s -> k' s xs) s
    leadin _ k' s xs = k' s xs

-- | 'Just' lead.
justL :: UnL (Maybe a) a 
justL = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s u = k (\s _ -> k' s u) s (\x -> u (Just x))
    leadin k k' s mb@(Just x) = k (\s _ -> k' s mb) s x
    leadin _ k' s mb = k' s mb

-- | 'Nothing' lead.
nothingL :: PP (Maybe a)
nothingL = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s u = k (\s _ -> k' s u) s (u Nothing)
    leadin k k' s mb@Nothing = k (\s -> k' s mb) s
    leadin _ k' s mb = k' s mb

-- | Construct/destruct a pair.
pairL :: BinL (a, b) a b
pairL = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s u = k (\s _ -> k' s u) s (\x2 x1 -> u (x1, x2))
    leadin k k' s t@(x1, x2) = k (\s _ _ -> k' s t) s x2 x1

-- | Construct/destruct a 3-tuple.
tripleL :: TernL (a, b, c) a b c
tripleL = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s u = k (\s _ -> k' s u) s (\x3 x2 x1 -> u (x1, x2, x3))
    leadin k k' s t@(x1, x2, x3) = k (\s _ _ _ -> k' s t) s x3 x2 x1

-- | Construct/destruct a 4-tuple.
quadrupleL :: QuaternL (a, b, c, d) a b c d
quadrupleL = K7 (Tr leadout) (Tr leadin)
  where
    leadout k k' s u = k (\s _ -> k' s u) s (\x4 x3 x2 x1 -> u (x1, x2, x3, x4))
    leadin k k' s t@(x1, x2, x3, x4) = k (\s _ _ _ _ -> k' s t) s x4 x3 x2 x1
