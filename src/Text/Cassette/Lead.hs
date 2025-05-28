{-# LANGUAGE RankNTypes #-}

module Text.Cassette.Lead where

import Control.Lens qualified as Lens
import Text.Cassette.Internal.Tr (Tr(..))
import Text.Cassette.Prim

-- | Nullary leads. Synonym to 'PP'.
type NullL s = forall r. K7 Tr r (s -> r)

-- | Unary leads. A lead of type @'UnL' s a@ projects/injects a component @a@
-- from/into outer type @s@.
type UnL s a = forall r. K7 Tr (a -> r) (s -> r)

-- | Binary leads. A lead of type @'BinL' s a b@ projects/injects components
-- @a@, @b@ from/into outer type @s@.
type BinL s a b = forall r. K7 Tr (a -> b -> r) (s -> r)

-- | Ternary leads. A lead of type @'TernL' s a b c@ projects/injects components
-- @a@, @b@, @c@ from/into outer type @s@.
type TernL s a b c = forall r. K7 Tr (a -> b -> c -> r) (s -> r)

-- | Quaternary leads. A lead of type @'QuaternL' s a b c d@ projects/injects
-- components @a@, @b@, @c@, @d@ from/into outer type @s@.
type QuaternL s a b c d = forall r. K7 Tr (a -> b -> c -> d -> r) (s -> r)

-- | Lift an isomorphism (see the
-- [lens](https://hackage.haskell.org/package/lens) library) to a lead.
isoL :: forall s a. Lens.Iso s s a a -> UnL s a
isoL l = K7 (Tr leadin) (Tr leadout)
  where
    leadin k k' s t = k (\s _ -> k' s t) s (Lens.view l t)
    leadout k k' s u = k (\s _ -> k' s u) s (\x -> u (Lens.view (Lens.from l) x))

-- | Lift a prism (see [lens](https://hackage.haskell.org/package/lens) library)
-- to a lead.
prismL :: Lens.Prism s s a a -> UnL s a
prismL l = K7 (Tr leadin) (Tr leadout)
  where
    leadin k k' s t = case Lens.preview l t of
      Nothing -> k' s t
      Just x -> k (\s _ -> k' s t) s x
    leadout k k' s u = k (\s _ -> k' s u) s (\x -> u (Lens.review l x))

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
consL = K7 (Tr leadin) (Tr leadout)
  where
    leadin k k' s xs@(x:xs') = k (\s _ _ -> k' s xs) s x xs'
    leadin _ k' s xs = k' s xs
    leadout k k' s u = k (\s _ -> k' s u) s (\x xs' -> u (x:xs'))

-- | '[]' lead.
nilL :: NullL [a]
nilL = K7 (Tr leadin) (Tr leadout)
  where
    leadin k k' s xs@[] = k (\s -> k' s xs) s
    leadin _ k' s xs = k' s xs
    leadout k k' s u = k (\s _ -> k' s u) s (u [])

-- | 'Just' lead.
justL :: UnL (Maybe a) a 
justL = K7 (Tr leadin) (Tr leadout)
  where
    leadin k k' s mb@(Just x) = k (\s _ -> k' s mb) s x
    leadin _ k' s mb = k' s mb
    leadout k k' s u = k (\s _ -> k' s u) s (\x -> u (Just x))

-- | 'Nothing' lead.
nothingL :: PP (Maybe a)
nothingL = K7 (Tr leadin) (Tr leadout)
  where
    leadin k k' s mb@Nothing = k (\s -> k' s mb) s
    leadin _ k' s mb = k' s mb
    leadout k k' s u = k (\s _ -> k' s u) s (u Nothing)

-- | Construct/destruct a pair.
pairL :: BinL (a, b) a b
pairL = K7 (Tr leadin) (Tr leadout)
  where
    leadin k k' s t@(x1, x2) = k (\s _ _ -> k' s t) s x1 x2
    leadout k k' s u = k (\s _ -> k' s u) s (\x1 x2 -> u (x1, x2))

-- | Construct/destruct a 3-tuple.
tripleL :: TernL (a, b, c) a b c
tripleL = K7 (Tr leadin) (Tr leadout)
  where
    leadin k k' s t@(x1, x2, x3) = k (\s _ _ _ -> k' s t) s x1 x2 x3
    leadout k k' s u = k (\s _ -> k' s u) s (\x1 x2 x3 -> u (x1, x2, x3))

-- | Construct/destruct a 4-tuple.
quadrupleL :: QuaternL (a, b, c, d) a b c d
quadrupleL = K7 (Tr leadin) (Tr leadout)
  where
    leadin k k' s t@(x1, x2, x3, x4) = k (\s _ _ _ _ -> k' s t) s x1 x2 x3 x4
    leadout k k' s u = k (\s _ -> k' s u) s (\x1 x2 x3 x4 -> u (x1, x2, x3, x4))
