{-# LANGUAGE BlockArguments #-}

module Text.Cassette.Internal.Tr where

import Control.Category (Category(..))
import Prelude hiding (flip, id, (.))

--- | The type of string transformers in CPS, /i.e./ functions from strings to
--- strings.
type C r = (String -> r) -> String -> r

-- | @'Tr' r r'@ is the type of string transformers with answer type
-- modification from @r@ to @r'@ through control effects.
newtype Tr r r' = Tr { unTr :: C r -> C r' }

instance Category Tr where
  id = Tr id
  Tr f . Tr g = Tr (f . g)

-- | '(<>)' is the choice operator. Note that this is an unrestricted
-- backtracking operator: it never commits to any particular choice.
instance Semigroup (Tr r r') where
  Tr f <> Tr g = Tr \k k' s -> f k (\_ -> g k k' s) s

-- | 'mempty' is the string transformer that always fails.
instance Monoid (Tr r r') where
  mempty = Tr \_ k' s -> k' s

-- | Capture continuation up to the closest 'reset'.
shift :: (C r -> Tr w r') -> Tr r r'
shift f = Tr \k -> unTr (f k) id

-- | Inverse of 'shift'.
plug :: C r -> Tr r r' -> Tr w r'
plug k (Tr f) = Tr \_ -> f k

-- | Replace the success continuation.
replace :: C r -> Tr w r
replace k = plug k id

pushNeg :: a -> Tr (a -> r) r
pushNeg x = shift \k -> replace \k' s -> k (\s _ -> k' s) s x

popNeg :: Tr r (a -> r)
popNeg = shift \k -> replace \k' s x -> k (\s -> k' s x) s

pushPos :: a -> Tr (r -> r') ((a -> r) -> r')
pushPos x = shift \k -> replace \k' s u -> k (\s _ -> k' s u) s (u x)

popPos :: Tr ((a -> r) -> r') (r -> r')
popPos = shift \k -> replace \k' s u -> k (\s _ -> k' s u) s (\_ -> u)
