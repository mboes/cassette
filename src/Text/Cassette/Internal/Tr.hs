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

-- | Capture continuation up to the closest 'reset'.
shift :: (C r -> Tr w r') -> Tr r r'
shift f = Tr (\k -> unTr (f k) id)

-- | Delimit what is captured by 'shift'.
reset :: Tr r r' -> C r'
reset (Tr f) = f id

-- | Inverse of 'shift'.
plug :: C r -> Tr r r' -> Tr w r'
plug k (Tr f) = Tr (\_ -> f k)

-- | Replace the success continuation.
replace :: C r -> Tr w r
replace k = plug k id

push :: a -> Tr (a -> r) r
push x = shift (\k -> replace (\k' s -> k (\s _ -> k' s) s x))

pop :: Tr r (a -> r)
pop = shift (\k -> replace (\k' s x -> k (\s -> k' s x) s))
