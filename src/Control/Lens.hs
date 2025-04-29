{-# LANGUAGE RankNTypes #-}

-- | Minimal re-implementation of just enough of lens, in order to avoid the
-- dependency. This is for INTERNAL USE ONLY. In downstream code, use lens or
-- any optics library.
module Control.Lens
  ( -- * Lenses
    Lens
  , view
    -- * Isomorphisms
  , Iso
  , iso
  , from
    -- * Prisms
  , Prism
  , preview
  , review
  ) where

import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Profunctor (Profunctor(..))
import Data.Profunctor.Unsafe ((#.))
import Data.Profunctor.Choice (Choice(..))

type Lens s t a b = forall f. (Functor f) => (a -> f b) -> s -> f t

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

view :: Lens s t a b -> s -> a
view l s = getConst (l Const s)

data Market a b s t = Market (b -> t) (s -> Either t a)

instance Profunctor (Market a b) where
  dimap f g (Market bt seta) = Market (g . bt) (first g . seta . f)

instance Choice (Market a b) where
  right' (Market bt seta) = Market bt' seta'
    where
      bt' b           = Right (bt b)
      seta' (Left c)  = Left (Left c)
      seta' (Right s) = either (Left . Right) Right (seta s)

withPrism
  :: Prism s t a b
  -> ((b -> t) -> (s -> Either t a) -> r)
  -> r
withPrism k f = case coerce (k (Market Identity Right)) of
  Market bt seta -> f bt seta

review :: Prism s t a b -> b -> t
review p = withPrism p $ \build _ -> build

preview :: Prism s t a b -> s -> Maybe a
preview p = withPrism p $ \_ match -> either (const Nothing) Just . match

data Exchange a b s t = Exchange (s -> a) (b -> t)

instance Profunctor (Exchange a b) where
  dimap f g (Exchange sa bt) = Exchange (sa . f) (g . bt)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

withIso :: Iso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso l k = case l (Exchange id Identity) of
  Exchange sa bt -> k sa (runIdentity #. bt)

from :: Iso s t a b -> Iso b a t s
from l = withIso l $ \sa bt -> iso bt sa
