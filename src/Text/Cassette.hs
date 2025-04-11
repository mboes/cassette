-- | The combinators of this library are all pairs of functions going in
-- opposite directions. These pairs are called /cassettes/, sporting two
-- tracks (the two functions), one of which is read is one direction, the
-- other of which (accessed by flipping the cassette) is read in the opossite
-- direction.
--
-- = __Example__
--
-- Consider the data type for abstract syntax trees of the λ-calculus:
--
-- >>> :{
--   type Ident = String
--   data Term where
--     Var :: Ident -> Term
--     Abs :: Ident -> Term -> Term
--     App :: Term -> Term -> Term
--   deriving instance Show Term
-- :}
--
-- Given a few wrappers lifting constructors to cassette /leads/ (the
-- definitions are mechanical), ...
--
-- >>> :{
--     varL = K7 (Tr leadout) (Tr leadin) where
--        leadout k k' s x = k (\ s _ -> k' s x) s (Var x)
--        leadin k k' s t@(Var x) = k (\ s _ -> k' s t) s x
--        leadin k k' s t = k' s t
--     absL = K7 (Tr leadout) (Tr leadin) where
--        leadout k k' s t1 x = k (\ s _ -> k' s t1 x) s (Abs x t1)
--        leadin k k' s t@(Abs x t1) = k (\ s _ _ -> k' s t) s t1 x
--        leadin k k' s t = k' s t
--     appL = K7 (Tr leadout) (Tr leadin) where
--        leadout k k' s t2 t1 = k (\ s _ -> k' s t2 t1) s (App t1 t2)
--        leadin k k' s t@(App t1 t2) = k (\ s _ _ -> k' s t) s t2 t1
--        leadin k k' s t = k' s t
-- :}
--
-- ... the concrete syntax for terms of the λ-calculus can be defined
-- as follows:
--
-- >>> :{
-- let term :: PP Term
--     term =
--       varL --> ident <|>
--       absL --> char '^' . ident . char '.' . optSpace . term <|>
--       appL --> parens (term . sepSpace . term)
--     parens p = char '(' . p . char ')'
--     ident = consL --> letter . many alphaNum
-- :}
--
-- From this single specification, we can extract a parser, using
-- 'parse', and also a pretty printer, using 'pretty'.
--
-- >>> parse term "^x. (x x)"
-- Just (Abs "x" (App (Var "x") (Var "x")))
--
-- >>> pretty term (Abs "x" (App (Var "x") (Var "x")))
-- Just "^x. (x x)"
--
-- = Grammar specifications
--
-- Specifications are built from primitive and derived combinators, which
-- affect the input string in some way. For each constructor of each datatype,
-- we need to write a /lead/, which is a pair of a construction function and a
-- destruction function. Leads are pure combinators that do not affect the
-- input string. By convention, we suffix their name with "L".
--
-- Internally, the primitive combinators are written in CPS. Leads also need
-- to be written in this style, being primitive. They can, however, be
-- automatically generated for every datatype using some Template Haskell
-- hackery (in a separate package). A number of leads for standard datatypes
-- are defined in the 'Text.Cassette.Lead' module.

module Text.Cassette (module X) where

import Prelude hiding ((.))
import Text.Cassette.Char as X
import Text.Cassette.Combinator as X
import Text.Cassette.Lead as X
import Text.Cassette.Number as X
import Text.Cassette.Prim as X

-- $setup
-- >>> :seti -XStandaloneDeriving -XGADTSyntax
-- >>> import Control.Category
