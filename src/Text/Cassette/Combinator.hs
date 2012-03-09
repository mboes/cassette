{-# LANGUAGE RankNTypes #-}
module Text.Cassette.Combinator where

import Text.Cassette.Prim
import Text.Cassette.Char


-- choice :: [PP a] -> PP a
-- choice = foldr1 (<|>)
--
-- count :: Int -> PP a -> PP [a]
-- count 0 _ = knil
-- count n p = p <> count (n - 1) p --> kcons
--
-- option :: a -> PP a -> PP a
-- option def p = p <|> shift def id
--
-- optionMaybe :: PP a -> PP (Maybe a)
-- optionMaybe p = p <|> shift Nothing id
--
-- optional :: PP a -> PP0
-- optional p = unshift [] (count 1 p <|> count 0 p)
--
-- skipMany1 :: PP a -> PP0
-- skipMany1 = undefined

many1 :: PP a -> PP [a]
many1 p = p <> many p --> kcons

-- sepBy :: PP a -> PP sep -> PP [a]
-- sepBy1 :: PP a -> PP sep -> PP [a]
-- endBy :: PP a -> PP sep -> PP [a]
-- endBy1 :: PP a -> PP sep -> PP [a]
-- sepEndBy :: PP a -> PP sep -> PP [a]
-- sepEndBy1 :: PP a -> PP sep -> PP [a]
-- chainl :: PP a -> PP (a -> a -> a) -> a -> PP a
-- chainl1 :: PP a -> PP (a -> a -> a) -> PP a
-- chainr :: PP a -> PP (a -> a -> a) -> a -> PP a
-- chainr1 :: PP a -> PP (a -> a -> a) -> PP a
-- eof :: PP0
-- notFollowedBy :: a -> PP a -> PP0
-- manyTill :: PP a -> PP end -> PP [a]
-- lookAhead :: PP a -> PP a

int :: PP Int
int = many1 digit --> K7 (\k k' s -> k (k' . show) s . read) (\k k' s -> k (k' . read) s . show)

pair = K7 (\k k' s x2 x1 -> k (const (k' x2 x1)) s (x1, x2))
          (\k k' s t@(x1, x2) -> k (\_ _ -> k' t) s x2 x1)

triple = K7 (\k k' s x3 x2 x1 -> k (const (k' x3 x2 x1)) s (x1, x2, x3))
            (\k k' s t@(x1, x2, x3) -> k (\_ _ _ -> k' t) s x3 x2 x1)

quadruple = K7 (\k k' s x4 x3 x2 x1 -> k (const (k' x4 x3 x2 x1)) s (x1, x2, x3, x4))
               (\k k' s t@(x1, x2, x3, x4) -> k (\_ _ _ _ -> k' t) s x4 x3 x2 x1)
