{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Text.Cassette.Combinator where

import Text.Cassette.Prim
import Text.Cassette.Leads


choice :: [PP a] -> PP a
choice [p] = p
choice (p:ps) = p <|> choice ps

count :: Int -> PP a -> PP [a]
count 0 _ = nilL
count n p = consL --> p <> count (n - 1) p

option :: a -> PP a -> PP a
option x p = p <|> shift x nothing

optionMaybe :: PP a -> PP (Maybe a)
optionMaybe p = justL --> p <|> nothingL

optional :: PP a -> PP0
optional p = unshift [] (count 1 p <|> count 0 p)

skipMany :: PP a -> PP0
skipMany p = unshift [] $ many p

skipMany1 :: PP a -> PP0
skipMany1 p = unshift [] $ many1 p

many :: PP a -> PP [a]
many p = many1 p <|> nilL

many1 :: PP a -> PP [a]
many1 p = consL --> p <> many p

sepBy :: PP a -> PP0 -> PP [a]
sepBy px psep = sepBy1 px psep <|> nilL

sepBy1 :: PP a -> PP0 -> PP [a]
sepBy1 px psep = consL --> px <> many (psep <> px)

chainl :: PP0 -> BinL a a a -> PP a -> a -> PP a
chainl opP opL xP dflt = chainl1 opP opL xP <|> shift dflt nothing

chainl1 :: PP0 -> BinL a a a -> PP a -> PP a
chainl1 opP opL xP = catanal opL --> xP <> many (opP <> xP)

chainr :: PP0 -> BinL a a a -> PP a -> a -> PP a
chainr opP opL xP dflt = chainr1 opP opL xP <|> shift dflt nothing

chainr1 :: PP0 -> BinL a a a -> PP a -> PP a
chainr1 opP opL xP = catanal opL --> xP <> many (opP <> xP)

-- notFollowedBy :: a -> PP a -> PP0
-- manyTill :: PP a -> PP end -> PP [a]

-- int :: PP Int
-- int = many1 digit --> K7 (\k k' s -> k (k' . show) s . read) (\k k' s -> k (k' . read) s . show)
--
-- pair = K7 (\k k' s x2 x1 -> k (const (k' x2 x1)) s (x1, x2))
--           (\k k' s t@(x1, x2) -> k (\_ _ -> k' t) s x2 x1)
--
-- triple = K7 (\k k' s x3 x2 x1 -> k (const (k' x3 x2 x1)) s (x1, x2, x3))
--             (\k k' s t@(x1, x2, x3) -> k (\_ _ _ -> k' t) s x3 x2 x1)
--
-- quadruple = K7 (\k k' s x4 x3 x2 x1 -> k (const (k' x4 x3 x2 x1)) s (x1, x2, x3, x4))
--                (\k k' s t@(x1, x2, x3, x4) -> k (\_ _ _ _ -> k' t) s x4 x3 x2 x1)
