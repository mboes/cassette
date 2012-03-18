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
chainr1 opP opL xP = catanar opL --> xP <> many (opP <> xP)

notFollowedBy :: a -> PP a -> PP0
notFollowedBy x xP = unshift x $ xP <> empty <|> shift x nothing

manyTill :: PP a -> PP0 -> PP [a]
manyTill xP endP = nilL --> endP <|> consL --> xP <> manyTill xP endP
