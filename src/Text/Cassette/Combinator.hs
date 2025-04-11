{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

module Text.Cassette.Combinator where

import Control.Category ((.))
import Prelude hiding ((.))
import Text.Cassette.Lead
import Text.Cassette.Prim

-- | Applies each cassette in the supplied list in order, until one of them
-- succeeds.
choice :: [PP a] -> PP a
choice [] = empty
choice (p:ps) = p <|> choice ps

-- | @count n p@ matches @n@ occurrences of @p@.
count :: Int -> PP a -> PP [a]
count 0 _ = nilL
count n p = consL --> p . count (n - 1) p

-- | Tries to apply the given cassette. It returns the value of the cassette on
-- success, the first argument otherwise.
option :: a -> PP a -> PP a
option x p = p <|> shift x nothing

-- | Tries to apply the given cassette. It returns a value of the form @Just x@
-- on success, @Nothing@ otherwise.
optionMaybe :: PP a -> PP (Maybe a)
optionMaybe p = justL --> p <|> nothingL

-- | Tries to match the given cassette and discards the result, otherwise does
-- nothing in case of failure.
optional :: PP a -> PP0
optional p = unshift [] (count 1 p <|> count 0 p)

-- | Apply the given cassette zero or more times.
many :: PP a -> PP [a]
many p = many1 p <|> nilL

-- | Apply the given cassette one or more times.
many1 :: PP a -> PP [a]
many1 p = consL --> p . many p

-- | Apply the given cassette zero or more times, discarding the result.
skipMany :: PP a -> PP0
skipMany p = unshift [] $ many p

-- | Apply the given cassette one or more times, discarding the result.
skipMany1 :: PP a -> PP0
skipMany1 p = unshift [] $ many1 p

-- | Apply the first argument zero or more times, separated by the second
-- argument.
sepBy :: PP a -> PP0 -> PP [a]
sepBy px psep = sepBy1 px psep <|> nilL

-- | Apply the first argument one or more times, separated by the second
-- argument.
sepBy1 :: PP a -> PP0 -> PP [a]
sepBy1 px psep = consL --> px . many (psep . px)

-- | @chainl p op x@ matches zero or more occurrences of @p@, separated by @op@.
-- Returns a value obtained by a /left associative/ application of all functions
-- returned by @op@ to the values returned by @p@. If there are zero occurrences
-- of @p@, the value @x@ is returned.
chainl :: PP0 -> BinL a a a -> PP a -> a -> PP a
chainl opP opL xP dflt = chainl1 opP opL xP <|> shift dflt nothing

-- | Match a a left-associative chain of infix operators.
chainl1 :: PP0 -> BinL a a a -> PP a -> PP a
chainl1 opP opL xP = catanal opL --> xP . many (opP . xP)

-- | @chainr p op x@ matches zero or more occurrences of @p@, separated by @op@.
-- Returns a value obtained by a /right associative/ application of all
-- functions returned by @op@ to the values returned by @p@. If there are zero
-- occurrences of @p@, the value @x@ is returned.
chainr :: PP0 -> BinL a a a -> PP a -> a -> PP a
chainr opP opL xP dflt = chainr1 opP opL xP <|> shift dflt nothing

-- | Match a a right-associative chain of infix operators.
chainr1 :: PP0 -> BinL a a a -> PP a -> PP a
chainr1 opP opL xP = catanar opL --> xP . many (opP . xP)

-- | @notFollowedBy p@ only succeeds when @p@ fails. This combinator does not
-- consume/produce any input.
notFollowedBy :: PP0 -> PP0
notFollowedBy p = unshift () $ shift () (p . empty) <|> shift () nothing

-- | Applies first argument zero or more times until second argument succeeds.
manyTill :: PP a -> PP0 -> PP [a]
manyTill xP endP = nilL --> endP <|> consL --> xP . manyTill xP endP
