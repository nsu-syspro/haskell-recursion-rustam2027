{-# OPTIONS_GHC -Wall #-}
-- Note: the above pragma enables all warnings

module Task3 where

-----------------------
-- Helper type synonyms

type Peg = String
type Move = (Peg, Peg)

-----------------------

-- Usage examples
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a

