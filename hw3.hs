module Homework3 where

--Exercise 1
type Prog = [Cmd]

data Cmd = LD Int
	| ADD
	| MULT
	| DUP
	| INC
	| SWAP
	| POP Int

type Rank = Int
type CmdRank = (Int, Int)
type Stack = [Int]

--Part a)
rankC :: Cmd -> CmdRank
rankC (LD x) = (0, 1)
rankC (ADD) = (2, 1)
rankC (MULT) = (2, 1)
rankC (DUP) = (1, 2)
rankC (INC) = (1, 1)
rankC (SWAP) = (2, 2)
rankC (POP x) = (x, 0)

rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP x = rank x 0

rank :: Prog -> Rank -> Maybe Rank
rank [] r = Just r
rank (x:xs) r = if n-m > r
		then Nothing
		else rank xs ((m-n) + r)
		where (n, m) = rankC x

--Part b)
semStatTC :: Prog -> Maybe Stack
semStatTC [] = Just []
semStatTC p = case rankP p of
		Nothing -> Nothing
		Just r -> Just (sem p)

sem :: Prog -> Stack
sem = foldl (flip semCmd) []

semCmd :: Cmd -> Stack -> Stack
semCmd (LD x) s = (x:s)
semCmd (ADD) (x:y:xs) = (x+y):xs
semCmd (MULT) (x:y:xs) = (x*y):xs
semCmd (DUP) (x:xs) = x:x:xs
semCmd (INC) (x:xs) = x+1:xs
semCmd (SWAP) (x:y:xs) = y:x:xs
semCmd (POP x) s = drop x s
