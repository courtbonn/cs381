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

--Exercise 2
data Shape = X
	| TD Shape Shape
	| LR Shape Shape
	deriving Show

type BBox = (Int, Int)

--Part a)
bbox :: Shape -> BBox
bbox 	      X = (1, 1)
bbox   (TD x y) = let (a,b) = bbox x
		      (c,d) = bbox y
		  in (max a c, b+d)
bbox   (LR x y) = let (a,b) = bbox x
		      (c,d) = bbox y
		  in (a+c, max b d)
--Part b)
rect :: Shape -> Maybe BBox
rect X = Just (1,1)
rect (TD x y) = case rect x of
		Nothing -> Nothing
		Just (a,b) -> case rect y of
			      Nothing -> Nothing
			      Just (c,d) -> if a == c then Just (a, b+d)
					    else Nothing
rect (LR x y) = case rect x of
		Nothing -> Nothing
		Just (a,b) -> case rect y of
			      Nothing -> Nothing
			      Just (c,d) -> if b == d then Just (a+c, b)
					    else Nothing
