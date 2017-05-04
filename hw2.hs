module Hw2 where

--Exercise 1:	A Stack Language


type Prog 	= [Cmd]
data Cmd	= LD Int
		| ADD
		| MULT
		| DUP
		| DEF String Prog
		| CALL String
		deriving Show

type Stack	= Maybe [Int]
type D		= Stack -> Stack

type Macros	= [(String, Prog)]
type State	= (Stack, Macros)
type E		= Maybe State -> Maybe State


doExist :: String -> State -> Bool
doExist s t = s `elem` (map fst (snd t))

findIndex :: String -> State -> Prog
findIndex s (t,c:cs) 
				 | fst c == s = snd c
				 | otherwise  = findIndex s (t,cs)


sem :: Prog -> Stack
sem [] = Nothing
sem xs = sEm' xs (Just [])

sEm' :: Prog -> D
sEm' [] (Just s) = Just s
sEm' (x:xs) (Just s) = sEm' xs (semCmd x (Just s))
sEm' xs Nothing = Nothing   

semCmd :: Cmd -> D
semCmd (LD i)       (Just x) = Just (i:x)
semCmd (DUP)     (Just (x:xs)) = Just (x:x:xs)
semCmd (DUP)         _  = Nothing
semCmd (ADD)   (Just (x:y:xs)) = Just ((x+y):xs)
semCmd (ADD)  	     _	= Nothing
semCmd (MULT)  (Just (x:y:xs)) = Just ((x*y):xs)
semCmd (MULT)        _  = Nothing

--Exercise 1-2: Extended 

--a) See above abstract syntax for Cmd for the extended syntax. 

--b) Defined above: type State	= (Stack, Macros)

--c)

sem2 :: Prog -> Maybe State
sem2 [] = Nothing
sem2 xs = sem2' xs (Just (Just [],[]))

sem2' :: Prog -> E
sem2' [] (Just s) = Just s
sem2' (x:xs) (Just s) = sem2' xs (semCmd2 x (Just s))
sem2' xs Nothing = Nothing


semCmd2 :: Cmd -> E
semCmd2 (LD i) (Just (x,y))	
				| semCmd (LD i) x == Nothing = Nothing
				| otherwise		     = Just ((semCmd (LD i) x), y)
semCmd2 ADD (Just (x,y))
				| semCmd ADD x == Nothing = Nothing
				| otherwise		  = Just ((semCmd ADD x), y)
semCmd2 MULT (Just (x,y))
				| semCmd MULT x == Nothing = Nothing
				| otherwise		  = Just ((semCmd MULT x), y)
semCmd2 DUP (Just (x,y))
				| semCmd DUP x == Nothing = Nothing
				| otherwise		  = Just ((semCmd DUP x), y)
semCmd2 (DEF c p) (Just (x, y)) = if not (doExist c (x,y))
								   then Just (x,((c,p):y))
								   else Nothing
semCmd2 (CALL c)  (Just (x, y)) = if doExist c (x,y)
								   then sem2' (findIndex c (x,y)) (Just (x,y))
					 			   else Nothing

--sandbox
p :: Prog
p = [LD 2, DUP]

test0 :: Prog
test0 = [LD 3, DUP, ADD, DUP, MULT]

test1 :: Prog
test1 = [LD 3, ADD]

test2 :: Prog
test2 = []

test3 :: Prog
test3 = [LD 3, DEF "dadm" [DUP,ADD,DUP,MULT], DEF "dam" [DUP,ADD,DUP], CALL "dadm", CALL "dam"]

u = [LD 3, DEF "dadm" [DUP,ADD,DUP,MULT], CALL "dadm"]
u' = [LD 3, DEF "dadm" [DUP,ADD,DUP,MULT], DEF "dam" [DUP,ADD,DUP], CALL "dadm", CALL "dam"]

--Exercise 3 Mini Logo

data Cmd2 = Pen Mode 
		  | MoveTo Int Int
		  | Seq Cmd2 Cmd2
		  deriving Show

data Mode = Up | Down deriving Show

type State2 = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)

type Lines = [Line] 

semS :: Cmd2 -> State2 -> (State2, Lines)
semS (Pen Up) (_,x,y) = ((Up,x,y), [])
semS (Pen Down) (_,x,y) = ((Down,x,y), [])
semS (MoveTo m s) (Up,x,y) = ((Up,m,s),[])
semS (MoveTo m s) (Down,x,y) = ((Down,m,s), [(x,y,m,s)])
semS (Seq cmd1 cmd2) s = (fst s2, snd s1 ++ snd s2) 
						where
							s1 = semS cmd1 s
							s2 = semS cmd2 (fst s1)

--initial state:
initstate = (Up, 0, 0)

sem' :: Cmd2 -> Lines
sem' s = snd (semS s initstate)

moveTest1 = Pen Down `Seq` MoveTo 1 1
semTest1 = sem' moveTest1  --[(0,0,1,1)]
moveTest2 = Pen Down `Seq` MoveTo 2 4 `Seq` MoveTo 3 5
semTest2 = sem' moveTest2 --[(0,0,2,4),(2,4,3,5)]