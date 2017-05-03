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


sem :: Prog -> D
sem []       s 	= s
sem (x:xs)   s 	= case (semCmd x s) of
		  Nothing -> Nothing
		  s' -> sem xs s'

semCmd :: Cmd -> D
semCmd (LD i)       (Just x) = Just (i:x)
semCmd (DUP)     (Just (x:xs)) = Just (x:x:xs)
semCmd (DUP)         _  = Nothing
semCmd (ADD)   (Just (x:y:xs)) = Just ((x+y):xs)
semCmd (ADD)  	     _	= Nothing
semCmd (MULT)  (Just (x:y:xs)) = Just ((x*y):xs)
semCmd (MULT)        _  = Nothing

--Exercise 1-2: Extended 

sem2 :: Prog -> E
sem2 [] s = s
sem2 (x:xs) s = case (semCmd2 x s) of
		  Nothing -> Nothing
		  s' -> sem2 xs s'


--semCmd2 :: Cmd -> E


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
								   then sem2 (findIndex c (x,y)) (Just (x,y))
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
