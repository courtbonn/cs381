module Hw2 where

--Exercise 1:	A Stack Language

type Prog 	= [Cmd]
type Stack	= [Int]
type D		= Stack -> Maybe Stack

type State	= (Macros, Stack)
type Macros	= [(String, Prog)]
type E		= State -> Maybe State

data Cmd	= LD Int
		| ADD
		| MULT
		| DUP
		| DEF String Prog
		| CALL String
		deriving Show

sem :: Prog -> D
sem []       s 	= Just s
sem (x:xs)   s 	= case (semCmd x s) of
		  Nothing -> Nothing
		  Just s' -> sem xs s'

semCmd :: Cmd -> D
semCmd (LD i)       (x) = Just (i:x)
semCmd (DUP)     (x:xs) = Just (x:x:xs)
semCmd (DUP)         _  = Nothing
semCmd (ADD)   (x:y:xs) = Just ((x+y):xs)
semCmd (ADD)  	     _	= Nothing
semCmd (MULT)  (x:y:xs) = Just ((x*y):xs)
semCmd (MULT)        _  = Nothing

--Exercise 1-2: Extended 
{-
sem2 :: Prog -> E
sem2 []	      s	= Just s
sem2 (x:xs)   s	= case (semCmd2 x s) of
		  Nothing -> Nothing
		  Just s' -> sem2 xs s'


--semCmd2 :: Cmd -> E
-}

--sandbox
p :: Prog
p = [LD 2, DUP]

test0 :: Prog
test0 = [LD 3, DUP, ADD, DUP, MULT]

test1 :: Prog
test1 = [LD 3, ADD]

test2 :: Prog
test2 = []
