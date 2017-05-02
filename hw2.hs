--Names: Jacob Geddings
module Hw2 where

--Exercise 1. A Stack Language

--Consider the stack language S defined by the following grammar. 

--S ::=C|C;S
--C ::=LDInt |ADD|MULT|DUP

{-An S-program essentially consists of a (non-empty) sequence of commands/operations C. 
The meaning of an S- program is to start with an empty stack and to perform its first operation on it, 
which results in a new stack to which the second operation in S (if it exists) is applied, and so on. 
The stack that results from the application of the last command is the result of the program.
The operation LD loads its integer parameter onto the stack. 
The operation ADD removes the two topmost integers from the stack and puts their sum onto the stack. 
If the stack contains fewer than two elements, ADD produces an error. 
Similarly, the operation MULT takes the two topmost integers from the stack and puts their product on top of the stack. 
It also produces an error if the stack contains fewer than two elements. 
Finally, the operation DUP places a second copy of the stack’s topmost element on the stack. 
(You can find out the error condition for DUP yourself.) Here is a definition of the abstract syntax that you should use. -}
{-
type Prog = [Cmd]
data Cmd = LD Int
           | ADD
           | MULT 
           | DUP

--Integer stacks should be represented by the type [Int], that is, lists of integers, that is, your program should contain and use the following definition.

type Stack = [Int]
-}
{-Define the semantics for the stack language as a Haskell function sem that yields the semantics of a program. 
Please note that the semantic domain has to be defined as a function domain 
(since the meaning of a stack program is a transformation of stacks) and as an error domain 
(since operations can fail). Therefore, sem has to have the following type where you have to find an appropriate type defintition for D.-}

--sem :: Prog -> D

--To define sem you probably want to define an auxiliary function semCmd for the semantics of individual operations,
--which has the following type.

--semCmd :: Cmd -> D

--Hint. Test your functions with the programs [LD 3,DUP,ADD,DUP,MULT] and [LD 3,ADD] and the empty stack [] as inputs.


type Prog		= [Cmd]
type Stack		= [Int]
type D			= Stack -> Maybe Stack		--Maybe is inserted due to requirement for handling errors

data Cmd		= LD Int
			| ADD
			| MULT
			| DUP
			deriving Show

sem :: Prog -> D
sem	[]	s	= Just s
sem (x:xs)	s	= case (semCmd x s) of
			  Nothing -> Nothing
			  Just s' -> sem xs s'

semCmd :: Cmd -> D
semCmd (LD i)	x 	= Just (i:x)
semCmd (DUP)  (x:xs)	= Just (x:x:xs)
semCmd (DUP)	_	= Nothing
semCmd (ADD)  (x:y:xs)	= Just ((x + y):xs)
semCmd (ADD)	_	= Nothing
semCmd (MULT) (x:y:xs)	= Just ((x * y):xs)
semCmd (MULT)	_	= Nothing

--Tests
test0 :: Prog
test0 = [LD 3, DUP, ADD, DUP, MULT]		--Just [36]

--Exercise 2. Extending the Stack Language by Macros

{-Suppose we want to add a simple macro facility to the stack language that allows us to define parameterless macros like SQR = DUP; MULT. 
The definition C would change as follows. -}

--C ::=LDInt |ADD|MULT|DUP|DEFString(S)|CALLString

{-The operation DEF n (C1; . . . ;Ck) defines a macro named n that is available in the rest of the program and, 
when called, causes the execution of C1;...;Ck. CALL n calls the macro named n if it has been defined earlier in the program. 
If n is not a defined macro, CALL n yields an error. You can choose how to deal with nested macro definitions 
(allowing nested macro definitions might lead to a simpler semantics definition). -}

--(a) Extend the abstract syntax to represent macro definitions and calls, that is, give a correspondingly changed data definition for Cmd.

--(b) Define a new type State to represent the state for the new language. The state includes the macro definitions and the stack. 
--Please note that a macro definition can be represented by a pair whose first component is the macro name and 
--the second component is the sequence of commands. Multiple macro definitions can be stored in a list. 
--A type to represent macro definitions could thus be defined as follows.

type Macros = [(String,Prog)]

--(c) Define the semantics for the extended language as a function sem2. As in exercise 1, you probably want to define
--an auxiliary function semCmd2 for the semantics of individual operations.

--Exercise 3. Mini Logo

--Consider the simplified version of Mini Logo (without macros), defined by the following abstract syntax.
data Cmd2 = Pen Mode
           | MoveTo Int Int
           | Seq Cmd2 Cmd2

data Mode = Up | Down

--The semantics of a Mini Logo program is ultimately a set of drawn lines. 
--However, for the definition of the semantics a “drawing state” must be maintained that keeps track of the 
--current position of the pen and the pen’s status (Up or Down). This state should be represented by values of the following type.

type State = (Mode,Int,Int)

--The semantic domain representing a set of drawn lines is represented by the type Lines.

type Line  = (Int,Int,Int,Int)

type Lines = [Line]

--Define the semantics of Mini Logo by giving two function definitions. First, define a function semS that has the following type.
  
--semS :: Cmd2 -> State -> (State,Lines)

--This function defines for each Cmd how it modifies the current drawing state and what lines it produces. 
--After that define the semantic function sem' of the following type.

--sem' :: Cmd2 -> Lines

--The function sem' should call semS. The initial state is defined to have the pen up and the current drawing position at (0, 0).
--Note. To test your semantics you can use the function ppLines defined in the Haskell file provided on the class web site. 
--This function converts a list of lines into an svg file that can be rendered by most browsers into a picture.
