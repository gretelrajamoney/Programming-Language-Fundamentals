-- Name: Gretel Rajamoney
-- 2/4/2022
-- CS 381 Part 1 HW 4

-- definition of the abstract syntax and type definitions
type Prog = [Cmd]

data Cmd
    = LD Int -- loads its integer parameter onto the stack
    | ADD -- removes the two topmost integers from the stack and puts their sum onto the stack,
          -- if the stack contains fewer than two elements, ADD produces an error
    | MULT -- removes the two topmost integer from the stack and puts their product onto the stack,
           -- if the stack contains fewer than two elements, MULT produces an error
    | DUP -- places a copy of the stack's topmost element on the stack, if the stack is empty then
          -- DUP produces an error
    deriving Show

type Stack = [Int]


run :: Prog -> Stack -> Maybe Stack -- function that runs a program and a stack and returns a maybe stack
run [] x = Just x -- if there is no program, just output the original stack
run x s = runSub x (Just s) -- calls the runSub secondary function to run


semCmd :: Cmd -> Stack -> Maybe Stack -- function that runs a command and a stack and returns a maybe stack
semCmd (LD x) s = Just(x:s) -- adds the integer to the beginning of the stack 
semCmd (ADD) x = -- handles ADD command
    if length x <= 1 -- if there aren't atleast two numbers on the stack
        then Nothing -- return Nothing
    else addCmd x -- calls the addCmd secondary function
semCmd (MULT) x = -- handles MULT command
    if length x <= 1 -- if there aren't atleast two numbers on the stack
        then Nothing -- return Nothing
    else multCmd x -- calls the multCmd secondary function
semCmd (DUP) [] = Nothing -- if no values are in the stack return Nothing
semCmd (DUP) (x:xs) = Just([x,x] ++ xs) -- add a duplicate of the top value on the stack to the stack
otherwise = Just([]) -- else return Just([])


runSub :: Prog -> Maybe Stack -> Maybe Stack -- function that runs a program and a maybe stack and returns a maybe stack
runSub x Nothing = Nothing -- if nothing in the maybe stack then return nothing
runSub [] (Just x) = Just x -- if no programs then return just the stack
runSub (x:xs) (Just s) = runSub xs (semCmd x s) -- recursively call semCmd and runSub for all programs in the stack

addCmd :: Stack -> Maybe Stack -- secondary function that runs a stack and returns a maybe stack
addCmd (x:y:xs) = Just([x + y] ++ xs) -- adds the top two values in the stack and adds it to the stack

multCmd :: Stack -> Maybe Stack -- secondary function that runs a stack and returns a maybe stack 
multCmd (x:y:xs) = Just([x * y] ++ xs) -- multiplies the top two values in the stack and adds it to the stack


-- testing stack
stack1 :: Stack
stack1 = [1, 2, 3, 4, 5]

-- testing programs
test1 = [LD 3, DUP, ADD, DUP, MULT]
test2 = [LD 3, ADD]
test3 = []
test4 = [ADD, ADD, ADD, ADD]