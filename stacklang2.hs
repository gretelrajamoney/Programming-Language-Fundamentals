-- Name: Gretel Rajamoney
-- 2/4/2022
-- CS 381 Part 2 HW 4

-- definition of the abstract syntax and type definitions
type Prog = [Cmd]
type Stack = [Either Bool Int]

data Cmd
    = LDI Int -- loads its integer parameter onto the stack (replaced LD)
    | LDB Bool -- loads its boolean parameter onto the stack
    | LEQ -- removes the top integer from the stack, removes the second integer
          -- from the stack, if top <= second the True is pushed on the stack else
          -- False is pushed onto the stack
    | ADD -- same as with stack language 1, only adds integers
    | MULT -- same as with stack language 1, only multiplies integers
    | DUP -- places a copy of the stack's topmost element on the stack, if the stack
          -- is empty then DUP produces an error, works with both integer and boolean values
    | IFELSE Prog Prog -- if the value on top of the stack is true, then run the first
                       -- program, else run the second program
    deriving Show

run :: Prog -> Stack -> Maybe Stack -- function that runs a program and a stack and returns a maybe stack
run [] x = Just x -- if there is no program, just output the original stack
run x s = runSub x (Just s) -- calls the runSub secondary function to run


semCmd :: Cmd -> Stack -> Maybe Stack -- function that takes a command and a stack and returns a maybe stack
semCmd (LDI x) s = Just((Right x):s) -- loads an integer correctly onto the stack
semCmd (LDB x) s = Just((Left x):s) -- loads a boolean correctly onto the stack
semCmd (ADD) s = -- handles the ADD command
    if length s <= 1 then Nothing -- if the stack doesnt have two or more values then return Nothing
    else (add_R s) -- otherwise it calls the add_R secondary function
semCmd (MULT) s = -- handles the MULT command
    if length s <= 1 then Nothing -- if the stack doesnt have two or more values then return Nothing
    else (mult_R s) -- otherwise it calls the mult_R secondary function
semCmd (LEQ) s = -- handles the LEQ command
    if length s <= 1 then Nothing -- if the stack doesnt have two or more values then return Nothing
    else (leq_R s) -- otherwise it calls the leq_R secondary function
semCmd (DUP) [] = Nothing -- if the stack is empty then return Nothing
semCmd (DUP) (x:xs) = Just([x,x] ++ xs) -- duplicate the top value on the stack and add it to the stack
semCmd (IFELSE x1 x2) s = 
    if head s == Left True 
        then run x1 (tail s)
    else if head s == Left False 
        then run x2 (tail s)
    else Nothing


add_R :: Stack -> Maybe Stack -- secondary function that takes in a stack and returns a maybe stack
add_R (x:y:xs) =
    if checkInt x == False || checkInt y == False -- if the top two values are not integers
        then Nothing -- then return Nothing
    else Just((addCmd x y) :xs) -- else call addCmd and add it to the top of the stack

mult_R :: Stack -> Maybe Stack -- secondary function that takes in a stack and returns a maybe stack
mult_R (x:y:xs) =
    if checkInt x == False || checkInt y == False -- if the top two values are not integers
        then Nothing -- then return Nothing
    else Just((multCmd x y) :xs) -- else call multCmd and add it to the top of the stack

leq_R :: Stack -> Maybe Stack -- secondary function that takes in a stack and returns a maybe stack
leq_R (x:y:xs) =
    if checkInt x == False || checkInt y == False -- if the top two values are not integers
        then Nothing -- then return Nothing
    else if x <= y then Just((Left True) :xs) -- if x is greater than or equal to y then push a True onto the stack
    else Just((Left False) :xs) -- else then push a False onto the stack

addCmd :: Either Bool Int -> Either Bool Int -> Either Bool Int -- secondary function that takes in two either bool ints and returns one back
addCmd (Right x) (Right y) = Right(x + y) -- adds the top two values of the stack and adds it onto the top of the stack

multCmd :: Either Bool Int -> Either Bool Int -> Either Bool Int -- secondary function that takes in two either bool ints and returns one back
multCmd (Right x) (Right y) = Right(x * y) -- multiplies the top two values of the stack and adds it onto the top of the stack

checkInt :: Either Bool Int -> Bool -- secondary function that takes in an either bool int and returns a bool 
checkInt (Right _ ) = True  -- if it has a Right return True
checkInt (Left _ ) = False  -- if it has a Left return False

runSub :: Prog -> Maybe Stack -> Maybe Stack -- function that takes in a program and a maybe stack and returns a maybe stack
runSub x Nothing = Nothing -- if Nothing in the maybe stack then return Nothing
runSub [] (Just x) = Just x -- if no programs then return just the stack in a maybe format
runSub (x:xs) (Just s) = runSub xs (semCmd x s) -- recursively call semCmd until finished


-- testing stacks
stack1 :: Stack
stack1 = [Right 1, Right 3, Right 5, Right 7, Right 9]

stack2 :: Stack
stack2 = [Left True, Right 3]

-- testing programs
test1 = [LDI 3, DUP, ADD, DUP, MULT]
test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
test3 = [LEQ]
test4 = [ADD, ADD, MULT, DUP]
test5 = [LEQ, IFELSE [] [], LDI 9]
test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]