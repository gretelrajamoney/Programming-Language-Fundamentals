-- Name: Gretel Rajamoney, Kaavya Subramanian, & Hannah Maung
-- Assignment: Homework 5 - Types
-- Course: CS 381 - 001 - W2022


-- definition of the abstract syntax and type definition
type Prog = [Cmd]
type Stack = [Val]
type Rank = Int
type CmdRank = (Int,Int)
data Ret
  = A [Int]
  | RankError
  | TypeError
  deriving Show

data Cmd
  = LDI Int -- loads its integer parameter onto the stack
    | LDB Bool -- loads its boolean parameter onto the stack
    | LEQ -- removes the top integer from the stack, removes the second integer
          -- from the stack, if top <= second the True is pushed on the stack
          -- else False is pushed onto the stack
    | ADD -- same as with stack language 2, only adds integers
    | MULT -- same as with stack language 2, only multiplies integers
    | DUP -- places a copy of the stack's topmost element on the stack, if the stack
          -- is empty then DUP produces an error, works with both integer and boolean values
    | IFELSE Prog Prog -- if the value on top of the stack is true, then run the first
                       -- program, else run the second program
    | DEC -- decrements the topmost element on the stack, it must be an int
    | SWAP -- exchanges the two topmost elements on the stack
    | POP Int -- pops k elements off the stack
    deriving Show

data Val = I Int | B Bool -- dynamically type checks all operations during run time
  deriving Show -- specifies that we want the compiler to automatically generate instances


checkInt :: Val -> Bool -- secondary function that takes in an either bool int and returns a bool
checkInt (B _) = False -- if it is a boolean type then return false
checkInt (I _) = True -- if it is an integer type then return true

checkTF :: Val -> Bool -- secondary function that takes a value and returns a boolean
checkTF (B x) = x -- returns the boolean only from the value

convertStack :: [Int] -> Stack -- secondary function that handles the run function
convertStack l = [(I i) | i <- l] -- changes program from an int to a stack

convertIntList :: Maybe Stack -> [Int] -- secondary function that handles the run function
convertIntList (Nothing) = [] -- if the maybe stack contains Nothing, then return an empty integer list
convertIntList (Just s) = [i | (I i) <- s] -- if the maybe stack is not empty, then turning it back into an integer list

addValInt :: Val -> Val -> Val -- secondary function that handles the ADD command
addValInt (I i) (I j) = I (i + j) -- takes the top two numbers and adds them and returns the integer

compareValInt :: Val -> Val -> Bool -- secondary function that handles the LEQ command
compareValInt (I i) (I j) = -- takes the top two integer values from the stack
  if i <= j then True -- if the top value is less than or equal to the second value then return True
  else False -- if is not less than or equal to, then return False

multValInt :: Val -> Val -> Val -- secondary function that handles the MULT command
multValInt (I i) (I j) = I (i * j) -- takes the top two numbers and multiplies them and returns the integer

valSwap :: Stack -> Maybe Stack -- secondary function that handles the SWAP command
valSwap (x:y:s) = Just (y:x:s) -- takes the stack and switched the top two numbers and returns a maybe stack

remove :: Stack -> Int -> Maybe Stack -- secondary function that handles the POP command
remove [] _ = Just [] -- if the stack is empty, return an empty maybe stack
remove l 0 = Just l -- if you are popping zero elements, return original stack as a maybe stack
remove (_:ys) n = remove ys (n - 1) -- remove the amount of numbers you are popping from the stack and return it as a maybe stack

semCmd :: Cmd -> Stack -> Maybe Stack -- function that takes a command and a stack and returns a maybe stack
semCmd (LDI n) s =  Just ((I n):s) -- loads an integer  onto the stack
semCmd (LDB n) s = Just ((B n):s) -- loads a boolean onto the stack
semCmd (ADD) s = -- handles the ADD command
  if length s < 2 then Nothing -- if the stack does not have two or more values then return Nothing
  else if checkInt(head s ) == False || checkInt (head (tail s)) == False then Nothing -- ensures the values are integers
  else Just ((addValInt (head s) (head (tail s))):(tail (tail s))) -- pushes the added value onto the top of the stack using addValInt
semCmd (MULT) s = -- handles the MULT command
  if length s < 2 then Nothing -- if the stack does not have two or more values then return Nothing
  else if checkInt(head s ) == False || checkInt (head (tail s)) == False then Nothing -- ensures the values are integers
  else Just ((multValInt (head s) (head (tail s))):(tail (tail s))) -- pushes the multiplied value onto the top of the stack using multValInt
semCmd (LEQ) s = -- handles the LEQ command
  if length s < 2 then Nothing -- if the stack does not have two or more values then return Nothing
  else if checkInt(head s ) == False || checkInt (head (tail s)) == False then Nothing -- ensures the values are integers
  else if compareValInt (head s) (head (tail s)) == True then Just ((B True):(tail (tail s))) -- uses the secondary function compareValInt
  else Just ((B False):(tail (tail s))) -- if x is greater than or equal to y then push a True onto the stack, else push a False
semCmd (DUP) [] = Nothing -- if the stack is empty then return Nothing
semCmd (DUP) s = Just ((head s):s) -- duplicate the top value on the stack and add it to the stack
semCmd (IFELSE p1 p2) s = -- handles the IFELSE function
  if checkInt(head s) == True then Nothing -- if the top value is in an integer then return Nothing
  else if checkTF(head s) == True then runIFELSE p1 (Just (tail s)) -- if checkTF is True then run program 1 using runIFELSE
  else runIFELSE p2 (Just (tail s)) -- if checkTF is False then run program 2
semCmd DEC [] = Nothing -- if the stack is empty then return Nothing
semCmd DEC ((B x):s) = Nothing -- if the first value on the stack is a boolean, then return Nothing
semCmd DEC ((I x):s) = Just ((I (x - 1)):s) -- if the first value on the stack is an integer then subtract it by one
semCmd SWAP s = -- handles the SWAP command
  if length s < 2 then Nothing -- if the stack does not have two or more values then return Nothing
  else valSwap s -- calls the valSwap secondary function to handle the stack
semCmd (POP k) s = -- handles the POP command
  if length s < k then Nothing -- if the length of the stack is smaller than the pop number, then return Nothing
  else if length s == k then Just [] -- if the length of the stack is equal to the pop number, then return an empty maybe stack
  else remove s k -- calls the remove secondary function to handles the stack

rankC :: Cmd -> CmdRank -- function that maps each stack operation to its rank
rankC ADD = (2,1) -- since ADD removes two values from the stack and places one on
rankC MULT = (2,1) -- since MULT removes two values from the stack and places one on
rankC DEC = (1,1) -- since DEC removes one value from the stack and places one on
rankC SWAP = (2,2) -- since SWAP removes two values from the stack and places two on
rankC (POP i) = (i,0) -- since POP removes i values from the stack and places zero on
rankC DUP = (1,2) -- since DUP removes one value from the stack and places two on
rankC LEQ = (2,1) -- since LEQ removes two values from the stack and places one on
rankC (LDI i) = (0,1) -- since LDI removes zero values from the stack and places one on
rankC (LDB b) = (0,1) -- since LDB removes zero values from the stack and places one on
rankC (IFELSE p1 p2) = (-1,-1) -- since we dont know which branch will be executed

rankIFELSE :: Prog -> Prog -> Maybe Rank -> Maybe Rank -- handles IFELSE to figure out which branch to execute
rankIFELSE p1 p2 Nothing = Nothing -- if the maybe rank is Nothing, then return Nothing
rankIFELSE p1 p2 (Just r) = -- if the maybe rank is not Nothing
  if checkNothing(rankP p1 (r - 1)) == True then Nothing -- if checkNothing returns True for p1, then return Nothing
  else if checkNothing(rankP p2 (r - 1)) == True then Nothing -- if checkNothing returns True for p2, then return Nothing
  else if rank_r p1 (Just (r - 1)) < rank_r p2 (Just (r - 1)) then rank_r p1 (Just (r - 1)) -- if p1 is less than p2, then use p1
  else rank_r p2 (Just (r - 1)) -- if p1 is not less than p2, then use p2 for the outputted maybe rank

rankTemp :: CmdRank -> Maybe Rank -> Maybe Rank -- secondary function that is called within rank_r
rankTemp (x,y) (Just r) = -- handles the input values when rankTemp is called
  if r < x then Nothing -- if the maybe rank is less than the command rank, then Nothing
  else Just (r - x + y) -- calculates the rank by subtracting r from x added to y

rank_r :: Prog -> Maybe Rank -> Maybe Rank -- secondary function that is called within rankP
rank_r [] (Just r) = Just r -- if the program is empty, then return the maybe rank only
rank_r p Nothing = Nothing -- if the maybe rank is Nothing, then return Nothing
rank_r ((IFELSE p1 p2):ps) (Just r)= rank_r ps (rankIFELSE p1 p2 (Just r)) -- handles if there is IFELSE in program
rank_r (p:ps) (Just r) = rank_r ps (rankTemp (rankC p) (Just r)) -- returns the rank of the program using rankTemp

rankP :: Prog -> Rank -> Maybe Rank -- takes in a program and a rank and return a maybe rank
rankP [] r = Just r -- if the program is empty, then return only the rank as a maybe rank
rankP p r  = rank_r p (Just r) -- if the program is not empty, compute the rank of a program using rank_r

run_r :: Prog -> Maybe Stack -> Maybe Stack -- secondary function that is called from the run function
run_r p Nothing = Nothing -- if the maybe stack is Nothing, then just return Nothing
run_r [] (Just s) = Just s -- if the program is empty, then return only the maybe stack
run_r (p:ps) (Just s) = run_r ps (semCmd p s ) -- if the program stack is not empty, then call semCmd for all programs

runIFELSE :: Prog -> Maybe Stack -> Maybe Stack -- secondary function that is called within semCmd to handles the IFELSE command
runIFELSE [] s = s -- if the program is empty, then return only the maybe stack
runIFELSE p s = run_r p s -- if the program is not empty, then call the run_r secondary function

checkNothing :: Maybe a -> Bool -- secondary function called within run that return a boolean
checkNothing (Just s) = False -- if it is of a maybe type then return false
checkNothing (Nothing) = True -- if it is Nothing then return True

run :: Prog -> [Int] -> Ret -- main function that takes in a program and an integer list and returns a return data type
run p s =
  if semStatTC p (length s) == 0 then RankError -- if the semStatTC returns 0 then return a RankError
  else if checkNothing (run_r p (Just (convertStack s))) == True then TypeError -- if checkNothing returns True then return a TypeError
  else A (convertIntList (run_r p (Just (convertStack s)))) -- if neither a RankError nor a TypeError then return an A

semStatTC :: Prog -> Rank -> Int -- secondary function that checks whether the stack program is rank correct
semStatTC p r =
  if rankP p r == Nothing then 0 -- if it is not rank correct then return a 0
  else 1 -- if it is rank correct then return a 1
