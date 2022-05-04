-- Names: Gretel Rajamoney, Kaavya Subramanian, Hannah Maung
-- Assignment: Homework 5
-- Course: CS 381


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

-- dynamically type checks all operations during run time
data Val = I Int | B Bool
  deriving Show

checkInt:: Val -> Bool -- secondary function that takes in an either bool int and returns a bool
checkInt (B _) = False -- if it is a boolean type then return false
checkInt (I _) = True -- if it is an integer type then return true

checkTF :: Val -> Bool
checkTF (B x) = x

convertStack :: [Int] -> Stack
convertStack l = [(I i) | i <- l]

convertIntList :: Maybe Stack -> [Int]
convertIntList (Nothing) = []
convertIntList (Just s) = [i | (I i) <- s]

-- semCmd:: Cmd -> Stack -> Stack
-- semCmd ADD ((I x):(I y):s) = ((I (x + y)):s)

addValInt:: Val -> Val -> Val
addValInt (I i) (I j) = I (i + j)
--
compareValInt:: Val -> Val -> Bool
compareValInt (I i) (I j) =
  if i <= j then True
  else False

multValInt:: Val -> Val -> Val
multValInt (I i) (I j) = I (i * j)

valSwap :: Stack -> Maybe Stack
valSwap (x:y:s) = Just (y:x:s)

remove:: Stack -> Int -> Maybe Stack
remove [] _ = Just []
remove l 0 = Just l
remove (_:ys) n = remove ys (n - 1)


-- semCmd:: Cmd -> Stack -> Ret
-- semCmd (LDI n) s = A ((I n):s)

semCmd:: Cmd -> Stack -> Maybe Stack -- function that takes a command and a stack and returns a maybe stack
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
  if checkInt(head s) == True then Nothing
  else if checkTF(head s) == True then runIFELSE p1 (Just (tail s))
  else runIFELSE p2 (Just (tail s))
semCmd DEC [] = Nothing
semCmd DEC ((B x):s) = Nothing
semCmd DEC ((I x):s) = Just ((I (x-1)):s)
semCmd SWAP s =
  if length s < 2 then Nothing
  else valSwap s
semCmd (POP k) s =
  if length s < k then Nothing
  else if length s == k then Just []
  else remove s k

rankC:: Cmd -> CmdRank
rankC ADD = (2,1)
rankC MULT = (2,1)
rankC DEC = (1,1)
rankC SWAP = (2,2)
rankC (POP i) = (i,0)
rankC DUP = (1,2)
rankC LEQ = (2,1)
rankC (LDI i) = (0,1)
rankC (LDB b) = (0,1)
-- redirect rankC to rankTemp
rankC (IFELSE p1 p2) = (-1, -1)

rankIFELSE :: Prog -> Prog -> Maybe Rank -> Maybe Rank
rankIFElSE p1 p2 Nothing = Nothing
rankIFELSE p1 p2 (Just r) =
  if checkNothing(rankP p1 (r - 1)) == True then Nothing
  else if checkNothing(rankP p2 (r - 1)) == True then Nothing
  else if rank_r_IFELSE p1 (r -1) < rank_r_IFELSE p2 (r-1) then Just (rank_r_IFELSE p1 (r-1))
  else Just (rank_r_IFELSE p2 (r-1))

rankTemp :: CmdRank -> Maybe Rank -> Maybe Rank
rankTemp (x,y) (Just r) =
  if r < x then Nothing
  else Just (r - x + y)

rankTemp_IFELSE :: CmdRank -> Rank -> Rank
rankTemp_IFELSE (x,y) r = r - x + y

rank_r_IFELSE :: Prog -> Rank -> Rank
rank_r_IFELSE [] r = r
rank_r_IFELSE (p:ps) r = rank_r_IFELSE ps (rankTemp_IFELSE (rankC p) r)

rank_r :: Prog -> Maybe Rank -> Maybe Rank
rank_r [] (Just r) = Just r
rank_r p Nothing = Nothing
rank_r ((IFELSE p1 p2):ps) (Just r)= rank_r ps (rankIFELSE p1 p2 (Just r))
rank_r (p:ps) (Just r) = rank_r ps (rankTemp (rankC p) (Just r))

rankP :: Prog -> Rank -> Maybe Rank
rankP [] r = Just r
rankP p r  = rank_r p (Just r)

run_r::Prog -> Maybe Stack -> Maybe Stack
run_r p Nothing = Nothing
run_r [] (Just s) = Just s
run_r (p:ps) (Just s) = run_r ps (semCmd p s )

-- run_r::Prog -> Maybe Stack -> Ret
-- run_r p Nothing = TypeError
-- run_r [] (Just s) = A s
-- run_r (p:ps) (Just s) = run_r ps (semCmd p s )


runIFELSE::Prog -> Maybe Stack -> Maybe Stack
runIFELSE [] s = s
runIFELSE p s = run_r p s

-- run::Prog -> [Int] -> Maybe [Int]
-- run [] s = Just s
-- run p s = Just (convertIntList (run_r p (Just (convertStack s))))

checkNothing :: Maybe a -> Bool
checkNothing (Just s) = False
checkNothing (Nothing) = True

run :: Prog -> [Int] -> Ret
run p s =
  if semStatTC p (length s) == 0 then RankError
  else if checkNothing (run_r p (Just (convertStack s))) == True then TypeError
  else A (convertIntList (run_r p (Just (convertStack s))))



semStatTC :: Prog -> Rank -> Int
semStatTC p r =
  if rankP p r == Nothing then 0
  else 1
