-- Gretel Rajamoney
-- CS 381
-- 1/19/2022


import HW2types -- imports the HW2types.hs file


-- Question 1 Part A
-- function that inserts an element into a multiset
ins :: Eq a => a -> Bag a -> Bag a -- function declaration called "ins"
ins x [] = [(x, 1)] -- if multiset is empty, create new bag in it
ins x ((y, n): multiset) | x == y = (y, n + 1): multiset -- if multiset with this value exists, increase count by one
                         | otherwise = (y, n): ins x multiset -- if multiset with this value does not exist, create a new one


-- Question 1 Part B
-- function that removes a single element from a multiset
del :: Eq a => a -> Bag a -> Bag a -- function declaration called "del"
del x [] = [] -- if multiset is empty, return an empty multiset back
del x ((y, n): multiset)  | n > 1 && x == y  = (y, n - 1): multiset -- if more than one of the value exists in multiset, decrease it by one
                          | x == y           = multiset -- if only one of the value exists in the multiset
                          | otherwise        = (y, n): del x multiset -- delete that entire bag in the multiset


-- Question 1 Part C
-- function that takes a list of values and produces a multiset representation
xs = [7,3,8,7,3,2,7,5] -- sample multiset from assignment
bag :: Eq a => [a] -> Bag a -- function declaration called "bag"
bag [] = [] -- if multiset is empty, return an empty multiset back
bag (x: xs) = ins x (bag xs) -- if multiset is not empty, use ins function to insert it into correct representation


-- Question 1 Part D
-- function that determines whether or not its first argument bag is contained 
-- in the second
subbag :: Eq a => Bag a -> Bag a -> Bool -- function declaration called "subbag"
subbag ((ax, ay): a) ((bx, by): b) = null [(x, y) | (x, y) <- ((ax, ay): a), -- loads first argument bag
                                     null [(w, z) | (w, z) <- ((bx, by): b), -- loads second multiset bag
                                     y <= by && x == bx]] -- returns true if the value is contained in both


-- Question 1 Part E
-- function that tests whether a bag is actually a set, which is the case when 
-- each element occurs only once
isSet :: Eq a => Bag a -> Bool -- function declaration called "isSet"
isSet ((a, b): n) = null [(x, y) | (x, y) <- ((a, b): n), -- load multiset into function
                    y > 1] -- if more than one value exists, return False


-- Question 1 Part F
-- function that computes the number of elements contained in a bag
size :: Bag a -> Int -- function declaration called "size"
size = sum . map snd -- takes the sum of the second element of the whole multiset


-- Question 2 Part A
-- function that computes the list of nodes contained in a given graph
g :: Graph -- defines a graph called g
g = [(1,2),(1,3),(2,3),(2,4),(3,4)] -- sample graph from assignment
h :: Graph -- defines a graph called h
h = [(1,2),(1,3),(2,1),(3,2),(4,4)] -- sample graph from assignment
nodes :: Graph -> [Node] -- function declaration called "nodes"
nodes [] = [] -- if graph set is empty, return empty set back
nodes (x:y) = norm (nodes y++ [fst x, snd x]) -- uses norm function to find all nodes in both elements in order


-- Question 2 Part B
-- function that computer the list of successors for a node in a given graph
suc :: Node -> Graph -> [Node] -- function declaration called "suc"
suc x [] = [] -- if graph set is empty, return empty set back
suc x ((a, b): graphset) | x == a     = [b]++ suc x graphset -- if value is the first element of the node, return the next greatest node
                         | otherwise  = suc x graphset -- if value is the last node return [], if value is the first element of last node return second element


-- Question 2 Part C
-- function that removes a node together with all of its incident edges from a graph
detach :: Node -> Graph -> Graph -- function declaration called "detach"
detach x [] = [] -- if graph set is empty, return empty set back
detach x ((a, b): graphset) | x == a     = detach x graphset -- if value is found in first element of graphset, remove entire node
                            | x == b     = detach x graphset  -- if value is found in second element of graphset, remove entire node
                            | otherwise  = (a, b) : detach x graphset -- return all nodes without value in graphset


-- Question 2 Part D
-- function that creates a cycle of any given number
cyc_temp :: Int -> Graph -- function declaration called "cyc_temp"
cyc_temp 1 = [] -- if value equals one, return empty graphset
cyc_temp x = (cyc_temp (x - 1) ++ [(x - 1, x)]) -- generates circle nodes by increasing nodes and decreasing count
cyc x = cyc_temp x ++ [(x, 1)] -- uses variable "cyc" to save generated nodes from temp and add last node with (value, 1)


-- Question 3 Part A
-- function that computes the width of a shape
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2] -- example figure set provided in assignment
width :: Shape -> Length -- function declaration called "width"
width (Pt x)                      = 0 -- makes width of point zero 
width (Circle x radius)           = 2 * (radius) -- makes width double the radius
width (Rect x length_1 length_2)  = length_1 -- makes width the first provided length


-- Question 3 Part B
-- function that computes the bounding box of a shape
bbox :: Shape -> BBox -- function declaration called "bbox"
bbox (Pt x)                     = ((x), (x)) -- point is just point
-- for circle first box is, (first element minus radius, second element minus radius)
-- for circle second box is, (first element plus radius, second element plus radius)
bbox (Circle x radius)          = ((fst x - radius, snd x - radius), (fst x + radius, snd x + radius))
-- for rectangles first box is, (point, point)
-- for rectangles second box is, (first element plus first length, second element plus second length)
bbox (Rect x length_1 length_2) = ((x), (fst x + length_1, snd x + length_2))


-- Question 3 Part C
-- function that computes the minimum x coordinate of a shape
minX :: Shape -> Number -- function declaration called "minX"
minX (Pt x)                     = snd x -- second element of the point
minX (Rect x length_1 length_2) = snd x -- second element of the point
minX (Circle x radius)          = snd x - radius -- second element of the point minus the circle radius


-- Question 3 Part D
-- function that moves the position of a shape by a vector given by a point 
-- as its second argument
move :: Shape -> Point -> Shape -- function declaration called "move"
move (Pt x) vector                     = (Pt (addPt x vector)) -- adds point with vector point using addPt function
-- adds rectangle point with vector point using addPt function, returns both lengths after new point
move (Rect x length_1 length_2) vector = (Rect (addPt x vector) length_1 length_2)
-- adds cirle point with vector point using addPt function, returns radius after new point
move (Circle x radius) vector          = (Circle (addPt x vector) radius)
addPt :: Point -> Point -> Point -- function declaration called "addPt"
addPt (a, b) (c, d) = (a + c, b + d) -- adds both first elements and both second elements