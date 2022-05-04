-- Homework 3 template

module Sentence where

-- Grammar for the animal sentence language:
--
--   <sentence> ->  <noun> <verb> [<noun>]  
--               	|  <sentence> `and` <sentence>
--				 	
--   <noun> 	-> <adj> <noun> | <noun> `and` <noun>
--					| `cats` | `dogs` | `ducks` | `bunnies`

--   <verb>		->  `chase` | `cuddle` | `hug` | `scare`
--   <adj>		->	`silly` | `small` | `old` | `happy`

data Sentence
   = NVN Noun Verb Noun -- finish noun verb noun sentence
   | NV Noun Verb -- finish noun verb sentence
   | And Sentence Sentence -- finish sentence and sentence
   | End
  deriving (Eq,Show)

data Adj = Silly | Small | Old | Happy -- finish adjectives
  deriving (Eq,Show)

data Noun = -- finish
    NP Adj Noun  -- Noun phrase
    | NAnd Noun Noun  -- Finish noun and noun
    | Cats | Dogs | Ducks | Bunnies -- list of nouns
  deriving (Eq,Show)

data Verb = Chase | Cuddle | Hug | Scare -- finish
  deriving (Eq,Show)


-- | The sentence: cats cuddle ducks and dogs cuddle ducks
ex1 :: Sentence
ex1 = NVN Cats Hug Dogs

ex2 :: Sentence
ex2 = NVN (NP Silly Cats) Hug Dogs

ex3 :: Sentence
ex3 = NVN (NAnd Dogs Cats) Chase Ducks

ex4 :: Sentence
ex4 = NVN (NAnd (NP Silly Dogs) Cats) Chase Ducks


-- | Build a sentence from a noun verb noun.
-- | buildS2 Cats Hug Cats
-- | NVN Cats Hug Cats

buildS2 :: Noun -> Verb ->Noun-> Sentence
buildS2 n1 v n2 = NVN n1 v n2 -- finish

-- | Build a sentence from a noun verb 
-- | buildS1 Cats Hug 
-- | NV Cats Hug 

buildS1 :: Noun -> Verb ->Sentence
buildS1 n v = NV n v -- finish


-- | Build a noun phrase from an adjective and noun
-- | buildNP Silly Dogs
-- | NP Silly Dogs

buildNP :: Adj -> Noun -> Noun
buildNP a n = NP a n -- finish

-- | Build a noun conjunction from two nouns
-- | buildNAnd Dogs Cats
-- | NAnd Dogs Cats

buildNAnd :: Noun -> Noun -> Noun
buildNAnd n1 n2 = NAnd n1 n2 -- finish

-- | Build a sentence that is a conjunction of a list of other sentences.
-- | conjunction [ex1, ex2]
-- | And (NVN Cats Hug Dogs) (NVN (NP Silly Cats) Hug Dogs)
--  
conjunction :: [Sentence] -> Sentence
conjunction []    = End -- if no conjunction sentence just end
conjunction [a,b] = (And a b) -- if only two, combine normally
conjunction (s : xs) = And s (conjunction xs) -- if multiple, interatively go through list
-- finish

-- | Pretty print a sentence.
pretty :: Sentence -> String
pretty (NVN s v o) = prettyNoun s ++ " " ++ prettyVerb v ++ " " ++ prettyNoun o
pretty (And l r)   = pretty l ++ " and " ++ pretty r
pretty (NV s v)     = prettyNoun s ++ " " ++ prettyVerb v
pretty (End) = "."

-- | Pretty print a noun.
prettyNoun :: Noun -> String 
prettyNoun Cats    = "cats"
prettyNoun Dogs    = "dogs"
prettyNoun Ducks   = "ducks"
prettyNoun Bunnies = "bunnies"
-- finish


prettyNoun (NP a n) = prettyAdj a ++ " " ++ prettyNoun n
prettyNoun (NAnd m n) = prettyNoun m ++ " and " ++prettyNoun n

-- | Pretty print a verb.
prettyVerb :: Verb -> String
prettyVerb Chase  = "chase" -- list all verbs
prettyVerb Cuddle = "cuddle"
prettyVerb Hug    = "hug"
prettyVerb Scare  = "scare"
-- finish

-- | Pretty print an adjective.
prettyAdj :: Adj -> String
prettyAdj Silly  = "silly" -- list all adjectives
prettyAdj Small  = "small"
prettyAdj Old    = "old"
prettyAdj Happy  = "happy"
-- finish


-- | Does the sentence contain only cuddling and hugs?
-- | isNice ex2
-- |   True
isNice :: Sentence -> Bool
isNice (NVN _ Chase _)  = False -- test all NVN's
isNice (NVN _ Cuddle _) = True
isNice (NVN _ Hug _)    = True
isNice (NVN _ Scare _)  = False
isNice (NV _ Chase)     = False -- test all NV's
isNice (NV _ Cuddle)    = True
isNice (NV _ Hug)       = True
isNice (NV _ Scare)     = False
isNice (And s1 s2)      = isNice s1 && isNice s2 -- iteratively test all And's
-- finish


-- |Count the number of words in a sentence
-- | wordCount ex4
--    6

nounCount :: Noun -> Int -- create temporary function nounCount to count all nouns
nounCount (NP a n) = nounCount n + 1 -- counts nouns in noun phrases
nounCount (NAnd n1 n2) = nounCount n1 + nounCount n2 + 1 -- counts nouns in NAnd's
nounCount Cats = 1 -- counts all singular nouns
nounCount Dogs = 1
nounCount Ducks = 1
nounCount Bunnies = 1


wordCount :: Sentence -> Int  
wordCount ( And l r ) = wordCount l + wordCount r  -- iteratively counts words in And's
wordCount ( NVN n1 v n2) = nounCount n1 + nounCount n2 + 1 -- uses nounCount to count nouns in NVN's
wordCount ( NV n v) = nounCount n + 1 -- uses nounCount to count nouns in NV's
-- finish