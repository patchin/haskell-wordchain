{- 
   words.hs - Patrick Chin, Sept. 22, 2014

   Usage:
   
   Place a file called input.txt with the four-letter words in the same directory as the program. Then run the function "main" in ghci or
   run the compile program from the command line.
   The output of the program is a string of the form (x,[1,2..n]) where x is the length of the path and [1,2..n] is the list of words.
   The length is defined as the number of jumps/transforms to go from the start word to the end/target word.

   eg. (2,["BARE","BARK","DARK"])

   If a cycle is detected or no results are found then the result is (1000000, []). 1000000 is just a sentinel value.
   
   NOTE!!! - maxPathLength is a variable used to tune the depth that searching is performed. Stop searching along a path in the graph
   if the current length exceeds this value.

   Notes: 
  
   The program initially generates a list of tuples from the inputted word list. Each tuple is the word and the list of neighbouring words.
   To search for a path of word transformation we begin with the "start" word and traverse its neighbours and their neighbours in search of the "target" word.
   Essentially we are creating a graph where each word is a node and there is an edge between word-nodes if they differ in just one letter. A list
   of tranformations from the start word to the end word is a path in this graph.

   We could half processing time of creating the lookup table by creating two relations for each pair of words that
   are neighbours. In reality the creation of the table is pretty fast. It's the searching
   that's slow.

   We could accelerate search by storing more than just neighbours of dist=1 with each word. 
   Neighbours of dist=2, dist=3, etc.

   The fanout and search depth is killing search time. Each recursion call could be made faster if we assume 
   there aren't long paths. Setting a maxPathLength speeds up search. We could also start maxPathLength at a small number
   and increase it if there are no results found at a specified path length plus we should cache the search results so that
   subsequent searches don't have to repeat the calculation at a previous level.

   Another optimization idea is to perform the search from both ends and if the generated search paths share any words then there
   is a path from start to finish.

   Maybe there is some result from Gray Code theory that could help us optimize the search?
-}

-- import Data.HashTable
import Data.List
import Data.Ord
import Data.Char
import qualified Data.Map as Map

-- test
-- wordList = ["foil", "boil", "food", "bowl", "bool", "foot", "bawl", "fowl", "boot", "soot", "fown", "fawn"]

areNeighbours :: (Eq a) => [a] -> [a] -> Bool
areNeighbours a b = if length a /= length b
                 then False
                 else areNeighbours2 a b
areNeighbours2 a b = diffLetters a b == 1

diffLetters :: (Eq a) => [a] -> [a] -> Integer
diffLetters [] [] = 0
diffLetters (a:as) (b:bs) = 
	if a == b
        then 0 + diffLetters as bs
        else 1 + diffLetters as bs

getNeighbours word [] = []
getNeighbours word (x:xs) 
	| x == word = getNeighbours word xs
	| areNeighbours word x = x : getNeighbours word xs
 	| otherwise = getNeighbours word xs
	
createNode word list = (word, getNeighbours word list)

-- createTable [] wordList = []
-- createTable (x:xs) wordList = createNode x wordList : createTable xs wordList
createTable [] wordList oldMap = oldMap
createTable (x:xs) wordList oldMap = createTable xs wordList (Map.insert key value oldMap)  
	where node = createNode x wordList
      	      key = fst node
      	      value = snd node

-- getTableEntry key table = filter (\x -> fst x == key) table 
getTableEntry key table = Map.lookup key table

-- getEntry key = head (getTableEntry key hashTable)

maxPathLength = 5 

getNeighboursValue x =
	case x of 
  		(Just value) -> value
  		Nothing -> []

getDistance start finish currLength path hashTable 
			   | currLength > maxPathLength  = (1000000, []) -- limit long paths
			   | start == finish = (0, [start, finish])
			   | elem start path = (1000000, path ++ [start]) -- reached a cycle
			   | elem finish (getNeighboursValue startNeighbours) = (currLength + 1, path ++ [start, finish])
                           | startNeighbours == Nothing = (1000000, []) -- dead end, no neighbours
			   | otherwise = recurse 
			   where startNeighbours = getTableEntry start hashTable 
                                 recurse = minimumBy (comparing fst) (map (\x -> getDistance x finish (currLength + 1) (path ++ [start]) hashTable) (getNeighboursValue startNeighbours)) 

main = do 
       s <- readFile "input.txt" 
       let t = words s
       -- let hashTable = createTable t t
       let hashTable = createTable t t (Map.empty)
       putStrLn "Enter start:" 
       start <- getLine 
       putStrLn "Enter end:" 
       end <- getLine 
       let x = getDistance (map toUpper start) (map toUpper end) 0 [] hashTable
       print x
