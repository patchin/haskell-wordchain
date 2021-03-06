haskell-wordchain
=================

Solution to wordchain puzzle.

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
