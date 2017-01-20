import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

data Stats = Stats {
  solved :: Int,
  total :: Int,
  maxDepth :: Int
} deriving (Show)

choices = [1..5]
codeSize = 4

main =
  let startingGuesses = uniqBy categorize allCodes
      stats = map computeStats startingGuesses
      zipped = zip (map categorize startingGuesses) stats
      toString (category, stats) = show category ++ ": " ++ statsToString stats
  in mapM_ putStrLn $ map toString zipped

computeStats :: [Int] -> Stats
computeStats startingGuess =
  List.foldl' (foldCode startingGuess) (Stats 0 0 0) allCodes

foldCode :: [Int] -> Stats -> [Int] -> Stats
foldCode startingGuess stats code =
  foldGuess code 1 allCodes stats startingGuess

foldGuess :: [Int] -> Int -> [[Int]] -> Stats -> [Int]-> Stats
foldGuess code depth possibilities stats guess =
  -- trace ("code: " ++ show code ++ ", depth: " ++ show depth ++
  --        ", possibilities: " ++ show possibilities ++
  --        ", guess: " ++ show guess ++ ", stats: " ++ show stats) $
  let score = computeScore guess code
  in if fst score == codeSize
       then -- trace "solved" $
            solvedIt depth stats
       else -- trace ("score: " ++ show score) $
            let remainingPossibilities =
                  filter ((== score) . computeScore guess)
                  $ possibilities
            in List.foldl'
                 (foldGuess code (depth + 1) remainingPossibilities)
                 stats
                 remainingPossibilities

-- Given a guess and a code, return a pair containing the numnber of
-- exact matches (red), and the number of "right color wrong place"
-- (white).  The result is the same if guess and code are swapped.
--
computeScore :: [Int] -> [Int] -> (Int, Int)
computeScore guess code =
  let mismatched = filter (\ (c, g) -> c /= g) $ zip code guess
      red = codeSize - length mismatched
      -- Using the pattern match + list comprrehension is way faster
      -- than map fst/snd.
      (c2, g2) = unzip mismatched
      white = countWhite g2 c2
  in (red, white)

countWhite :: [Int] -> [Int] -> Int
countWhite [] _ = 0
countWhite (g:rest) code =
  case remove g code of
    Nothing -> countWhite rest code
    Just newCode -> 1 + countWhite rest newCode

-- remove first occurrence of the element from the list.  If the
-- element was not found returns Nothing, otherwise Just list.
--
remove :: Eq a => a -> [a] -> Maybe [a]
remove _ [] = Nothing
remove element (first:rest) =
  if element == first
    then Just rest
    else fmap (first:) $ remove element rest

allCodes :: [[Int]]
allCodes = makeCodes choices codeSize

makeCodes :: [a] -> Int -> [[a]]
makeCodes _ 0 = [[]]
makeCodes choices size =
  let codes = makeCodes choices (size - 1)
  in concat $ map (\ choice -> map (choice:) codes) choices

-- Given a code, compute the frequencies of the unique digits in the
-- code and sort into reverse numerical order.  E.g., a code with all
-- unique digits maps to [1,1,1,1], a code with two digits the same
-- maps to [2,1,1], etc.
--
categorize :: [Int] -> [Int]
categorize code =
  reverse $ List.sort $ Map.elems $ count code

solvedIt :: Int -> Stats -> Stats
solvedIt depth (Stats !solved !total !maxDepth) =
  Stats (solved + 1) (total + depth) (max depth maxDepth)

statsToString :: Stats -> String
statsToString (Stats solved total maxDepth) =
  let average = fromIntegral total / fromIntegral solved
  in "Solved: " ++ show solved ++ ", average: " ++ show average ++
    ", maxDepth: " ++ show maxDepth

-- Takes a list of items, and returns a map where the keys are the
-- items and the values are the number of times the item occurs in the
-- list.
--
count :: Ord a => [a] -> Map a Int
count list =
  List.foldl' (\ map digit -> Map.insertWith (+) digit 1 map)
    Map.empty list

-- Takes a list of items and a function that maps an item to a key,
-- and returns a list of unique items based on the key.
--
uniqBy :: Ord k => (a -> k) -> [a] -> [a]
uniqBy func list =
  Map.elems $ List.foldl' (\ map e -> Map.insert (func e) e map) Map.empty list
