import Data.List as List
import Data.Map as Map
import Debug.Trace

data Stats = Stats {
  solved :: Int,
  total :: Int,
  maxDepth :: Int
} deriving (Show)

choices = [1, 2, 3, 4, 5, 6]
numChoices = 4

main =
  let startingGuesses = uniqBy categorize allCodes
      stats = List.map computeStats startingGuesses
      zipped = zip (List.map categorize startingGuesses) stats
      toString (category, stats) = show category ++ ": " ++ statsToString stats
  in mapM_ putStrLn $ List.map toString zipped

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
  in if fst score == numChoices
       then -- trace "solved" $
            solvedIt depth stats
       else -- trace ("score: " ++ show score) $
            let remainingPossibilities =
                  List.filter ((== score) . computeScore guess)
                  $ possibilities
            in List.foldl'
                 (foldGuess code (depth + 1) remainingPossibilities)
                 stats
                 remainingPossibilities

computeScore :: [Int] -> [Int] -> (Int, Int)
computeScore guess code =
  let mismatched = List.filter (\ (c, g) -> c /= g) $ zip code guess
      red = numChoices - length mismatched
      c2 = [c | (c, _) <- mismatched]
      g2 = [g | (_, g) <- mismatched]
      (white, _) =
        List.foldl' (\ accum@(count, remaining) g ->
          -- XXX we don't have to traverse the remaining list twice
          if g `elem` remaining
            then (count + 1, List.delete g remaining)
            else accum)
          (0, c2) g2
  in (red, white)

allCodes :: [[Int]]
allCodes = makeCodes choices numChoices

makeCodes :: [Int] -> Int -> [[Int]]
makeCodes _ 0 = [[]]
makeCodes choices number =
  let codes = makeCodes choices (number - 1)
  in concat $ List.map (\ choice -> List.map (\ x -> choice:x) codes) choices

-- Given a code, compute the frequencies of the unique digits in the
-- code and sort into reverse numerical order.  E.g., a code with all
-- unique digits maps to [1,1,1,1], a code with two digits the same
-- maps to [2,1,1], etc.
--
categorize :: [Int] -> [Int]
categorize code =
  reverse $ List.sort $ Map.elems $ count code

solvedIt :: Int -> Stats -> Stats
solvedIt depth (Stats solved total maxDepth) =
  Stats (solved + 1) (total + depth) (max depth maxDepth)

statsToString :: Stats -> String
statsToString (Stats solved total maxDepth) =
  let average = fromIntegral total / fromIntegral solved
  in "Solved: " ++ show solved ++ ", average: " ++ show average ++
    ", maxDepth: " ++ show maxDepth

count :: Ord a => [a] -> Map a Int
count list =
  List.foldl' (\ map digit -> Map.insertWith (+) digit 1 map)
    Map.empty list

groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy func list =
  List.foldl' (\ map e -> Map.insertWith (++) (func e) [e] map)
    Map.empty list

uniqBy :: Ord k => (a -> k) -> [a] -> [a]
uniqBy func list =
  Map.elems
    $ List.foldl' (\ map e -> Map.insert (func e) e map) Map.empty list
