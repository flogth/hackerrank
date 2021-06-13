import           Data.List

differences :: Num a => [a] -> [a]
differences []           = []
differences (_     : []) = []
differences (x : y : xs) = (abs $ x - y) : differences (y : xs)

solve :: String -> String
solve = show . minimum . differences . sort . map read . words . last . lines

main :: IO ()
main = interact solve
