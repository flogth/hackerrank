import           Data.List

getDiagonal :: [[a]] -> [a]
getDiagonal ([[x]]) = x : []
getDiagonal matrix =
  (head $ head matrix) : (getDiagonal $ tail $ map tail matrix)

solve :: String -> String
solve input = show $ abs (up - down)
 where
  matrix = map (map (read :: String -> Int)) $ map words $ tail $ lines input
  down   = sum $ getDiagonal matrix
  up     = sum $ getDiagonal $ transpose $ map reverse matrix

main :: IO ()
main = interact solve
