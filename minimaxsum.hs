import Data.List

solve :: String -> String
solve nums = unwords $ map show [mini, maxi]
  where
    nu = map read $ words nums
    maxi = sum $ tail $ sort nu
    mini = sum $ tail $ sortBy (flip compare) nu


main :: IO()
main = interact solve
