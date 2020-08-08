import           Data.List
import           Control.Monad
import           Data.Bifunctor

solve :: String -> String
solve =
  (\(a, b) -> (show a) ++ " " ++ (show b))
    . solve'
    . map ((map (read :: String -> Int)) . words)
    . lines

solve' :: [[Int]] -> (Int, Int)
solve' (as : bs : _) =
  join bimap length $ partition (> 0) $ filter (/= 0) $ zipWith (-) as bs



main :: IO ()
main = interact solve
