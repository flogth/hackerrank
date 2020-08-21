import           Data.List

solve :: String -> String
solve input =
  let (_ : xs : _) = map ((map read) . words) $ lines input in show $ solve' xs

solve' :: [Int] -> Int
solve' = head . head . (sortBy cmp) . group . sort
  where cmp x y = if (length x) >= (length y) then LT else GT


main :: IO ()
main = interact solve
