import           Data.List

partitions :: Int -> [a] -> [[a]]
partitions _ [] = []
partitions n l
  | (length l) == n
  = [l]
  | otherwise
  = let (x, xs) = splitAt n l in x : (partitions n $ (tail x) ++ xs)

solve :: String -> String
solve input =
  let (_ : s : md : _) = map ((map read) . words) $ lines input
      (d     : m  : _) = md
  in  show $ length $ filter ((== d) . sum) $ partitions m s

main :: IO ()
main = interact solve
