
solve :: String -> String
solve input =
  let (_ : a : b : _) = map ((map read) . words) $ lines input
  in  show $ solve' a b

solve' :: [Int] -> [Int] -> Int
solve' a b = length [ x | x <- [f, (2 * f) .. l], (l `mod` x) == 0 ]
 where
  f = foldl1 lcm a
  l = foldl1 gcd b

main :: IO ()
main = interact solve
