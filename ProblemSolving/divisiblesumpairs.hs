
solve :: String -> String
solve input =
  let (nk : ar : _) = map ((map read) . words) $ lines input
      (_  : k  : _) = nk
  in  show $ solve' ar k 0

solve' :: [Int] -> Int -> Int -> Int
solve' [] _ acc = acc
solve' (x : xs) t acc =
  let f x = 0 == x `mod` t
      n = length $ filter (f . (+ x)) xs
  in  solve' xs t (acc + n)

main :: IO ()
main = interact solve
