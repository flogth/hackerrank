
solve :: String -> String
solve input =
  let (x1 : v1 : x2 : v2 : _) = map read $ words input
  in  if (solve' x1 v1 x2 v2) then "YES" else "NO"

solve' :: Integer -> Integer -> Integer -> Integer -> Bool
solve' x1 v1 x2 v2 = p $ (fromInteger (x2 - x1)) / (fromInteger (v1 - v2))
 where
  isInt x = x == fromInteger (round x)
  p x = x >= 0 && isInt x && (x1 == x2 || v1 /= v2)

main :: IO ()
main = interact solve
