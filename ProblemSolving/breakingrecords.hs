
solve :: String -> String
solve input =
  let (_ : i : _) = lines input
      (ma, mi)    = solve' $ map read $ words $ i
  in  show ma ++ " " ++ show mi


solve' :: [Int] -> (Int, Int)
solve' l = f l (head l, head l) (0, 0)
 where
  f :: [Int] -> (Int, Int) -> (Int, Int) -> (Int, Int)
  f [] _ c = c
  f (x : xs) (ma, mi) (mac, mic) =
    f xs ((max ma x), (min mi x)) ((incif (> ma) x) mac, (incif (< mi) x) mic)


  incif f v = if f v then (+ 1) else id

main :: IO ()
main = interact solve
