import Data.List

solve :: String -> String
solve input = show $ closestSum b  (-1) (sortBy (flip compare) keyboards) (sort drives)
  where ls = map ((map read) . words) $ lines input
        b = head $ ls !! 0
        keyboards = ls !! 1
        drives = ls !! 2

closestSum :: (Integral a) => a -> a -> [a] -> [a] -> a
closestSum _ acc [] _ = acc
closestSum _ acc _ [] = acc
closestSum n acc kbds drives
  = if k + d > n then closestSum n  acc (tail kbds) drives else
      if k + d > acc then closestSum n (k + d) kbds (tail drives) else
        closestSum n acc kbds (tail drives)
  where k = head kbds
        d = head drives

main :: IO ()
main = interact solve
