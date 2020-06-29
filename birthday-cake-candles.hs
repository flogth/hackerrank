import Data.List

solve :: String -> String
solve
  = show .
      length .
        head .
          group .
            sortBy (flip compare) .
              map (read :: String -> Int) . words . last . lines

main :: IO ()
main = interact solve
