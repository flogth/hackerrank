solve :: String -> String
solve input = unlines $ map line [1..l]
  where
    l = read input
    line n = replicate (l-n) ' ' ++ replicate n '#'

main :: IO()
main = interact solve
