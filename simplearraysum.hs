-- solution for Simple Array Sum and A Very Big Sum

solve :: String -> String
solve = show . sum . map read . words . last . lines

main :: IO()
main = interact solve
