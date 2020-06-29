
solve :: String -> String
solve = unlines . map (show . roundGrade . read) . tail . lines

roundGrade :: Integral a => a -> a
roundGrade grade = if grade < 38 || m < 3 then grade else grade + (5 - m)
  where m = grade `mod` 5

main :: IO ()
main = interact solve
