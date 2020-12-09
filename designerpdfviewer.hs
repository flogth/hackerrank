import           Data.Char                      ( ord )

solve :: [Int] -> String -> Int
solve h w = (length w) * (maximum $ [ h !! i | i <- map index w ])
  where index = (+ (-97)) . ord

main :: IO ()
main = do
  h <- (map read . words) <$> getLine
  w <- getLine
  putStrLn $ show $ solve h w
