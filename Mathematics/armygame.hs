module Main where

main :: IO ()
main = interact solve
    where 
        solve = show . f . map read . words
        f (n:m:_) = (n + n `mod` 2) * (m + m `mod` 2) `div` 4    