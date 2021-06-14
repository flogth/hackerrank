module Main where

main :: IO ()
main = interact solve
    where 
        solve = show . f . map read . words
        f (b:a:_) = ceiling $ (2 * a) / b