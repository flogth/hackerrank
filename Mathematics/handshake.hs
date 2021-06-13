module Main where

main :: IO ()
main = interact solve
    where
        solve = unlines . map ( show . f . pred . read) . drop 1 . lines
        f n = (n*n + n) `div` 2