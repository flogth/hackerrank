module Main where

main :: IO ()
main = interact solve
    where solve = unlines . map ( show . (+1) . read) . drop 1 . lines