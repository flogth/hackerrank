import Data.Monoid

between :: (Ord a) => a -> a -> (a -> Bool)
between lower higher = getAll . foldMap (All .) [(>= lower), (<= higher)]

inOffsets :: (Integral a) => a -> [a] -> (a-> Bool) -> String
inOffsets start offsets f = show $ length $ filter (f . (+ start)) offsets

solve :: String -> String
solve input = let nums = map (map read) $ map words $ lines input
                  s = nums !! 0 !! 0
                  t = nums !! 0 !! 1
                  a = nums !! 1 !! 0
                  b = nums !! 1 !! 1
                  as = nums !! 3
                  bs = nums !! 4
                  f = between s t
                in unlines [inOffsets a as f , inOffsets b bs f]

main  :: IO()
main = interact solve
