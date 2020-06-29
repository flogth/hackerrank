import Data.List

solve :: String -> String
solve input = unlines $ map (show .(/ (fromIntegral . length $ l)) . fromIntegral . length) [pos, neg, zero]
  where
    l = map read $ words $ last $ lines input
    (pos, b) = partition (>0) l
    (neg, zero) = partition (<0) b

main :: IO()
main = interact solve
