module Main where

data V a = V a a
    deriving (Show)

instance Functor V where
    f `fmap` (V x y) = V (f x) (f y)

instance (Num a) => Semigroup (V a) where
    (V ax ay) <> (V bx by) = V (ax + bx) (ay + by)


solve :: String -> String
solve =  unlines . map (s . d . v . map read . words) . drop 1 . lines
    where 
        v (ax:ay:bx:by:_) = (V ax ay, V bx by)
        d (a,b) = b <> b <> (negate <$> a)
        s (V x y) = unwords $ show <$> [x,y]

main :: IO ()
main = interact solve