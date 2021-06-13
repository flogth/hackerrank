
data Winner = Balsa | Koca deriving (Show)

solve :: String -> String
solve =

game :: [Int] -> Winner
game n = Balsa

main :: IO()
main = interact solve
