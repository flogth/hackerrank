import Data.List
import Data.List.Split
import Data.Char
import Text.Printf (printf)

solve :: String -> String
solve input = intercalate ":" [hours, minutes, seconds]
  where
    h:m:s:_ = splitOn ":" input
    ampm = filter isAlpha s
    add12 = read h /= 12 && ampm == "PM" || read h == 12 && ampm == "AM"
    hours = if add12 then printf "%02d" $ (12 + read h ::Int) `mod` 24 else h
    minutes = m
    seconds = filter isDigit s

main :: IO ()
main = interact solve
