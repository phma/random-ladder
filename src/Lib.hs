module Lib
    ( someFunc
    , bitize
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

bitize :: Integer -> Integer -> String
bitize _ 0 = ""
bitize random 1 = if random > 0 then "3" else "2"
bitize random range =
  let crit = (range * 373 + 322) `div` 644
  in if random >= crit
	then '3' : bitize (random - crit) (range - crit)
	else '2' : bitize random crit
