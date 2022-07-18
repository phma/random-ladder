module Cryptography.RandomLadder
    ( someFunc
    , bitize
    , randomLadder
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

randomLadder :: a -> (a -> a -> a) -> a -> Integer -> Integer -> Integer -> a
-- randomLadder gen (<+>) zero n random range -> n*gen
-- range should be at least n**0.78788506
-- random should be in [0..range)
-- n should be nonnegative
randomLadder _ _ _ (-1) _ _ = error "negative multiplier/exponent"
randomLadder _ _ zero 0 _ _ = zero
randomLadder gen _ _ 1 _ _ = gen
randomLadder gen (<+>) _ 2 _ _ = gen <+> gen
randomLadder gen (<+>) zero n random range =
  let crit = (range * 373 + 322) `div` 644
      (quo2,rem2) = n `divMod` 2
      (quo3,rem3) = n `divMod` 3
  in if random >= crit
    then let third = randomLadder gen (<+>) zero quo3 (random - crit) (range - crit)
    in
      if rem3 == 0
      then third <+> third <+> third
      else
	if rem3 == 1
	then (third <+> third <+> third) <+> gen
	else (third <+> third <+> third) <+> (gen <+> gen)
    else let half = randomLadder gen (<+>) zero quo2 random crit
    in
      if rem2 == 0
      then half <+> half
      else (half <+> half) <+> gen
