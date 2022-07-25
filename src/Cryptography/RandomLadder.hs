module Cryptography.RandomLadder
    ( someFunc
    , bitize
    , randomLadder
    ) where
import qualified Data.Sequence as Seq
import Data.Sequence ((><), (<|), (|>), Seq((:<|)), Seq((:|>)))

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

makeLadder :: Integer -> Integer -> Integer -> Seq.Seq Int
-- makeLadder n random range -> sequence-of-instructions
-- range should be at least n**0.78788506
-- random should be in [0..range)
-- n should be nonnegative
makeLadder (-1) _ _ = error "negative multiplier/exponent"
makeLadder 0 _ _ = Seq.empty
makeLadder 1 _ _ = Seq.singleton 21
makeLadder 2 _ _ = Seq.singleton 32
makeLadder n random range =
  let crit = (range * 373 + 322) `div` 644
      (quo2,rem2) = n `divMod` 2
      (quo3,rem3) = n `divMod` 3
  in if random >= crit
    then let third = makeLadder quo3 (random - crit) (range - crit)
    in
      if rem3 == 0
      then 30 <| third
      else
	if rem3 == 1
	then 31 <| third
	else 32 <| third
    else let half = makeLadder quo2 random crit
    in
      if rem2 == 0
      then 20 <| half
      else 21 <| half

climbLadder :: Seq.Seq Int -> a -> a -> (a -> a -> a) -> a -> a
climbLadder Seq.Empty _ _ _ acc = acc
climbLadder (is :|> 20) gen gen2 (<+>) acc =
  climbLadder is gen gen2 (<+>) (acc <+> acc)
climbLadder (is :|> 21) gen gen2 (<+>) acc =
  climbLadder is gen gen2 (<+>) ((acc <+> acc) <+> gen)
climbLadder (is :|> 30) gen gen2 (<+>) acc =
  climbLadder is gen gen2 (<+>) (acc <+> acc <+> acc)
climbLadder (is :|> 31) gen gen2 (<+>) acc =
  climbLadder is gen gen2 (<+>) ((acc <+> acc <+> acc) <+> gen)
climbLadder (is :|> 32) gen gen2 (<+>) acc =
  climbLadder is gen gen2 (<+>) ((acc <+> acc <+> acc) <+> gen2)

randomLadder :: a -> (a -> a -> a) -> a -> Integer -> Integer -> Integer -> a
-- randomLadder gen (<+>) zero n random range -> n*gen
-- range should be at least n**0.78788506
-- random should be in [0..range)
-- n should be nonnegative
randomLadder gen (<+>) zero n random range =
  climbLadder (makeLadder n random range) gen (gen <+> gen) (<+>) zero
