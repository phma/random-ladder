{-# LANGUAGE InstanceSigs #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Cryptography.RandomLadder

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

newtype BigRand = BigRand Integer deriving Show

instance Arbitrary BigRand where
  arbitrary :: Gen BigRand
  arbitrary = BigRand <$> choose (0, 2^128-1)

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "bignum" $
      \(BigRand a) (BigRand b) -> randomLadder 1 (+) (*2) 0 a b (2^128) == a
  , QC.testProperty "bignum'" $
      \(BigRand a) (BigRand b) -> randomLadder' 1 (+) 0 a b (2^128) == a
  ]

unitTests = testGroup "Unit tests"
  [ testCase "5882353" $
      (randomLadder' 1 (+) 0 5882353 8388608 16777216) @?= 5882353
  ]
