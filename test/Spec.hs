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

qcProps = testGroup "(checked by QuickCheck)"
  [
  ]

unitTests = testGroup "Unit tests"
  [ testCase "5882353" $
      (randomLadder 1 (+) 0 5882353 8388608 16777216) @?= 5882353
  ]
