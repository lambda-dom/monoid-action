module Tests.Monoid.Action.BitsSpec (
    -- * Tests.
    spec,
) where

 
-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it, shouldBe)

-- Module to test.
import Data.Monoid.Action.Bits (bitCount, bits, pack)

-- Base.
import Data.Word (Word8, Word16, Word32, Word64)


-- Main module test driver.
spec :: Spec
spec = describe "Mono.Types.IntegralBytes tests" $ do
    spec_bitCount
    spec_bits
    spec_pack


-- Tests.
spec_bitCount :: Spec
spec_bitCount = describe "bitCount tests" $ do
    it "Case Word8" $ do
         bitCount @Word8 0 `shouldBe` 8

    it "Case Word16" $ do
         bitCount @Word16 0 `shouldBe` 16

    it "Case Word32" $ do
         bitCount @Word32 0 `shouldBe` 32

    it "Case Word64" $ do
         bitCount @Word64 0 `shouldBe` 64

spec_bits :: Spec
spec_bits = describe "bits tests" $ do
    let h = bits :: Word8 -> [Bool]
    it "Success cases" $ do
        h 0 `shouldBe` [False, False, False, False, False, False, False, False]
        h 1 `shouldBe` [True, False, False, False, False, False, False, False]
        h 8 `shouldBe` [False, False, False, True, False, False, False, False]
        h 0xff `shouldBe` [True, True, True, True, True, True, True, True]

spec_pack :: Spec
spec_pack = describe "bits tests" $ do
    let h = pack :: [Bool] -> Word8
    it "Success cases" $ do
        h [False, False, False, False, False, False, False, False] `shouldBe` 0
        h [True, False, False, False, False, False, False, False] `shouldBe` 1
        h [False, False, False, True, False, False, False, False] `shouldBe` 8
        h [True, True, True, True, True, True, True, True] `shouldBe` 0xff
