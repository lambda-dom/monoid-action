module Tests.Monoid.Action.BytesSpec (
    -- * Tests.
    spec,
) where

 
-- Imports.
-- Testing library.
import Test.Hspec (Spec, describe, it, shouldBe)

-- Module to test.
import Data.Monoid.Action.Bytes (byteCount, bytes, pack)

-- Base.
import Data.Word (Word8, Word16, Word32, Word64)


-- Main module test driver.
spec :: Spec
spec = describe "Mono.Types.IntegralBytes tests" $ do
    spec_byteCount
    spec_bytes
    spec_pack


-- Tests.
spec_byteCount :: Spec
spec_byteCount = describe "byteCount tests" $ do
    it "Case Word8" $ do
         byteCount @Word8 0 `shouldBe` 1

    it "Case Word16" $ do
         byteCount @Word16 0 `shouldBe` 2

    it "Case Word32" $ do
         byteCount @Word32 0 `shouldBe` 4

    it "Case Word64" $ do
         byteCount @Word64 0 `shouldBe` 8

spec_bytes :: Spec
spec_bytes = describe "bytes tests" $ do
    it "Success cases" $ do
        bytes (0xff :: Word32) `shouldBe` [0xff, 0, 0, 0]
        bytes (0xff00 :: Word32) `shouldBe` [0, 0xff, 0, 0]
        bytes (0xff0000 :: Word32) `shouldBe` [0, 0, 0xff,  0]
        bytes (0xff000000 :: Word32) `shouldBe` [0, 0, 0, 0xff]

spec_pack :: Spec
spec_pack = describe "pack tests" $ do
    let h = pack :: [Word8] -> Word32
    it "Success cases" $ do
        h [0xff, 0, 0, 0] `shouldBe` 0xff
        h [0, 0xff, 0, 0] `shouldBe` 0xff00
        h [0, 0, 0xff,  0] `shouldBe` 0xff0000
        h [0, 0, 0, 0xff] `shouldBe` 0xff000000

    it "Success when argument list longer than byteCount" $ do
        h ([0xff, 0, 0, 0] ++ [0xff, 0xff]) `shouldBe` 0xff
        h ([0, 0xff, 0, 0] ++ [0xff, 0xff]) `shouldBe` 0xff00
        h ([0, 0, 0xff,  0] ++ [0xff, 0xff]) `shouldBe` 0xff0000
        h ([0, 0, 0, 0xff] ++ [0xff, 0xff]) `shouldBe` 0xff000000
