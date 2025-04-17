{- |
Module: Data.Monoid.Action.Bytes

Byte-twiddling utilities.
-}

module Data.Monoid.Action.Bytes (
    -- * Functions.
    byteCount,
    byte,
    bytes,
    pack,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits, Bits ((.&.), shiftR, shiftL, (.|.)))
import Data.Word (Word8)

-- Package.
import Data.Monoid.Action.Bits (bitCount)
import Data.Foldable (foldl')


{- | Return the number of bytes in the integral type.

The actual argument is ignored by the function and only the type matters. It is implicitely assumed
that the number of bits is a multiple of @8@.
-}
{-# INLINE byteCount #-}
byteCount :: FiniteBits w => w -> Word
byteCount n = bitCount n `quot` 8

{- | Return the ith byte of the integral number.

Result is undefined if @i@ is larger than the 'byteCount' of the type.
-}
{-# INLINE byte #-}
byte :: (Integral w, Bits w) => Word -> w -> Word8
byte i n = fromIntegral $ shiftR (shiftL 0xff j .&. n) j
    where
        j = 8 * fromIntegral i

{- | Return the list of bytes from lowest to highest significance. -}
{-# INLINEABLE bytes #-}
bytes :: (Integral w, FiniteBits w) => w -> [Word8]
bytes w = go 0
    where
        go :: Word -> [Word8]
        go !n =
            if n == byteCount w
                then []
                else byte n w : go (succ n)

{- | Pack a list of bytes into an integral value, the inverse of 'bytes'.

note(s):

  * Argument list is truncated to a list of 'byteCount' length.
-}
{-# INLINEABLE pack #-}
pack :: forall w . (Integral w, FiniteBits w) => [Word8] -> w
pack = foldl' (.|.) 0 . fmap shift . zip [0 .. pred $ byteCount @w 0] . fmap fromIntegral
    where
        shift :: (Word, w) -> w
        shift (m, n) = shiftL n ( 8 * fromIntegral m)
