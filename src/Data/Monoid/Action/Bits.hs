{- |
Module: Data.Monoid.Action.Bits

Bit-twiddling utilities.
-}

module Data.Monoid.Action.Bits (
    -- * Bit functions.
    bitCount,
    bit,
    bits,
    pack,
) where

-- Imports.
-- Base.
import Data.Bits (FiniteBits (finiteBitSize), Bits ((.|.), testBit, zeroBits))
import qualified Data.Bits as Bits (bit)
import Data.Foldable (foldl')


{- | Return the number of bits in the integral type.

The actual argument is ignored by the function and only the type matters.
-}
{-# INLINE bitCount #-}
bitCount :: FiniteBits w => w -> Word
bitCount = fromIntegral . finiteBitSize

{- | Return the ith bit of the integral number.

Result is undefined if @i@ is larger than the 'bitCount' of the type.
-}
{-# INLINE bit #-}
bit :: Bits w => Word -> w -> Bool
bit i = flip testBit (fromIntegral i)

{- | Return the list of bits from lowest to highest significance. -}
{-# INLINEABLE bits #-}
bits :: FiniteBits w => w -> [Bool]
bits w = go 0
    where
        go :: Word -> [Bool]
        go !n =
            if n == bitCount w
                then []
                else bit n w : go (succ n)

{- | Pack a list of bits into an integral value, the inverse of 'bits'.

note(s)

  * Argument list is truncated to a list of 'bitCount' length.
-}
{-# INLINEABLE pack #-}
pack :: forall w . (Integral w, FiniteBits w) => [Bool] -> w
pack = foldl' (.|.) 0 . fmap shift . zip [0 .. pred $ bitCount @w 0]
    where
        shift :: (Word, Bool) -> w
        shift (n, b) = if b then Bits.bit (fromIntegral n) else zeroBits 
