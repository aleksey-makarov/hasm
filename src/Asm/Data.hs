{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Asm.Data where

import Data.Bits
import Data.Int
import Data.Ix
import Data.Word

newtype SectionOffset  = SectionOffset  { getSectionOffset  :: Int64 }
    deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits, Ix)

mask :: (Num b, Bits b, Ord b) => Int -> b
mask n = (1 `shift` n) - 1

fitN :: Int -> Int64 -> Maybe Word32
fitN bitN w =
    if (if w >= 0 then h == 0 else h == m) -- FIXME: wrong! (???) what about the bit at (bitN - 1)???
        then Just $ fromIntegral (w .&. m)
        else Nothing
    where
        m = mask bitN
        h = w .&. complement m

fixWord :: Integral a => Int -> a -> Word32
fixWord bitN v = mask bitN .&. fromIntegral v

isPower2 :: (Bits i, Num i) => i -> Bool
isPower2 n = n .&. (n - 1) == 0
