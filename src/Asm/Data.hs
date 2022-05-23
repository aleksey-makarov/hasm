{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MagicHash #-}

module Asm.Data where

import Data.Bits
import Data.Int
import Data.Ix
import Data.Word
import Numeric

newtype SectionOffset  = SectionOffset  { getSectionOffset  :: Int64 }
    deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits, Ix)

int64ToWord64 :: Int64 -> Word64
int64ToWord64 = fromIntegral

{-# INLINE int64ToWord64 #-}
padLeadingZeros :: Int -> String -> String
padLeadingZeros n s | length s > n = error "padLeadingZeros args"
                    | otherwise = replicate (n - length s) '0' ++ s

printInt64 :: Int64 -> String
printInt64 n = printWord64 $ int64ToWord64 n

printWord32 :: Word32 -> String
printWord32 n = padLeadingZeros 8 $ showHex n ""

printWord64 :: Word64 -> String
printWord64 n = padLeadingZeros 16 $ showHex n ""

readWord32 :: String -> Word32
readWord32 s = case readHex s of
    [(res, "")] -> res
    _ -> error "readHex error"

mask :: (Num b, Bits b, Ord b) => Int -> b
mask n = (1 `shift` n) - 1

fitN :: Int -> Int64 -> Either String Word32
fitN bitN w =
    if (if w >= 0 then h == 0 else h == complement m) -- FIXME: wrong! (???) what about the bit at (bitN - 1)???
        then Right $ fromIntegral (w .&. m)
        else Left ("fitN error: n: " ++ show bitN ++ " w: " ++ printInt64 w ++ " mask: " ++ printInt64 m)
    where
        m = mask bitN
        h = w .&. complement m

fixWord :: Integral a => Int -> a -> Word32
fixWord bitN v = mask bitN .&. fromIntegral v

isPower2 :: (Bits i, Num i) => i -> Bool
isPower2 n = n .&. (n - 1) == 0
