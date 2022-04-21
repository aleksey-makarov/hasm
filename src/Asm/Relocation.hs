{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- FIXME: use linear types for the array instead of ST (?)

module Asm.Relocation
    ( RelocationMonad
    , modifyWord8
    , modifyWord32LE
    , relocate
    ) where

import Control.Monad.Reader
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.ByteString.Lazy as BSL
import Data.Word

import Asm.Data

newtype RelocationMonad = RelocationMonad { unRelocationMonad :: forall s . ReaderT (STUArray s SectionOffset Word8) (ST s) () }

instance Semigroup RelocationMonad where
    (<>) a b = RelocationMonad $ (unRelocationMonad a) >> (unRelocationMonad b)

instance Monoid RelocationMonad where
    mempty = RelocationMonad $ return ()

read8 :: Ix i => i -> ReaderT (STUArray s i Word8) (ST s) Word8
read8 i = do
    a <- ask
    lift $ readArray a i

write8 :: Ix i => i -> Word8 -> ReaderT (STUArray s i Word8) (ST s) ()
write8 i w = do
    a <- ask
    lift $ writeArray a i w

modifyWord8 :: SectionOffset -> (Word8 -> Word8) -> RelocationMonad
modifyWord8 i f = RelocationMonad (f <$> read8 i >>= write8 i)

toWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
toWord32LE v0 v1 v2 v3 = fromIntegral v0 `shiftL` 0
                     .|. fromIntegral v1 `shiftL` 8
                     .|. fromIntegral v2 `shiftL` 16
                     .|. fromIntegral v3 `shiftL` 24

fromWord32LE :: Word32 -> (Word8, Word8, Word8, Word8)
fromWord32LE w = ( fromIntegral (w `shiftR` 0  .&. 0xff)
                 , fromIntegral (w `shiftR` 8  .&. 0xff)
                 , fromIntegral (w `shiftR` 16 .&. 0xff)
                 , fromIntegral (w `shiftR` 24 .&. 0xff)
                 )

read32LE :: (Ix i, Num i) => i -> ReaderT (STUArray s i Word8) (ST s) Word32
read32LE i = toWord32LE <$> read8 (i + 0)
                        <*> read8 (i + 1)
                        <*> read8 (i + 2)
                        <*> read8 (i + 3)

write32LE :: (Ix i, Num i) => i -> Word32 -> ReaderT (STUArray s i Word8) (ST s) ()
write32LE i w = do
    write8 (i + 0) v0
    write8 (i + 1) v1
    write8 (i + 2) v2
    write8 (i + 3) v3
    where
        (v0, v1, v2, v3) = fromWord32LE w

modifyWord32LE :: SectionOffset -> (Word32 -> Word32) -> RelocationMonad
modifyWord32LE i f = RelocationMonad (f <$> read32LE i >>= write32LE i)

relocate :: ByteString -> RelocationMonad -> ByteString
relocate bs (RelocationMonad ma) = pack $ elems $ runSTUArray $ do
    let
        l = fromIntegral $ BSL.length bs
    a <- newListArray (0, l - 1) $ unpack bs
    runReaderT ma a
    return a
