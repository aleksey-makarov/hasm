{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

-- FIXME: use linear types for the array instead of ST (?)

module Asm.Relocation
    ( RelocationMonad

    , readWord8
    , writeWord8
    , modifyWord8

    , readWord32LE
    , writeWord32LE
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

newtype RelocationMonad a
    = RelocationMonad
        { unRelocationMonad :: forall s . ReaderT (STUArray s SectionOffset Word8) (ST s) a}
    deriving (Functor)

instance Applicative RelocationMonad where
    pure a = RelocationMonad $ return a
    mf <*> ma = RelocationMonad $ unRelocationMonad mf <*> unRelocationMonad ma

instance Monad RelocationMonad where
    ma >>= fmb = RelocationMonad $ unRelocationMonad ma >>= \x -> unRelocationMonad $ fmb x

instance Semigroup (RelocationMonad ()) where
    a <> b = RelocationMonad $ (unRelocationMonad a) >> (unRelocationMonad b)

instance Monoid (RelocationMonad ()) where
    mempty = RelocationMonad $ return ()

readWord8 :: SectionOffset -> RelocationMonad Word8
readWord8 i = RelocationMonad $ do
    a <- ask
    lift $ readArray a i

writeWord8 :: SectionOffset -> Word8 -> RelocationMonad ()
writeWord8 i w = RelocationMonad $ do
    a <- ask
    lift $ writeArray a i w

modifyWord8 :: SectionOffset -> (Word8 -> Word8) -> RelocationMonad ()
modifyWord8 i f = f <$> readWord8 i >>= writeWord8 i

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

readWord32LE :: SectionOffset -> RelocationMonad Word32
readWord32LE i = toWord32LE <$> readWord8 (i + 0)
                            <*> readWord8 (i + 1)
                            <*> readWord8 (i + 2)
                            <*> readWord8 (i + 3)

writeWord32LE :: SectionOffset -> Word32 -> RelocationMonad ()
writeWord32LE i w = do
    writeWord8 (i + 0) v0
    writeWord8 (i + 1) v1
    writeWord8 (i + 2) v2
    writeWord8 (i + 3) v3
    where
        (v0, v1, v2, v3) = fromWord32LE w

modifyWord32LE :: SectionOffset -> (Word32 -> Word32) -> RelocationMonad ()
modifyWord32LE i f = f <$> readWord32LE i >>= writeWord32LE i

relocate :: ByteString -> RelocationMonad () -> ByteString
relocate bs (RelocationMonad ma) = pack $ elems $ runSTUArray $ do
    let
        l = fromIntegral $ BSL.length bs
    a <- newListArray (0, l - 1) $ unpack bs
    runReaderT ma a
    return a
