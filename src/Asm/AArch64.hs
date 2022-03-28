{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Asm.AArch64
    ( AArch64

    -- * Registers
    , Register
    , x0, x1, x2, x8
    , w0, w1

    -- * Instructions
    , instr
    , adr
    , b
    , MovData (..)
    , movk
    , movn
    , movz
    , ldr
    , svc
    ) where

import Control.Exception.ContextTH
import Control.Monad
import Control.Monad.Catch
import Data.Bits
import Data.ByteString.Builder
import Data.Elf.Constants
import Data.Int
import Data.Kind
import Data.Singletons.TH
import Data.Word

import Asm.Asm
import Asm.Data

$(singletons [d| data RegisterWidth = X | W |])

data AArch64

instance KnownArch AArch64 where
    data Instruction AArch64 = Instruction { getInstruction :: Word32 } -- deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)
    instructionSize = const 4
    ltorgAlign = const 4
    type RelocationType AArch64 = ElfRelocationType_AARCH64
    relocate = relocate'
    serializeInstruction = word32LE . getInstruction

type Register :: RegisterWidth -> Type
newtype Register c = R Word32

x0, x1, x2, x8 :: Register 'X
x0 = R 0
x1 = R 1
x2 = R 2
x8 = R 8

w0, w1 :: Register 'W
w0 = R 0
w1 = R 1

b64 :: forall w . SingI w => Register w -> Word32
b64 _ = case sing @w of
    SX -> 1
    SW -> 0

type Word9  = Word16
type Word21 = Word32
type Word26 = Word32

instr :: CodeMonad AArch64 m => Word32 -> m ()
instr w = void $ emit (Instruction w)

instrReloc :: CodeMonad AArch64 m => Word32 -> Label -> ElfRelocationType_AARCH64 -> m ()
instrReloc w l r = emitReloc (Instruction w) l r

-- emitReloc :: CodeMonad a m => Instruction a -> Label -> ElfRelocationType_AARCH64 -> m ()
-- emitReloc w l r = do
--     a <- offset
--     void $ emit (Instruction w) (Just $ LabelRelocation l r a)

-- | C6.2.10 ADR
adr_ :: Register 'X -> Word21 -> Word32
adr_ (R n) imm21 = 0x10000000 .|. imm .|. n
    where
        imm21' = fixWord 21 imm21 -- FIXME: Word21 should keep verified integer
        immlo = imm21' .&. 3
        immhi = imm21' `shiftR` 2
        imm   = (immhi `shift` 5) .|. (immlo `shift` 29)

class ArgADR w where
    adr :: CodeMonad AArch64 m => Register 'X -> w -> m ()
instance ArgADR Word21 where
    adr r w = instr $ adr_ r w
instance ArgADR Label where
    adr r l = instrReloc (adr_ r 0) l R_AARCH64_ADR_PREL_LO21

-- offsetToImm21 :: MonadThrow m => TextAddress -> m Word21
-- offsetToImm21 (TextAddress o)
--   | not $ isBitN 21 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral (o .&. mask 21)

-- | C6.2.26 B
b_ :: Word26 -> Word32
b_ imm26 = 0x14000000 .|. imm26'
    where
        imm26' = fixWord 26 imm26 -- FIXME: Word26 should keep verified integer

class ArgB w where
    b :: CodeMonad AArch64 m => w -> m ()
instance ArgB Word26 where
    b imm26 = instr $ b_ imm26
instance ArgB Label where
    b l = instrReloc (b_ 0) l R_AARCH64_JUMP26

-- offsetToImm26 :: MonadThrow m => TextAddress -> m Word26
-- offsetToImm26 (TextAddress o)
--   | o .&. 0x3 /= 0    = $chainedError $ "offset is not aligned: " ++ show o
--   | not $ isBitN 28 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral ((o `shiftR` 2) .&. mask 26)

-- | C6.2.190 MOVК
data MovData = LSL0  Word16
             | LSL16 Word16
             | LSL32 Word16
             | LSL48 Word16

movDataToInstrBits :: MovData -> Word32
movDataToInstrBits d = s `shift` 21 .|. (fromIntegral w `shift` 5)
    where
        (s, w) = case d of
            (LSL0  w') -> (0, w')
            (LSL16 w') -> (1, w')
            (LSL32 w') -> (2, w')
            (LSL48 w') -> (3, w')

movk :: (CodeMonad AArch64 m, SingI w) => Register w -> MovData -> m ()
movk r@(R n) d = instr $ (b64 r `shift` 31)
                      .|. 0x72800000
                      .|. movDataToInstrBits d
                      .|. n

-- | C6.2.191 MOVN
movn :: (CodeMonad AArch64 m, SingI w) => Register w -> MovData -> m ()
movn r@(R n) d = instr $ (b64 r `shift` 31)
                      .|. 0x12800000
                      .|. movDataToInstrBits d
                      .|. n

-- | C6.2.192 MOVZ
movz :: (CodeMonad AArch64 m, SingI w) => Register w -> MovData -> m ()
movz r@(R n) d = instr $ (b64 r `shift` 31)
                      .|. 0x52800000
                      .|. movDataToInstrBits d
                      .|. n

-- | C6.2.132 LDR (literal)
ldr :: (CodeMonad AArch64 m, SingI w) => Register w -> Word9 -> m ()
ldr r@(R n) imm9 = instr $ (b64 r `shift` 30)
                        .|. 0x18000000
                        .|. (imm9' `shift` 5)
                        .|. n
    where
        imm9' = fixWord 9 imm9 -- FIXME: Word9 should keep verified integer

-- offsetToImm9 :: MonadThrow m => TextAddress -> m Word9
-- offsetToImm9 (TextAddress o)
--   | o .&. 0x3 /= 0    = $chainedError $ "offset is not aligned: " ++ show o
--   | not $ isBitN 11 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral ((o `shiftR` 2) .&. mask 9)


-- | C6.2.317 SVC
svc ::CodeMonad AArch64 m => Word16 -> m ()
svc imm = instr $ 0xd4000001 .|. (fromIntegral imm `shift` 5)

relocate' ::      MonadThrow m =>
    ElfRelocationType_AARCH64 ->
          Instruction AArch64 ->
                  TextAddress ->
                  TextAddress ->
                        Int64 -> m (Instruction AArch64)
relocate' R_AARCH64_JUMP26 (Instruction w) (TextAddress p) (TextAddress s) a = do
    let
        x = (s + a - p) `shiftR` 2
    imm <- $maybeAddContext "imm does not fit" $ fitN 26 x
    return $ Instruction $ (w .&. 0xfc000000) .|. imm
relocate' R_AARCH64_ADR_PREL_LO21 (Instruction w) (TextAddress p) (TextAddress s) a = do
    let
        x = (s + a - p)
    imm21 <- $maybeAddContext "imm does not fit" $ fitN 21 x
    let
        immlo = imm21 .&. 3
        immhi = imm21 `shiftR` 2
        imm   = (immhi `shift` 5) .|. (immlo `shift` 29)
    return $ Instruction $ (w .&. 0x9f00001f) .|. imm
relocate' rl _ _ _ _ = $chainedError ("relocation is not implemented: " <> show rl)
