{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    , x0, x1, x2, x8, x20, x21, x22, x29, x30, sp
    , w0, w1, w2, w19

    , ArithmeticArgument (..)
    , BitwiseArgument (..)
    , Cond (..)
    , MovData (..)
    , Shift (..)

    -- * Instructions
    , instr
    , add
    , adr
    , Asm.AArch64.and
    , bcond
    , b
    , cmp
    , csel
    , ldr
    , mov
    , movk
    , movn
    , movz
    , ret
    , sub
    , subs
    , svc
    ) where

import Prelude as P hiding (EQ, GT, LT)

import Control.Exception.ContextTH
import Control.Monad
import Control.Monad.Catch
import Data.Bits
import Data.ByteString.Builder
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Int
import Data.Kind
import Data.Singletons.TH
import Data.Word

import Asm.Asm
import Asm.Data
import Asm.Relocation

$(singletons [d| data RegisterWidth = X | W |])

data AArch64

instance KnownArch AArch64 where
    data Instruction AArch64 = Instruction { getInstruction :: Word32 } -- deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)
    instructionSize = const 4
    serializeInstruction = word32LE . getInstruction

    ltorgAlign = const 4
    mkRelocation = mkRelocationAArch64

type instance RelocationType AArch64 = ElfRelocationType_AARCH64
type instance ArchElfClass AArch64 = 'ELFCLASS64

type Register :: RegisterWidth -> Type
newtype Register c = R Word32

type SP :: RegisterWidth -> Type
data SP w = SP

sp :: SP 'X
sp = sp

x0, x1, x2, x8, x20, x21, x22, x29, x30 :: Register 'X
x0  = R 0
x1  = R 1
x2  = R 2
x8  = R 8
x20 = R 20
x21 = R 21
x22 = R 22
x29 = R 29
x30 = R 30

w0, w1, w2, w19 :: Register 'W
w0  = R 0
w1  = R 1
w2  = R 2
w19 = R 19

b64 :: forall w . SingI w => Register w -> Word32
b64 _ = case sing @w of
    SX -> 1
    SW -> 0

mkReg :: Word32 -> Register w
mkReg n = R n

type Word1  = Word32
type Word2  = Word32
type Word6  = Word32
type Word9  = Word32
type Word12 = Word32
type Word19 = Word32
type Word21 = Word32
type Word26 = Word32

instr :: CodeMonad AArch64 m => Word32 -> m ()
instr w = void $ emit (Instruction w)

instrReloc :: CodeMonad AArch64 m => Word32 -> Symbol -> ElfRelocationType_AARCH64 -> m ()
instrReloc w l r = emitReloc (Instruction w) l r

-- emitReloc :: CodeMonad a m => Instruction a -> Symbol -> ElfRelocationType_AARCH64 -> m ()
-- emitReloc w l r = do
--     a <- offset
--     void $ emit (Instruction w) (Just $ LabelRelocation l r a)

data ArithmeticArgument
    = ExtendedRegister
    | Immediate Word12
    | ImmediateN Word12
    | ShiftedRegister

data BitwiseArgument w
    = BImmediate
        { biN    :: Word1
        , biImmr :: Word6
        , biImms :: Word6
        }
    | BShiftedRegister
        { bsrReg   :: Register w
        , bsrShift :: Shift
        , bsrImm   :: Word6
        }

-- FIXME: use my HT module to generate this (Cond, Shift)
data Cond = EQ -- Equal
          | NE -- Not Equal
          | HI -- Unsigned Higher
          | HS -- Unsigned Higher or Same
          | LS -- Unsigned Lower or Same
          | LO -- Unsigned Lower
          | GT -- Signed Greater Than
          | GE -- Signed Greater Than or Equal
          | LE -- Signed Less Than or Equal
          | LT -- Signed Less Than
          | CS -- Unsigned Overflow (Carry Set)
          | CC -- No Unsigned Overflow (Carry Clear)
          | VS -- Signed Overflow
          | VC -- No Signed Overflow
          | MI -- Minus, Negative
          | PL -- Plus, Positive or Zero
          | AL -- Always Executed
          | NV -- Never Executed

condToEnc :: Cond -> Word32
condToEnc EQ = 0b0000
condToEnc NE = 0b0001
condToEnc HI = 0b1000
condToEnc HS = 0b0010
condToEnc LS = 0b1001
condToEnc LO = 0b0011
condToEnc GT = 0b1100
condToEnc GE = 0b1010
condToEnc LE = 0b1101
condToEnc LT = 0b1011
condToEnc CS = 0b0010
condToEnc CC = 0b0011
condToEnc VS = 0b0110
condToEnc VC = 0b0111
condToEnc MI = 0b0100
condToEnc PL = 0b0101
condToEnc AL = 0b1110
condToEnc NV = 0b1111

data Shift = LSL
           | LSR
           | ASR
           | ROR

shiftToEnc :: Shift -> Word32
shiftToEnc LSL = 0b00
shiftToEnc LSR = 0b01
shiftToEnc ASR = 0b10
shiftToEnc ROR = 0b11

data MovData = LSL0  Word16
             | LSL16 Word16
             | LSL32 Word16
             | LSL48 Word16

-- | C6.2.4 ADD

addImmediate :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Word32 -> Word12 -> m ()
addImmediate rd@(R d) (R n) sh imm = instr $  (b64 rd `shift` 31)
                                          .|. 0x11000000
                                          .|. (sh `shift` 22)
                                          .|. (imm `shift` 10)
                                          .|. (n `shift` 5)
                                          .|. d

add :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> ArithmeticArgument -> m ()
add rd rn (Immediate imm)  = addImmediate rd rn 0 imm
add rd rn (ImmediateN imm) = addImmediate rd rn 1 imm
add _ _ _ = undefined

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
instance ArgADR Symbol where
    adr r l = instrReloc (adr_ r 0) l R_AARCH64_ADR_PREL_LO21

-- offsetToImm21 :: MonadThrow m => SectionOffset -> m Word21
-- offsetToImm21 (SectionOffset o)
--   | not $ isBitN 21 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral (o .&. mask 21)

-- | C6.2.12 AND

andImmediate :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Word1 -> Word6 -> Word6 -> m ()
andImmediate rd@(R d) (R n) nBit immr imms = instr $  (b64 rd `shift` 31)
                                                  .|. 0x12000000
                                                  .|. (nBit `shift` 22)
                                                  .|. (immr `shift` 16)
                                                  .|. (imms `shift` 10)
                                                  .|. (n `shift` 5)
                                                  .|. d

and :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> BitwiseArgument w -> m ()
and rd rn (BImmediate n immr imms) = andImmediate rd rn n immr imms
and _ _ _ = undefined

-- | C6.2.26 B.<cond>

bcond_ :: Cond -> Word19 -> Word32
bcond_ cond imm19 = 0x54000000 .|. (imm19 `shift` 5)
                               .|. condToEnc cond

class ArgBCond a where
    bcond :: CodeMonad AArch64 m => Cond -> a -> m ()
instance ArgBCond Word19 where
    bcond cond imm19 = instr $ bcond_ cond imm19
instance ArgBCond Symbol where
    bcond cond l = instrReloc (bcond_ cond 0) l R_AARCH64_CONDBR19

-- | C6.2.26 B

b_ :: Word26 -> Word32
b_ imm26 = 0x14000000 .|. imm26'
    where
        imm26' = fixWord 26 imm26 -- FIXME: Word26 should keep verified integer

class ArgB w where
    b :: CodeMonad AArch64 m => w -> m ()
instance ArgB Word26 where
    b imm26 = instr $ b_ imm26
instance ArgB Symbol where
    b l = instrReloc (b_ 0) l R_AARCH64_JUMP26

-- offsetToImm26 :: MonadThrow m => SectionOffset -> m Word26
-- offsetToImm26 (SectionOffset o)
--   | o .&. 0x3 /= 0    = $chainedError $ "offset is not aligned: " ++ show o
--   | not $ isBitN 28 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral ((o `shiftR` 2) .&. mask 26)

-- | C6.2.61 CMP

cmp :: (CodeMonad AArch64 m, SingI w) => Register w -> ArithmeticArgument -> m () -- FIXME
cmp rn arg = subs (mkReg 31) rn arg

-- | C6.2.69 CSEL

csel :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Register w -> Cond -> m () -- FIXME
csel rd@(R d) (R n) (R m) cond = instr $  (b64 rd `shift` 31)
                                      .|. 0x1a800000
                                      .|. (m `shift` 16)
                                      .|. (condToEnc cond `shift` 12)
                                      .|. (n `shift` 5)
                                      .|. d

-- | C6.2.187 MOV

class ArgMov (a :: RegisterWidth -> Type) (b :: RegisterWidth -> Type) where
    mov :: (CodeMonad AArch64 m, SingI w) => a w -> b w -> m ()
instance ArgMov Register SP where
    mov _rd _ = undefined
instance ArgMov SP Register where
    mov _ _rm = undefined
instance ArgMov Register Register where
    mov rd rm = orrShiftedRegister rd (mkReg 31) rm LSL 0
instance ArgMov Register BitwiseArgument where
    mov rd (BImmediate n immr imms) = orrImmediate rd (mkReg 31) n immr imms
    mov rd (BShiftedRegister rm sht imm6) = orrShiftedRegister rd (mkReg 31) rm sht imm6

-- | C6.2.190 MOVК

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

-- offsetToImm9 :: MonadThrow m => SectionOffset -> m Word9
-- offsetToImm9 (SectionOffset o)
--   | o .&. 0x3 /= 0    = $chainedError $ "offset is not aligned: " ++ show o
--   | not $ isBitN 11 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral ((o `shiftR` 2) .&. mask 9)

-- | C6.2.206 ORR (shifted register)

orrImmediate :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Word1 -> Word6 -> Word6 -> m ()
orrImmediate rd@(R d) (R n) nBit immr imms = instr $  (b64 rd `shift` 31)
                                                  .|. 0x32000000
                                                  .|. (nBit `shift` 22)
                                                  .|. (immr `shift` 16)
                                                  .|. (imms `shift` 10)
                                                  .|. (n `shift` 5)
                                                  .|. d

orrShiftedRegister :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Register w -> Shift -> Word6 -> m ()
orrShiftedRegister rd@(R d) (R n) (R m) sht imm6 = instr $  (b64 rd `shift` 31)
                                                        .|. 0x2a000000
                                                        .|. (shiftToEnc sht `shift` 22)
                                                        .|. (m `shift` 16)
                                                        .|. (imm6 `shift` 10)
                                                        .|. (n `shift` 5)
                                                        .|. d

orr :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> BitwiseArgument w -> m ()
orr rd rn (BImmediate n immr imms) = orrImmediate rd rn n immr imms
orr rd rn (BShiftedRegister rm sht imm6) = orrShiftedRegister rd rn rm sht imm6

-- | C6.2.219 RET

ret :: CodeMonad AArch64 m => Register 'X -> m ()
ret (R n) = instr $ 0xd65f0000 .|. (n `shift` 5)

-- | C6.2.308 SUB

subsImmediate :: (CodeMonad AArch64 m, SingI w) => Word1 -> Register w -> Register w -> Word32 -> Word12 -> m ()
subsImmediate s rd@(R d) (R n) sh imm = instr $  (b64 rd `shift` 31)
                                             .|. 0x51000000
                                             .|. (s `shift` 29)
                                             .|. (sh `shift` 22)
                                             .|. (imm `shift` 10)
                                             .|. (n `shift` 5)
                                             .|. d

sub :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> ArithmeticArgument -> m ()
sub rd rn (Immediate imm)  = subsImmediate 0 rd rn 0 imm
sub rd rn (ImmediateN imm) = subsImmediate 0 rd rn 1 imm
sub _ _ _ = undefined

-- | C6.2.315 SUBS

subs :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> ArithmeticArgument -> m ()
subs rd rn (Immediate imm)  = subsImmediate 1 rd rn 0 imm
subs rd rn (ImmediateN imm) = subsImmediate 1 rd rn 1 imm
subs _ _ _ = undefined

-- | C6.2.317 SVC
svc :: CodeMonad AArch64 m => Word16 -> m ()
svc imm = instr $ 0xd4000001 .|. (fromIntegral imm `shift` 5)

mkRelocationAArch64 ::
                 MonadThrow m =>
    ElfRelocationType_AARCH64 ->
                SectionOffset -> -- p (he address of the place being relocated)
                SectionOffset -> -- s (is the address of the symbol)
                        Int64 -> -- a (the addend for the relocation)
                                 m (RelocationMonad ())
mkRelocationAArch64 R_AARCH64_JUMP26 p@(SectionOffset p') (SectionOffset s) a = do
    let
        x = (s + a - p') `shiftR` 2
    imm <- $maybeAddContext "imm does not fit" $ fitN 26 x
    let
        f w = (w .&. 0xfc000000) .|. imm
    return $ modifyWord32LE p f
mkRelocationAArch64 R_AARCH64_CONDBR19 p@(SectionOffset p') (SectionOffset s) a = do
    let
        x = (s + a - p') `shiftR` 2
    imm <- $maybeAddContext "imm does not fit" $ fitN 19 x
    let
        f w = (w .&. 0xff00001f) .|. (imm `shift` 5)
    return $ modifyWord32LE p f
mkRelocationAArch64 R_AARCH64_ADR_PREL_LO21 p@(SectionOffset p') (SectionOffset s) a = do
    let
        x = (s + a - p')
    imm21 <- $maybeAddContext "imm does not fit" $ fitN 21 x
    let
        immlo = imm21 .&. 3
        immhi = imm21 `shiftR` 2
        imm   = (immhi `shift` 5) .|. (immlo `shift` 29)
        f w = (w .&. 0x9f00001f) .|. imm
    return $ modifyWord32LE p f
mkRelocationAArch64 rl _ _ _ = $chainedError ("relocation is not implemented: " <> show rl)
