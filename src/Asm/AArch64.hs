{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
    , x0, x1, x2, x8, x19, x20, x21, x22, x29, x30, sp
    , w0, w1, w2, w19, w21

    , ExtendedRegister (..)
    , Immediate (..)
    , BitPattern (..)
    , ShiftedRegister (..)

    , Cond (..)
    , MovData (..)
    , Shift (..)

    , Address (..)
    , PAddress (..)

    -- * Instructions
    , instr
    , add
    , adr
    , Asm.AArch64.and
    , bcond
    , b
    , bl
    , cmp
    , csel
    , ldp
    , ldr
    , ldrb
    , lsl
    , lsr
    , movsp
    , mov
    , movk
    , movn
    , movz
    , nop
    , ret
    , stp
    , str
    , strb
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
import Asm.Data as AD
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

x0, x1, x2, x8, x20, x19, x21, x22, x29, x30, sp :: Register 'X
x0  = R 0
x1  = R 1
x2  = R 2
x8  = R 8
x19 = R 19
x20 = R 20
x21 = R 21
x22 = R 22
x29 = R 29
x30 = R 30
sp  = R 31

w0, w1, w2, w19, w21 :: Register 'W
w0  = R 0
w1  = R 1
w2  = R 2
w19 = R 19
w21 = R 21

b64 :: forall w . SingI w => Register w -> Word32
b64 _ = case sing @w of
    SX -> 1
    SW -> 0

mkReg :: Word32 -> Register w
mkReg n = R n

type Word1  = Word32
type Word2  = Word32
type Word3  = Word32
type Word5  = Word32
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

data ExtendedRegister (w :: RegisterWidth)
    = ExtendedRegister
        { erReg    :: Word5
        , erExtend :: Extend
        , erAmount :: Word3
        }

data Immediate (w :: RegisterWidth)
    = Immediate Word12
    | ImmediateN Word12

data BitPattern (w :: RegisterWidth)
    = BitPattern
        { bpN    :: Word1
        , bpImmr :: Word6
        , bpImms :: Word6
        }

data ShiftedRegister (w :: RegisterWidth)
    = ShiftedRegister
        { srReg   :: Register w
        , srShift :: Shift
        , srImm   :: Word6
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

data Extend = UXTB
            | UXTH
            | UXTW
            | UXTX
            | SXTB
            | SXTH
            | SXTW
            | SXTX

extendToEnc :: Extend -> Word32
extendToEnc UXTB = 0b000
extendToEnc UXTH = 0b001
extendToEnc UXTW = 0b010
extendToEnc UXTX = 0b011
extendToEnc SXTB = 0b100
extendToEnc SXTH = 0b101
extendToEnc SXTW = 0b110
extendToEnc SXTX = 0b111

data MovData = LSL0  Word16
             | LSL16 Word16
             | LSL32 Word16
             | LSL48 Word16

data PAddress
    = PPostIndex
        { ppostiR   :: Register 'X
        , ppostiImm :: Int32
        }
    | PPreIndex
        { ppreiR   :: Register 'X
        , ppreiImm :: Int32
        }
    | PSignedOffset
        { psoR   :: Register 'X
        , psoImm :: Int32
        }

data Address
    = PostIndex
        { postiR   :: Register 'X
        , postiImm :: Int32
        }
    | PreIndex
        { preiR   :: Register 'X
        , preiImm :: Int32
        }
    | UnsignedOffset
        { soR   :: Register 'X
        , soImm :: Word32
        }

-- | C6.2.4 ADD

class ArgArithm a where
    add, sub, subs :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> a w -> m ()

instance ArgArithm Immediate where
    add  rd rn (Immediate imm)  = addImmediate    rd rn 0 imm
    add  rd rn (ImmediateN imm) = addImmediate    rd rn 1 imm
    sub  rd rn (Immediate imm)  = subsImmediate 0 rd rn 0 imm
    sub  rd rn (ImmediateN imm) = subsImmediate 0 rd rn 1 imm
    subs rd rn (Immediate imm)  = subsImmediate 1 rd rn 0 imm
    subs rd rn (ImmediateN imm) = subsImmediate 1 rd rn 1 imm

instance ArgArithm ExtendedRegister where
    add = undefined
    sub rd rn (ExtendedRegister m extend amount) = subsExtendedRegister 0 rd rn m extend amount
    subs rd rn (ExtendedRegister m extend amount) = subsExtendedRegister 1 rd rn m extend amount

instance ArgArithm ShiftedRegister where
    add  rd rn ShiftedRegister { .. } = addsShiftedRegister 0 rd rn srReg srShift srImm
    sub = undefined
    subs rd rn ShiftedRegister { .. } = subsShiftedRegister 1 rd rn srReg srShift srImm

instance ArgArithm Register where
    add  rd rn rm = add rd rn $ ShiftedRegister rm LSL 0
    sub  = undefined
    subs = undefined

addImmediate :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Word32 -> Word12 -> m ()
addImmediate rd@(R d) (R n) sh imm = instr $  (b64 rd `shift` 31)
                                          .|. 0x11000000
                                          .|. (sh `shift` 22)
                                          .|. (imm `shift` 10)
                                          .|. (n `shift` 5)
                                          .|. d

addsShiftedRegister :: (CodeMonad AArch64 m, SingI w) =>
                                                Word1 ->
                                           Register w ->
                                           Register w ->
                                           Register w ->
                                                Shift ->
                                                Word6 -> m ()
addsShiftedRegister s rd@(R d) (R n) (R m) sht imm6 = instr $  (b64 rd `shift` 31)
                                                           .|. 0x0b000000
                                                           .|. (s `shift` 29)
                                                           .|. (shiftToEnc sht `shift` 22)
                                                           .|. (m `shift` 16)
                                                           .|. (imm6 `shift` 10)
                                                           .|. (n `shift` 5)
                                                           .|. d

-- | C6.2.10 ADR

adr_ :: Register 'X -> Word21 -> Word32
adr_ (R n) imm21 = 0x10000000 .|. imm .|. n
    where
        imm21' = fixWord 21 imm21 -- FIXME: Word21 should keep verified integer
        immlo = imm21' .&. 3
        immhi = imm21' `shiftR` 2
        imm   = (immhi `shift` 5) .|. (immlo `shift` 29)

class ArgADR a where
    adr :: CodeMonad AArch64 m => Register 'X -> a -> m ()
instance ArgADR Word21 where
    adr r w = instr $ adr_ r w
instance ArgADR Symbol where
    adr r l = instrReloc (adr_ r 0) l R_AARCH64_ADR_PREL_LO21

-- offsetToImm21 :: MonadThrow m => SectionOffset -> m Word21
-- offsetToImm21 (SectionOffset o)
--   | not $ isBitN 21 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral (o .&. mask 21)

-- | C6.2.12 AND

class ArgBitwise a where
    and, orr :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> a w -> m ()

instance ArgBitwise BitPattern where
    and rd rn (BitPattern n immr imms) = andImmediate rd rn n immr imms
    orr rd rn (BitPattern n immr imms) = orrImmediate rd rn n immr imms

instance ArgBitwise ShiftedRegister where
    and = undefined
    orr rd rn (ShiftedRegister rm sht imm6) = orrShiftedRegister rd rn rm sht imm6

andImmediate :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Word1 -> Word6 -> Word6 -> m ()
andImmediate rd@(R d) (R n) nBit immr imms = instr $  (b64 rd `shift` 31)
                                                  .|. 0x12000000
                                                  .|. (nBit `shift` 22)
                                                  .|. (immr `shift` 16)
                                                  .|. (imms `shift` 10)
                                                  .|. (n `shift` 5)
                                                  .|. d

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

b_ :: Word1 -> Word26 -> Word32
b_ op imm26 = (op `shift` 31) .|. 0x14000000 .|. imm26

class ArgB w where
    b, bl :: CodeMonad AArch64 m => w -> m ()
instance ArgB Word26 where
    b  imm26 = instr $ b_ 0 imm26
    bl imm26 = instr $ b_ 1 imm26
instance ArgB Symbol where
    b  l = instrReloc (b_ 0 0) l R_AARCH64_JUMP26
    bl l = instrReloc (b_ 1 0) l R_AARCH64_JUMP26

-- offsetToImm26 :: MonadThrow m => SectionOffset -> m Word26
-- offsetToImm26 (SectionOffset o)
--   | o .&. 0x3 /= 0    = $chainedError $ "offset is not aligned: " ++ show o
--   | not $ isBitN 28 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral ((o `shiftR` 2) .&. mask 26)

-- | C6.2.61 CMP

class ArgArithm1 a where
    cmp :: (CodeMonad AArch64 m, SingI w) => Register w -> a w -> m ()
instance ArgArithm1 Register where
    cmp rn rm = subs (R 31) rn $ ShiftedRegister rm LSL 0
instance ArgArithm1 Immediate where
    cmp rn arg = subs (mkReg 31) rn arg

-- | C6.2.69 CSEL

csel :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Register w -> Cond -> m () -- FIXME
csel rd@(R d) (R n) (R m) cond = instr $  (b64 rd `shift` 31)
                                      .|. 0x1a800000
                                      .|. (m `shift` 16)
                                      .|. (condToEnc cond `shift` 12)
                                      .|. (n `shift` 5)
                                      .|. d

-- | C6.2.129 LDP

-- FIXME: check the `imm` argument
ldstp :: forall m w . (CodeMonad AArch64 m, SingI w) => Word32 -> Register 'X -> Int32 -> Register w -> Register w -> m ()
ldstp w (R n) imm rt1@(R t1) (R t2) = instr $  w
                                           .|. (b64 rt1 `shift` 31)
                                           .|. (immW   `shift` 15)
                                           .|. (t2     `shift` 10)
                                           .|. (n      `shift`  5)
                                           .|. (t1     `shift`  0)
    where
        immW :: Word32
        immW = fromIntegral (AD.mask 7 .&. imm')
        imm' :: Int32
        imm' = case sing @w of
            SW -> imm `shiftR` 2
            SX -> imm `shiftR` 3

ldp :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> PAddress -> m ()
ldp rt1 rt2 PPostIndex { .. }    = ldstp 0x28c00000 ppostiR ppostiImm rt1 rt2
ldp rt1 rt2 PPreIndex { .. }     = ldstp 0x29c00000 ppreiR  ppreiImm  rt1 rt2
ldp rt1 rt2 PSignedOffset { .. } = ldstp 0x29400000 psoR    psoImm    rt1 rt2

-- | C6.2.131 LDR (Immediate)

ldstSz :: forall w . SingI w => Register w -> Word2
ldstSz _ = case sing @w of
    SW -> 0b10
    SX -> 0b11

ldstFixImm :: forall w . SingI w => Register w -> Int32 -> Word9
ldstFixImm _ imm = fromIntegral (AD.mask 9 .&. imm)

ldstuFixImm :: forall w . SingI w => Register w -> Word32 -> Word12
ldstuFixImm _ imm = AD.mask 12 .&. imm'
    where
        imm' = case sing @w of
            SW -> imm `shiftR` 2
            SX -> imm `shiftR` 3

ldst :: CodeMonad AArch64 m => Word32 -> Word2 -> Register 'X -> Word9 -> Register w -> m ()
ldst w sz (R n) imm (R t) = instr $ w
                                 .|. (sz  `shift` 30)
                                 .|. (imm `shift` 12)
                                 .|. (n   `shift`  5)
                                 .|. (t   `shift`  0)

ldstu :: CodeMonad AArch64 m => Word32 -> Word2 -> Register 'X -> Word12 -> Register w -> m ()
ldstu w sz (R n) imm (R t) = instr $ w
                                  .|. (sz  `shift` 30)
                                  .|. (imm `shift` 10)
                                  .|. (n   `shift`  5)
                                  .|. (t   `shift`  0)

ldrimm :: (CodeMonad AArch64 m, SingI w) => Register w -> Address -> m ()
ldrimm rt PostIndex { .. }      = ldst  0xb8400400 (ldstSz rt) postiR (ldstFixImm  rt postiImm) rt
ldrimm rt PreIndex { .. }       = ldst  0xb8400c00 (ldstSz rt) preiR  (ldstFixImm  rt preiImm)  rt
ldrimm rt UnsignedOffset { .. } = ldstu 0xb9400000 (ldstSz rt) soR    (ldstuFixImm rt soImm)    rt

-- | C6.2.132 LDR (literal)

ldrlit :: (CodeMonad AArch64 m, SingI w) => Register w -> Word9 -> m ()
ldrlit r@(R n) imm9 = instr $ 0x18000000
                           .|. (b64 r `shift` 30)
                           .|. (imm9  `shift`  5)
                           .|. (n     `shift`  0)

class ArgLdr a where
    ldr :: (CodeMonad AArch64 m, SingI w) => Register w -> a -> m ()
instance ArgLdr Word9 where
    ldr = ldrlit
instance ArgLdr Address where
    ldr = ldrimm

-- | C6.2.131 LDRB (Immediate)

ldrb :: (CodeMonad AArch64 m, SingI w) => Register w -> Address -> m ()
ldrb rt PostIndex { .. }      = ldst  0x38400400 0 postiR (ldstFixImm rt postiImm) rt
ldrb rt PreIndex { .. }       = ldst  0x38400c00 0 preiR  (ldstFixImm rt  preiImm) rt
ldrb rt UnsignedOffset { .. } = ldstu 0x39400000 0 soR    soImm                    rt

-- | C6.2.178 LSL (immediate)

lsl :: forall m w . (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Word6 -> m ()
lsl rd rn sht = ubfm rd rn bp
    where
        bp = case sing @w of
            SW -> BitPattern 0 (negate sht .&. AD.mask 5) (31 - sht)
            SX -> BitPattern 1 (negate sht .&. AD.mask 6) (63 - sht)

-- | C6.2.180 LSR (register)

lsr :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> Register w -> m ()
lsr rd@(R d) (R n) (R m) = instr $ 0x1ac02400
                                .|. (b64 rd `shift` 31)
                                .|. (m      `shift` 16)
                                .|. (n      `shift`  5)
                                .|. (d      `shift`  0)

-- | C6.2.185 MOV (to/from SP)

movsp :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> m ()
movsp rd rn = add rd rn $ Immediate 0

-- | C6.2.187 MOV

class ArgMov a where
    mov :: (CodeMonad AArch64 m, SingI w) => Register w -> a w -> m ()
instance ArgMov Register where
    mov rd rm = orrShiftedRegister rd (mkReg 31) rm LSL 0
instance ArgMov BitPattern where
    mov rd (BitPattern n immr imms) = orrImmediate rd (mkReg 31) n immr imms
instance ArgMov ShiftedRegister where
    mov rd (ShiftedRegister rm sht imm6) = orrShiftedRegister rd (mkReg 31) rm sht imm6

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

-- | C6.2.203 NOP

nop :: CodeMonad AArch64 m => m ()
nop = instr $ 0xd503201f

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

-- | C6.2.219 RET

ret :: CodeMonad AArch64 m => Register 'X -> m ()
ret (R n) = instr $ 0xd65f0000 .|. (n `shift` 5)

-- | C6.2.273 STP

stp :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> PAddress -> m ()
stp rt1 rt2 PPostIndex { .. }    = ldstp 0x28800000 ppostiR ppostiImm rt1 rt2
stp rt1 rt2 PPreIndex { .. }     = ldstp 0x29800000 ppreiR  ppreiImm  rt1 rt2
stp rt1 rt2 PSignedOffset { .. } = ldstp 0x29000000 psoR    psoImm    rt1 rt2

-- | C6.2.274 STP (Immediate)

str :: (CodeMonad AArch64 m, SingI w) => Register w -> Address -> m ()
str rt PostIndex { .. }      = ldst  0xb8000400 (ldstSz rt) postiR (ldstFixImm  rt postiImm) rt
str rt PreIndex { .. }       = ldst  0xb9000c00 (ldstSz rt) preiR  (ldstFixImm  rt preiImm)  rt
str rt UnsignedOffset { .. } = ldstu 0xb9000000 (ldstSz rt) soR    (ldstuFixImm rt soImm)    rt

-- | C6.2.131 LDRB (Immediate)

strb :: (CodeMonad AArch64 m, SingI w) => Register w -> Address -> m ()
strb rt PostIndex { .. }      = ldst  0x38000400 0 postiR (ldstFixImm rt postiImm) rt
strb rt PreIndex { .. }       = ldst  0x38000c00 0 preiR  (ldstFixImm rt  preiImm) rt
strb rt UnsignedOffset { .. } = ldstu 0x39000000 0 soR    soImm                    rt

-- | C6.2.308 SUB

subsImmediate :: (CodeMonad AArch64 m, SingI w) =>
                                          Word1 ->
                                     Register w ->
                                     Register w ->
                                         Word32 ->
                                         Word12 -> m ()
subsImmediate s rd@(R d) (R n) sh imm = instr $  (b64 rd `shift` 31)
                                             .|. 0x51000000
                                             .|. (s `shift` 29)
                                             .|. (sh `shift` 22)
                                             .|. (imm `shift` 10)
                                             .|. (n `shift` 5)
                                             .|. d

subsExtendedRegister :: (CodeMonad AArch64 m, SingI w) =>
                                                 Word1 ->
                                            Register w ->
                                            Register w ->
                                                 Word5 ->
                                                Extend ->
                                                 Word3 -> m ()
subsExtendedRegister s rd@(R d) (R n) m extend amount = instr $  (b64 rd `shift` 31)
                                                             .|. 0x6b200000
                                                             .|. (s `shift` 29)
                                                             .|. (m `shift` 16)
                                                             .|. (extendToEnc extend `shift` 13)
                                                             .|. (amount `shift` 10)
                                                             .|. (n `shift` 5)
                                                             .|. d

subsShiftedRegister :: (CodeMonad AArch64 m, SingI w) =>
                                                Word1 ->
                                           Register w ->
                                           Register w ->
                                           Register w ->
                                                Shift ->
                                                Word6 -> m ()
subsShiftedRegister s rd@(R d) (R n) (R m) sht imm6 = instr $  (b64 rd `shift` 31)
                                                           .|. 0x6b000000
                                                           .|. (s `shift` 29)
                                                           .|. (shiftToEnc sht `shift` 22)
                                                           .|. (m `shift` 16)
                                                           .|. (imm6 `shift` 10)
                                                           .|. (n `shift` 5)
                                                           .|. d

-- | C6.2.315 SUBS

-- | C6.2.317 SVC
svc :: CodeMonad AArch64 m => Word16 -> m ()
svc imm = instr $ 0xd4000001 .|. (fromIntegral imm `shift` 5)

-- | C6.2.333 UBFM

-- FIXME: should be true: b64 rd == bpN
ubfm :: (CodeMonad AArch64 m, SingI w) => Register w -> Register w -> BitPattern w -> m ()
ubfm rd@(R d) (R n) BitPattern { .. } = instr $  (b64 rd `shift` 31)
                                             .|. 0x53000000
                                             .|. (bpN    `shift` 22)
                                             .|. (bpImmr `shift` 16)
                                             .|. (bpImms `shift` 10)
                                             .|. (n      `shift`  5)
                                             .|. d

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
    imm <- $eitherAddContext' $ fitN 26 x
    let
        f w = (w .&. 0xfc000000) .|. imm
    return $ modifyWord32LE p f
mkRelocationAArch64 R_AARCH64_CONDBR19 p@(SectionOffset p') (SectionOffset s) a = do
    let
        x = (s + a - p') `shiftR` 2
    imm <- $eitherAddContext' $ fitN 19 x
    let
        f w = (w .&. 0xff00001f) .|. (imm `shift` 5)
    return $ modifyWord32LE p f
mkRelocationAArch64 R_AARCH64_ADR_PREL_LO21 p@(SectionOffset p') (SectionOffset s) a = do
    let
        x = (s + a - p')
    imm21 <- $eitherAddContext' $ fitN 21 x
    let
        immlo = imm21 .&. 3
        immhi = imm21 `shiftR` 2
        imm   = (immhi `shift` 5) .|. (immlo `shift` 29)
        f w = (w .&. 0x9f00001f) .|. imm
    return $ modifyWord32LE p f
mkRelocationAArch64 rl _ _ _ = $chainedError ("relocation is not implemented: " <> show rl)
