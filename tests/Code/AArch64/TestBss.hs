{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module Code.AArch64.TestBss (testBss) where

import Prelude as P hiding (and)

import Control.Monad
import Control.Monad.Fix
import Data.Word

import Asm.Asm
import Asm.AArch64

-- | syscalls
sysExit :: Word16
sysExit = 93

charToInt :: (CodeMonad AArch64 m, MonadFix m) => m ()
charToInt = mdo
    and w0 w0 $ BitPattern 0 0 7 -- #0xff
    sub w1 w0 $ Immediate 0x61
    and w1 w1 $ BitPattern 0 0 7 -- #0xff
    cmp w1 $ Immediate 0x5
    bcond LS l
    sub w1 w0 $ Immediate 0x41
    sub w2 w0 $ Immediate 0x37
    and w1 w1 $ BitPattern 0 0 7 -- #0xff
    sub w0 w0 $ Immediate 0x30
    cmp w1 $ Immediate 0x5
    csel w0 w0 w2 HI
    ret x30
    l <- label
    sub w0 w0 $ Immediate 0x57
    ret x30

intToChar :: CodeMonad AArch64 m => m ()
intToChar = do

    and w1 w0 $ BitPattern 0 0 7 -- 0xff
    cmp w0    $ Immediate 0x9
    add w0 w1 $ Immediate 0x30
    add w1 w1 $ Immediate 0x57
    and w1 w1 $ BitPattern 0 0 7 -- 0xff
    and w0 w0 $ BitPattern 0 0 7 -- 0xff
    csel w0 w1 w0 HI
    ret x30

readUInt32 :: CodeMonad AArch64 m => Symbol -> m ()
readUInt32 charToIntSymbol = do
    stp x29 x30 $ PPreIndex sp (-48)
    movz w2 $ LSL0 0x0
    movSP x29 sp
    stp x19 x20 $ PSignedOffset sp 16
    mov x20 x0
    stp x21 x22 $ PSignedOffset sp 32
    mov x22 x1
    add x21 x0 $ Immediate 0x8
    l <- label
    ldrb w0 $ PostIndex x20 1
    lsl w19 w2 4
    bl charToIntSymbol
    add w2 w19 w0
    cmp x20 x21
    bcond NE l
    ldp x19 x20 $ PSignedOffset sp 16
    str w2 $ UnsignedOffset x22 0
    ldp x21 x22 $ PSignedOffset sp 32
    ldp x29 x30 $ PPostIndex sp 48
    ret x30
    nop

writeUInt32 :: CodeMonad AArch64 m => Symbol -> m ()
writeUInt32 _intToCharSymbol = do
    instr 0 -- stp x29, x30, [sp, #-48]!
    mov x29 sp
    instr 0 -- stp x19, x20, [sp, #16]
    mov x20 x1
    instr 0 -- mov w19 0x1c
    instr 0 -- str x21, [sp, #32]
    mov w21 w0
    nop
    instr 0 -- lsr w0, w21, w19
    instr 0 -- and w0 w0 0xf
    instr 0 -- bl  38 <int_to_char>
    instr 0 -- strb w0, [x20], #1
    instr 0 -- sub w19 w19 0x4
    instr 0 -- cmn w19, #0x4
    instr 0 -- b.ne c8 <write_uint32+0x20>  // b.any
    instr 0 -- ldp x19, x20, [sp, #16]
    instr 0 -- ldr x21, [sp, #32]
    instr 0 -- ldp x29, x30, [sp], #48
    ret x30

testBss :: (CodeMonad AArch64 m, MonadFix m) => m ()
testBss = mdo

    writeUInt32 int2c

    instr 0xffffffff
    instr 0x11111111
    instr 0x11111111
    instr 0x11111111
    instr 0x11111111

    readUInt32 c2int

    instr 0x11111111
    instr 0x11111111
    instr 0x11111111
    instr 0x11111111

    c2int <- label
    charToInt
    int2c <- label
    intToChar

    void $ labelExtern "_start"

    movz x0 $ LSL0 0
    movz x8 $ LSL0 sysExit
    svc 0
