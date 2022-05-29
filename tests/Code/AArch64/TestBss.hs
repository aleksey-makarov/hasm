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

testBss :: (CodeMonad AArch64 m, MonadFix m) => m ()
testBss = mdo

    -------------------------------------------------------
    charToInt <- label
    -------------------------------------------------------

    and w0 w0 $ BitPattern 0 0 7 -- #0xff
    sub w1 w0 $ Immediate 0x61
    and w1 w1 $ BitPattern 0 0 7 -- #0xff
    cmp w1 $ Immediate 0x5
    bcond LS l1
    sub w1 w0 $ Immediate 0x41
    sub w2 w0 $ Immediate 0x37
    and w1 w1 $ BitPattern 0 0 7 -- #0xff
    sub w0 w0 $ Immediate 0x30
    cmp w1 $ Immediate 0x5
    csel w0 w0 w2 HI
    ret x30
    l1 <- label
    sub w0 w0 $ Immediate 0x57
    ret x30

    -------------------------------------------------------
    intToChar <- label
    -------------------------------------------------------

    and w1 w0 $ BitPattern 0 0 7 -- 0xff
    cmp w0    $ Immediate 0x9
    add w0 w1 $ Immediate 0x30
    add w1 w1 $ Immediate 0x57
    and w1 w1 $ BitPattern 0 0 7 -- 0xff
    and w0 w0 $ BitPattern 0 0 7 -- 0xff
    csel w0 w1 w0 HI
    ret x30

    -------------------------------------------------------
    readUInt32 <- label
    -------------------------------------------------------

    stp x29 x30 $ PPreIndex sp (-48)
    movz w2 $ LSL0 0
    movsp x29 sp
    stp x19 x20 $ PSignedOffset sp 16
    mov x20 x0
    stp x21 x22 $ PSignedOffset sp 32
    mov x22 x1
    add x21 x0 $ Immediate 0x8
    l2 <- label
    ldrb w0 $ PostIndex x20 1
    lsl w19 w2 4
    bl charToInt
    add w2 w19 w0
    cmp x20 x21
    bcond NE l2
    ldp x19 x20 $ PSignedOffset sp 16
    str w2 $ UnsignedOffset x22 0
    ldp x21 x22 $ PSignedOffset sp 32
    ldp x29 x30 $ PPostIndex sp 48
    ret x30
    nop

    -------------------------------------------------------
    writeUInt32 <- label
    -------------------------------------------------------

    stp x29 x30 $ PPreIndex sp (-48)
    movsp x29 sp
    stp x19 x20 $ PSignedOffset sp 16
    mov x20 x1
    movz w19 $ LSL0 0x1c
    str x21 $ UnsignedOffset sp 32
    mov w21 w0
    nop
    l <- label
    lsr w0 w21 w19
    and w0 w0 $ BitPattern 0 0 3 -- 0xf
    bl intToChar
    strb w0 $ PostIndex x20 1
    sub w19 w19 $ Immediate 0x4
    cmn w19 $ Immediate 0x4
    bcond NE l
    ldp x19 x20 $ PSignedOffset sp 16
    ldr x21 $ UnsignedOffset sp 32
    ldp x29 x30 $ PPostIndex sp 48
    ret x30

    -------------------------------------------------------
    mainx <- label
    -------------------------------------------------------

    stp x29 x30 $ PPreIndex sp (-32)
    movz w2 $ LSL0 0x11
    movz w0 $ LSL0 0x0
    movsp x29 sp
    str x19 $ UnsignedOffset sp 16
    adrp x19 (0 :: Word32) -- FIXME: relocation here
    add x19 x19 $ Immediate 0 -- FIXME: relocation here
    mov x1 x19
    bl readSyscall
    add x1 x19 $ Immediate 0x14
    mov x0 x19
    bl readUInt32
    add x1 x19 $ Immediate 0x18
    add x0 x19 $ Immediate 0x9
    bl readUInt32
    ldp w2 w0 $ PSignedOffset x19 20
    mov x1 x19
    add w0 w2 w0
    bl writeUInt32
    mov x1 x19
    movz w2 $ LSL0 0x8
    movz w0 $ LSL0 0x1
    bl writeSyscall
    movz w0 $ LSL0 0x0
    ldr x19 $ UnsignedOffset sp 16
    ldp x29 x30 $ PPostIndex sp 32
    ret x30

    -------------------------------------------------------
    void $ labelExtern "_start"
    -------------------------------------------------------

    let
        stackSize = 256

    _something <- allocateBSS 8 256
    stack <- allocateBSS 8 stackSize

    adrp x19 stack
    add x19 x19 $ LO12 stack

    bl mainx
    movz x0 $ LSL0 0
    movz x8 $ LSL0 sysExit
    svc 0

    -------------------------------------------------------
    writeSyscall <- label
    -------------------------------------------------------

    ret x30

    -------------------------------------------------------
    readSyscall <- label
    -------------------------------------------------------

    ret x30
