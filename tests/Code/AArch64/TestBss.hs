{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Code.AArch64.TestBss (testBss) where

import Prelude as P hiding (and)

import Data.Word

import Asm.Asm
import Asm.AArch64

-- | syscalls
sysExit :: Word16
sysExit = 93

intToChar :: CodeMonad AArch64 m => m ()
intToChar = do

    and w1 w0 0xf
    cmp w0    0x9
    add w0 w1 0x30
    add w1 w1 0x57
    and w1 w1 0xff
    and w0 w0 0xff
    csel w0 w1 w0 HI -- hi = pmore
    ret x30

testBss :: CodeMonad AArch64 m => m ()
testBss = do

    movz x0 $ LSL0 0
    movz x8 $ LSL0 sysExit
    svc 0
