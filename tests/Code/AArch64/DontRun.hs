{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MonoLocalBinds #-}

module Code.AArch64.DontRun (dontRun) where

import Prelude as P

import Asm.Asm
import Asm.AArch64

dontRun :: CodeMonad AArch64 m => m ()
dontRun = do

    label >>= exportSymbol "_start"

    _a <- ascii "ascii"
    _az <- asciiz "asciiz"
    _w8 <- word8 0xa5
    --
    instr 0xabcdef01
    instr 0xabcdef02
    instr 0xabcdef03
    ltorg
    --
    _w8''' <- word8 0xa5
    _w64 <- word64 0xffeeddccbbaa9964
    _w32 <- word32 0xffeedd32
    _w16 <- word16 0xff17
    _w8'' <- word8 0xa5
    _w16 <- word16 0xff16
    --
    instr 0xabcdef04
    instr 0xabcdef05
    instr 0xabcdef06
    ltorg
    --
    _w8' <- word8 0xa5
    instr 0xabcdef07
