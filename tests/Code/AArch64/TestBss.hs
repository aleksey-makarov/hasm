{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Code.AArch64.TestBss (testBss) where

import Data.Word

import Asm.Asm
import Asm.AArch64

-- | syscalls
sysExit :: Word16
sysExit = 93

testBss :: CodeMonad AArch64 m => m ()
testBss = do

    movz x0 $ LSL0 0
    movz x8 $ LSL0 sysExit
    svc 0
