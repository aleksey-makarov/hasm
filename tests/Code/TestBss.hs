{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Code.TestBss (testBss) where

import Data.Word

import Asm.AsmAArch64

-- | syscalls
sysExit :: Word16
sysExit = 93

testBss :: CodeMonad m => m ()
testBss = do

    movz x0 $ LSL0 0
    movz x8 $ LSL0 sysExit
    svc 0
