{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module Code.AArch64.HelloWorld (helloWorld) where

import Prelude as P

import Control.Monad.State
import Data.Word

import Asm.Asm
import Asm.AArch64

msg :: String
msg = "Hello World!\n"

-- | syscalls
sysExit, sysWrite :: Word16
sysWrite = 64
sysExit = 93

helloWorld :: (CodeMonad AArch64 m, MonadFix m) => m ()
helloWorld = mdo

    start <- label
    exportSymbol "_start" start
    movz x0 $ LSL0 1
    helloString <- ascii msg
    adr x1 helloString
    movz x2 $ LSL0 $ fromIntegral $ P.length msg
    movz x8 $ LSL0 sysWrite
    svc 0

    movz x0 $ LSL0 0
    movz x8 $ LSL0 sysExit
    svc 0
