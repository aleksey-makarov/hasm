{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}

module Code.AArch64.ForwardLabel (forwardLabel) where

import Prelude as P

import Control.Monad.Fix
import Data.Word

import Asm.Asm
import Asm.AArch64

ok :: String
ok = "ok\n"

bad :: String
bad = "bad\n"

-- | syscalls
sysExit, sysWrite :: Word16
sysWrite = 64
sysExit = 93

forwardLabel :: (CodeMonad AArch64 m, MonadFix m) => m ()
forwardLabel = mdo

    label >>= exportSymbol "_start"

    lOk <- ascii ok
    lBad <- ascii bad

    movz x0 $ LSL0 1

    adr x1 lOk
    movz x2 $ LSL0 $ fromIntegral $ P.length ok

    b skipBad

    adr x1 lBad
    movz x2 $ LSL0 $ fromIntegral $ P.length bad

    skipBad <- label

    movz x8 $ LSL0 sysWrite
    svc 0

    movz x0 $ LSL0 0
    movz x8 $ LSL0 sysExit
    svc 0
