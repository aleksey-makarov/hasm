{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Asm.LdDummy (ldDummy) where

import Control.Monad.Catch
import Data.Bits

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Control.Exception.ContextTH

data MachineConfig (a :: ElfClass)
    = MachineConfig
        { mcAddress :: WordXX a -- ^ Virtual address of the executable segment
        , mcAlign   :: WordXX a -- ^ Required alignment of the executable segment
                                --   in physical memory (depends on max page size)
        }

getMachineConfig :: (SingElfClassI a, MonadThrow m) => ElfMachine -> m (MachineConfig a)
getMachineConfig EM_AARCH64 = return $ MachineConfig 0x400000 0x10000
getMachineConfig EM_X86_64  = return $ MachineConfig 0x400000 0x1000
getMachineConfig _          = $chainedError "could not find machine config for this arch"

ld' :: forall a m . (MonadThrow m, SingElfClassI a) => ElfListXX a -> m (ElfListXX a)
ld' es = do

    txtSection <- elfFindSectionByName es ".text"
    txtSectionData <- case txtSection of
        ElfSection { esData = ElfSectionData textData } -> return textData
        _ -> $chainedError "could not find correct \".text\" section"

    -- FIXME: no need for lazy match here.  It's a compiler bug
    ~(ElfHeader { .. }) <- elfFindHeader es
    MachineConfig { .. } <- getMachineConfig ehMachine
    return $
        ElfSegment
            { epType       = PT_LOAD
            , epFlags      = PF_X .|. PF_R
            , epVirtAddr   = mcAddress
            , epPhysAddr   = mcAddress
            , epAddMemSize = 0
            , epAlign      = mcAlign
            , epData       =
                ElfHeader
                    { ehType  = ET_EXEC
                    , ehEntry = mcAddress + headerSize (fromSingElfClass $ singElfClass @a)
                    , ..
                    }
                ~: ElfRawData
                    { edData = txtSectionData
                    }
                ~: ElfListNull
            }
        ~: ElfSegmentTable
        ~: ElfListNull

-- | Simple static linker
ldDummy :: MonadThrow m => Elf -> m Elf
ldDummy (Elf c l) = Elf c <$> withSingElfClassI c ld' l
