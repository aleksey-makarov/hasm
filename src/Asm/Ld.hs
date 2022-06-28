{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Asm.Ld (ld) where

import Control.Monad.Catch
import Data.Bits
import Data.Singletons
import Data.Singletons.Sigma

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

getMachineConfig :: (IsElfClass a, MonadThrow m) => ElfMachine -> m (MachineConfig a)
getMachineConfig EM_AARCH64 = return $ MachineConfig 0x400000 0x10000
getMachineConfig EM_X86_64  = return $ MachineConfig 0x400000 0x1000
getMachineConfig _          = $chainedError "could not find machine config for this arch"

ld' :: forall a m . (MonadThrow m, IsElfClass a) => ElfList a -> m (ElfList a)
ld' (ElfList es) = do

    txtSection <- elfFindSectionByName es ".text"
    txtSectionData <- case txtSection of
        ElfSection { esData = ElfSectionData textData } -> return textData
        _ -> $chainedError "could not find correct \".text\" section"

    header <- elfFindHeader es
    case header of
        ElfHeader { .. } -> do
            MachineConfig { .. } <- getMachineConfig ehMachine
            return $ ElfList
                [ ElfSegment
                    { epType       = PT_LOAD
                    , epFlags      = PF_X .|. PF_R
                    , epVirtAddr   = mcAddress
                    , epPhysAddr   = mcAddress
                    , epAddMemSize = 0
                    , epAlign      = mcAlign
                    , epData       =
                        [ ElfHeader
                            { ehType  = ET_EXEC
                            , ehEntry = mcAddress + headerSize (fromSing $ sing @a)
                            , ..
                            }
                        , ElfRawData
                            { edData = txtSectionData
                            }
                        ]
                    }
                , ElfSegmentTable
                ]
        _ -> $chainedError "could not find ELF header"

-- | Simple static linker
ld :: MonadThrow m => Elf -> m Elf
ld (c :&: l) = (c :&:) <$> withElfClass c ld' l
