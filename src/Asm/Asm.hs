{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Asm.Asm
    (
    -- * Data
      CodeState
    , CodeMonad
    , Label
    , KnownArch (..)

    -- * Assembler directives
    , label
    , ltorg
    , exportSymbol
    , offset
    , emit
    , emitReloc
    , align

    , ascii, asciiz
    , byte, short, word, long
    , word8, word16, word32, word64

     -- * Functions
    , assemble
    ) where

import Prelude as P

import Control.Exception.ContextTH
import Control.Monad.Catch hiding (mask)
import Control.Monad.State as MS
import Data.Array.Unboxed
import Data.Bifunctor
import Data.Bits
import Data.ByteString.Builder hiding (word8)
import qualified Data.ByteString.Builder as BSB
import Data.ByteString.Lazy as BSL
-- Data.ByteString.Lazy.Char8 as BSLC
import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Int
import Data.Kind
-- import Data.List (sortOn)
import Data.Singletons.Sigma
import Data.Singletons.TH
import Data.Word

import Asm.Data

class KnownArch a where
    data Instruction a :: Type
    instructionSize :: Num b => Instruction a -> b
    ltorgAlign :: Proxy a -> Int
    type RelocationType a :: Type
    relocate ::      MonadThrow m =>
                 RelocationType a ->
                    Instruction a ->
                      TextAddress ->
                      TextAddress ->
                            Int64 -> m (Instruction a)
    serializeInstruction :: Instruction a -> Builder

type CodeMonad a m = (KnownArch a, MonadThrow m, MonadState (CodeState a) m)
newtype Label = Label Int

--------------------------------------------------------------------------------
-- state

data LabelRelocation a
    = LabelRelocation
        { lrLabel      :: Label
        , lrRelocation :: RelocationType a
        , lrAddress    :: TextAddress
        }

data TextChunk a
    = InstructionChunk
        { _icInstruction :: Instruction a
        , _icRelocation :: Maybe (LabelRelocation a)
        }
    | BuilderChunk
        { _bcLength :: Int
        , _bcData   :: Builder
        }

data LabelTableUnallocatedEntry
    = LabelTableUnallocatedEntry
        { ltueIndex  :: Int
        , ltueAlign  :: Int
        , ltueLength :: Int
        , ltueData   :: Builder
        }

data CodeState a
    = CodeState
        { textOffset            :: TextAddress   -- Should always be aligned by instructionSize
        , textReversed          :: [TextChunk a]
        , symbolsRefersed       :: [(String, Label)]
        , labelTableNext        :: Int
        , labelTableUnallocated :: [LabelTableUnallocatedEntry]
        , labelTable            :: [(Int, TextAddress)]
        }

codeStateInit :: CodeState a
codeStateInit = CodeState 0 [] [] 0 [] []

offset :: MonadState (CodeState a) m => m TextAddress
offset = gets textOffset

--------------------------------------------------------------------------------
-- text

emitChunk :: CodeMonad a m => TextChunk a -> m TextAddress
emitChunk c = state f where
    f CodeState {..} =
        ( textOffset
        , CodeState { textReversed = c : textReversed
                    , textOffset = l + textOffset
                    , ..
                    }
        )
    l = case c of
        InstructionChunk i _ -> instructionSize i
        BuilderChunk i _ -> fromIntegral i

emit' :: CodeMonad a m => Instruction a -> Maybe (LabelRelocation a) -> m TextAddress
emit' i lr = emitChunk $ InstructionChunk i lr

emit :: CodeMonad a m => Instruction a -> m ()
emit i = void $ emit' i Nothing

emitReloc :: CodeMonad a m => Instruction a -> Label -> RelocationType a -> m ()
emitReloc i l r = do
    o <- offset
    void $ emit' i (Just $ LabelRelocation l r o)

-- IMPORTANT: this can leave text array in unaligned state so
-- this should not be exported
emitBuilder :: CodeMonad a m => Int -> Builder -> m TextAddress
emitBuilder l bu | l < 0     = error "internal error: chunk length < 0"
                 | l == 0    = offset
                 | otherwise = emitChunk $ BuilderChunk l bu

-- IMPORTANT: this can leave text array in unaligned state so
-- this should not be exported
emitBuilderN :: CodeMonad a m => Int -> m ()
emitBuilderN n = void $ emitBuilder n $ mconcat $ P.map BSB.word8 $ P.replicate n 0

-- IMPORTANT: this can leave text array in unaligned state so
-- this should not be exported
align' :: CodeMonad a m => Int -> m ()
align' 0                       = return ()
align' 1                       = return ()
align' a | a < 0               = $chainedError "negative align"
         | not (isPower2 a)    = $chainedError "align is not a power of 2"
         | otherwise           = do
    o <- fromIntegral <$> offset -- all operations in Int's
    let
        n = (o + a - 1) .&. complement (a - 1)
        l = n - o
    emitBuilderN l

-- FIXME: Optimize the order of allocations
ltorg' :: CodeMonad a m => m ()
ltorg' = do
    let
        f LabelTableUnallocatedEntry { .. } = do
            align' ltueAlign
            (ltueIndex, ) <$> emitBuilder ltueLength ltueData
    lt <- gets labelTableUnallocated
    lt' <- mapM f lt
    let
        fm CodeState { .. } =
            CodeState
                { labelTableUnallocated = []
                , labelTable = lt' ++ labelTable
                , ..
                }
    modify fm

ltorg :: forall a m . CodeMonad a m => m ()
ltorg = ltorg' >> align' (ltorgAlign (Proxy @a))

align :: forall a m . CodeMonad a m => Int -> m ()
align a | a < (ltorgAlign (Proxy @a)) = $chainedError "align is too small"
        | otherwise                   = align' a

--------------------------------------------------------------------------------
-- pool

emitPool' :: MonadState (CodeState a) m => TextAddress -> m Label
emitPool' a = state f where
    f CodeState {..} =
        ( Label labelTableNext
        , CodeState { labelTableNext = labelTableNext + 1
                    , labelTable = (labelTableNext, a) : labelTable
                    , ..
                    }
        )

emitPool :: MonadState (CodeState a) m => Int -> Int -> Builder -> m Label
emitPool ltueAlign ltueLength ltueData = state f where
    f CodeState {..} = let ltueIndex = labelTableNext in
        ( Label labelTableNext
        , CodeState { labelTableNext = labelTableNext + 1
                    , labelTableUnallocated = (LabelTableUnallocatedEntry { .. }) : labelTableUnallocated
                    , ..
                    }
        )

label :: MonadState (CodeState a) m => m Label
label = offset >>= emitPool'

--------------------------------------------------------------------------------
-- asm directives

ascii, asciiz:: MonadState (CodeState a) m => String -> m Label
ascii s = emitPool 1 l bu
    where
        bu = stringUtf8 s
        l = fromIntegral $ BSL.length $ toLazyByteString bu
asciiz s = ascii (s <> "\0")

word8, byte :: MonadState (CodeState a) m => Word8 -> m Label
word8 w = emitPool 1 1 $ BSB.word8 w
byte = word8

word16, short :: MonadState (CodeState a) m => Word16 -> m Label
word16 w = emitPool 2 2 $ BSB.word16LE w
short = word16

word32, word :: MonadState (CodeState a) m => Word32 -> m Label
word32 w = emitPool 4 4 $ BSB.word32LE w
word = word32

word64, long :: MonadState (CodeState a) m => Word64 -> m Label
word64 w = emitPool 8 8 $ BSB.word64LE w
long = word64

exportSymbol :: MonadState (CodeState a) m => String -> Label -> m ()
exportSymbol s r = modify f where
    f CodeState {..} =
        CodeState { symbolsRefersed = (s, r) : symbolsRefersed
                  , ..
                  }

--------------------------------------------------------------------------------
-- assembler

zeroIndexStringItem :: ElfSymbolXX 'ELFCLASS64
zeroIndexStringItem = ElfSymbolXX "" 0 0 0 0 0

data ElfCompositionState a
    = ElfCompositionState
        { ecsNextSectionN :: ElfSectionIndex
        , ecsElfReversed  :: [ElfXX a]
        }

elfCompositionStateInit :: IsElfClass a => ElfCompositionState a
elfCompositionStateInit = ElfCompositionState 1 [h]
    where
        h = ElfHeader
            { ehData       = ELFDATA2LSB
            , ehOSABI      = ELFOSABI_SYSV
            , ehABIVersion = 0
            , ehType       = ET_REL
            , ehMachine    = EM_AARCH64
            , ehEntry      = 0
            , ehFlags      = 0
            }

getNextSectionN :: Monad m => StateT (ElfCompositionState a) m ElfSectionIndex
getNextSectionN = state f
    where
        f (ElfCompositionState { .. }) =
            ( ecsNextSectionN
            , ElfCompositionState
                { ecsNextSectionN = ecsNextSectionN + 1
                , ..
                }
            )

addNewSection :: Monad m => ElfXX a -> StateT (ElfCompositionState a) m ()
addNewSection e = modify f
    where
        f (ElfCompositionState { .. }) =
            ElfCompositionState
                { ecsElfReversed = e : ecsElfReversed
                , ..
                }

assemble :: forall a m . (MonadCatch m, KnownArch a) => StateT (CodeState a) m () -> m Elf
assemble m = do

    CodeState {..} <- execStateT (m >> ltorg') codeStateInit

    ElfCompositionState { .. } <- flip execStateT (elfCompositionStateInit @'ELFCLASS64) $ do

        ---------------------------------------------------------------------
        -- labels
        ---------------------------------------------------------------------

        let
            labelArray :: UArray Int Int64
            labelArray = array (0, P.length labelTable - 1) (P.map (bimap id getTextAddress) labelTable)

            labelToTextAddress :: Label -> TextAddress
            labelToTextAddress (Label i) = TextAddress $ labelArray ! i

        ---------------------------------------------------------------------
        -- txt
        ---------------------------------------------------------------------

        let
            text = P.reverse textReversed

            fTxt' :: MonadThrow m' => Instruction a -> Maybe (LabelRelocation a) -> m' (Instruction a)
            fTxt' i Nothing                         = return i
            fTxt' i (Just (LabelRelocation { .. })) = relocate lrRelocation i lrAddress (labelToTextAddress lrLabel) 0

            fTxt :: MonadThrow m' => TextChunk a -> m' Builder
            fTxt (InstructionChunk i rl) = serializeInstruction <$> fTxt' i rl
            fTxt (BuilderChunk _ bu) = return bu

        txt <- toLazyByteString <$> mconcat <$> mapM fTxt text
        textSecN <- getNextSectionN
        addNewSection
            ElfSection
                { esName      = ".text"
                , esType      = SHT_PROGBITS
                , esFlags     = SHF_EXECINSTR .|. SHF_ALLOC
                , esAddr      = 0
                , esAddrAlign = 8
                , esEntSize   = 0
                , esN         = textSecN
                , esLink      = 0
                , esInfo      = 0
                , esData      = ElfSectionData txt
                }

        ---------------------------------------------------------------------
        -- symbols section
        ---------------------------------------------------------------------

        shstrtabSecN <- getNextSectionN
        addNewSection
            ElfSection
                { esName      = ".shstrtab"
                , esType      = SHT_STRTAB
                , esFlags     = 0
                , esAddr      = 0
                , esAddrAlign = 1
                , esEntSize   = 0
                , esN         = shstrtabSecN
                , esLink      = 0
                , esInfo      = 0
                , esData      = ElfSectionDataStringTable
                }

        ---------------------------------------------------------------------
        -- symbols
        ---------------------------------------------------------------------

        let
            symbols = P.reverse symbolsRefersed

            fSymbol :: (String, Label) -> ElfSymbolXX 'ELFCLASS64
            fSymbol (s, l) =
                let
                    steName  = s
                    steBind  = STB_Global
                    steType  = STT_NoType
                    steShNdx = textSecN
                    steValue = fromIntegral $ getTextAddress $ labelToTextAddress l
                    steSize  = 0
                in
                    ElfSymbolXX{..}

            symbolTable = fSymbol <$> symbols

        (symbolTableData, stringTableData) <- serializeSymbolTable ELFDATA2LSB (zeroIndexStringItem : symbolTable)

        strtabSecN   <- getNextSectionN
        symtabSecN   <- getNextSectionN

        addNewSection
            ElfSection
                { esName      = ".symtab"
                , esType      = SHT_SYMTAB
                , esFlags     = 0
                , esAddr      = 0
                , esAddrAlign = 8
                , esEntSize   = symbolTableEntrySize ELFCLASS64
                , esN         = symtabSecN
                , esLink      = fromIntegral strtabSecN
                , esInfo      = 1
                , esData      = ElfSectionData symbolTableData
                }

        addNewSection
            ElfSection
                { esName      = ".strtab"
                , esType      = SHT_STRTAB
                , esFlags     = 0
                , esAddr      = 0
                , esAddrAlign = 1
                , esEntSize   = 0
                , esN         = strtabSecN
                , esLink      = 0
                , esInfo      = 0
                , esData      = ElfSectionData stringTableData
                }

        ---------------------------------------------------------------------
        -- section table
        ---------------------------------------------------------------------

        addNewSection ElfSectionTable

    return $ SELFCLASS64 :&: (ElfList $ P.reverse ecsElfReversed)
