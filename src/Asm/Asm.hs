{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- https://lisha.ufsc.br/teaching/os/exercise/where_is_my_variable.html

module Asm.Asm
    (
    -- * Data
      CodeState
    , CodeMonad
    , Symbol
    , KnownArch (..)
    , RelocationType

    -- * Assembler directives
    , label
    , labelExtern
    , ltorg
    , offset
    , emit
    , emitReloc
    , align
    , allocateBSS

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
import Data.Array.IArray
import Data.Bits
import Data.ByteString.Builder hiding (word8)
import qualified Data.ByteString.Builder as BSB
import Data.ByteString.Lazy as BSL
import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Foldable
import Data.Int
import Data.Kind
import Data.List (sortOn)
import Data.Singletons.Sigma
import Data.Singletons.TH
import Data.Word

import Asm.Data
import Asm.Relocation

class KnownArch a where
    data Instruction a :: Type

    instructionSize :: Num b => Instruction a -> b
    ltorgAlign :: Proxy a -> Int
    serializeInstruction :: Instruction a -> Builder
    mkRelocation ::
            MonadThrow m =>
        RelocationType a ->
           SectionOffset -> -- p (he address of the place being relocated)
           SectionOffset -> -- s (is the address of the symbol)
                   Int64 -> -- a (the addend for the relocation)
                             m RelocationMonad

type family RelocationType a = t | t -> a

type CodeMonad a m = (MonadThrow m, MonadState (CodeState a) m)
data Symbol = Symbol Int

--------------------------------------------------------------------------------
-- state

data TextChunk a
    = InstructionChunk
        { _icInstruction :: Instruction a
        -- , _icRelocation :: Maybe (LabelRelocation a)
        }
    | BuilderChunk
        { _bcLength :: Int
        , _bcData   :: Builder
        }

data SymbolTableItem
    = SymbolTableItemTxtUnallocated
        { stiTxtUN      :: Int
        , stiTxtUAlign  :: Int
        , stiTxtULength :: Int
        , stiTxtUData   :: Builder
        }
    | SymbolTableItemTxt
        { stiTxtN      :: Int
        , stiTxtName   :: Maybe String
        , stiTxtOffset :: SectionOffset
        }
    | SymbolTableItemBSSUnallocated
        { stiBSSUN      :: Int
        , stiBSSUAlign  :: Int
        , stiBSSULength :: Int
        }
    | SymbolTableItemBSS
        { -- stiBSSN      :: Int
        -- , stiBSSOffset :: Int64
        }

data RelocationTableItem a
    = RelocationTableItem
        { lrSymbol     :: Symbol
        , lrRelocation :: RelocationType a
        , lrAddress    :: SectionOffset
        }

data CodeState a
    = CodeState
        { textOffset            :: SectionOffset   -- Should always be aligned by instructionSize
        , textReversed          :: [TextChunk a]

        , symbolsNext           :: Int
        , symbolsReversed       :: [SymbolTableItem]

        , relocations           :: [RelocationTableItem a]
        }

codeStateInit :: CodeState a
codeStateInit = CodeState 0 [] 0 [] []

offset :: MonadState (CodeState a) m => m SectionOffset
offset = gets textOffset

--------------------------------------------------------------------------------
-- text

addRelocation :: (KnownArch a, CodeMonad a m) => RelocationTableItem a -> m ()
addRelocation c = modify f where
    f CodeState {..} =
        CodeState { relocations = c : relocations
                  , ..
                  }

emitChunk :: (KnownArch a, CodeMonad a m) => TextChunk a -> m SectionOffset
emitChunk c = state f where
    f CodeState {..} =
        ( textOffset
        , CodeState { textReversed = c : textReversed
                    , textOffset = l + textOffset
                    , ..
                    }
        )
    l = case c of
        InstructionChunk i -> instructionSize i
        BuilderChunk i _ -> fromIntegral i

emit :: (KnownArch a, CodeMonad a m) => Instruction a -> m ()
emit i = void $ emitChunk $ InstructionChunk i

emitReloc :: (KnownArch a, CodeMonad a m) => Instruction a -> Symbol -> RelocationType a -> m ()
emitReloc i l r = do
    o <- offset
    emit i
    addRelocation $ RelocationTableItem l r o

-- IMPORTANT: this can leave text array in unaligned state so
-- this should not be exported
emitBuilder :: (KnownArch a, CodeMonad a m) => Int -> Builder -> m SectionOffset
emitBuilder l bu | l < 0     = error "internal error: chunk length < 0"
                 | l == 0    = offset
                 | otherwise = emitChunk $ BuilderChunk l bu

-- IMPORTANT: this can leave text array in unaligned state so
-- this should not be exported
emitBuilderN :: (KnownArch a, CodeMonad a m) => Int -> m ()
emitBuilderN n = void $ emitBuilder n $ mconcat $ P.map BSB.word8 $ P.replicate n 0

-- IMPORTANT: this can leave text array in unaligned state so
-- this should not be exported
align' :: (KnownArch a, CodeMonad a m) => Int -> m ()
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
ltorg' :: (KnownArch a, CodeMonad a m) => m ()
ltorg' = do
    let
        f SymbolTableItemTxtUnallocated { .. } = do
            align' stiTxtUAlign
            SymbolTableItemTxt stiTxtUN Nothing <$> emitBuilder stiTxtULength stiTxtUData
        f x = return x
    symbols' <- gets symbolsReversed >>= mapM f
    let
        fm CodeState { .. } =
            CodeState
                { symbolsReversed = symbols'
                , ..
                }
    modify fm

ltorg :: forall a m . (KnownArch a, CodeMonad a m) => m ()
ltorg = ltorg' >> align' (ltorgAlign (Proxy @a))

align :: forall a m . (KnownArch a, CodeMonad a m) => Int -> m ()
align a | a < (ltorgAlign (Proxy @a)) = $chainedError "align is too small"
        | otherwise                   = align' a

allocateBSS :: MonadState (CodeState a) m => Int -> Int -> m Symbol
allocateBSS stiBSSUAlign stiBSSULength = state f where
    f CodeState {..} =
        ( Symbol symbolsNext
        , CodeState
            { symbolsNext = 1 + symbolsNext
                , symbolsReversed = SymbolTableItemBSSUnallocated
                     { stiBSSUN = symbolsNext
                     , ..
                     } : symbolsReversed
                , ..
                }
        )

--------------------------------------------------------------------------------
-- pool

emitPool' :: MonadState (CodeState a) m => Maybe String -> SectionOffset -> m Symbol
emitPool' stiTxtName stiTxtOffset = state f where
    f CodeState {..} =
        ( Symbol symbolsNext
        , CodeState
            { symbolsNext = symbolsNext + 1
            , symbolsReversed = SymbolTableItemTxt
                { stiTxtN = symbolsNext
                , ..
                } : symbolsReversed
            , ..
            }
        )

emitPool :: MonadState (CodeState a) m => Int -> Int -> Builder -> m Symbol
emitPool stiTxtUAlign stiTxtULength stiTxtUData = state f where
    f CodeState {..} =
        ( Symbol symbolsNext
        , CodeState
            { symbolsNext = symbolsNext + 1
            , symbolsReversed = SymbolTableItemTxtUnallocated
                { stiTxtUN = symbolsNext
                , ..
                } : symbolsReversed
            , ..
            }
        )

label :: MonadState (CodeState a) m => m Symbol
label = offset >>= emitPool' Nothing

labelExtern :: MonadState (CodeState a) m => String -> m Symbol
labelExtern name = offset >>= emitPool' (Just name)

--------------------------------------------------------------------------------
-- asm directives

ascii, asciiz:: MonadState (CodeState a) m => String -> m Symbol
ascii s = emitPool 1 l bu
    where
        bu = stringUtf8 s
        l = fromIntegral $ BSL.length $ toLazyByteString bu
asciiz s = ascii (s <> "\0")

word8, byte :: MonadState (CodeState a) m => Word8 -> m Symbol
word8 w = emitPool 1 1 $ BSB.word8 w
byte = word8

word16, short :: MonadState (CodeState a) m => Word16 -> m Symbol
word16 w = emitPool 2 2 $ BSB.word16LE w
short = word16

word32, word :: MonadState (CodeState a) m => Word32 -> m Symbol
word32 w = emitPool 4 4 $ BSB.word32LE w
word = word32

word64, long :: MonadState (CodeState a) m => Word64 -> m Symbol
word64 w = emitPool 8 8 $ BSB.word64LE w
long = word64

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

splitMapReverseM :: MonadThrow m => (a -> m (Maybe b)) -> [a] -> m ([a], [b])
splitMapReverseM f l = foldlM f' ([], []) l
    where
        f' (a, b) x = f'' <$> f x
            where
                f'' Nothing = (x : a, b)
                f'' (Just b') = (a, b' : b)

assemble :: forall a m . (MonadCatch m, KnownArch a) => StateT (CodeState a) m () -> m Elf
assemble m = do

    CodeState {..} <- execStateT (m >> ltorg') codeStateInit

    ElfCompositionState { .. } <- flip execStateT (elfCompositionStateInit @'ELFCLASS64) $ do

        ---------------------------------------------------------------------
        -- labels
        ---------------------------------------------------------------------

        -- let
        --     labelArray :: UArray Int Int64
        --     labelArray = array (0, P.length labelTable - 1) (P.map (bimap id getSectionOffset) labelTable)

        --     labelToSectionOffset :: Ref -> SectionOffset
        --     labelToSectionOffset (Ref i) = SectionOffset $ labelArray ! i

        ---------------------------------------------------------------------
        -- txt
        ---------------------------------------------------------------------

        let
            text = P.reverse textReversed
            symbols = P.reverse symbolsReversed

            symbolTab :: Array Int SymbolTableItem
            symbolTab = listArray (0, P.length symbols - 1) symbols

            getSymbolTableItem :: Symbol -> SymbolTableItem
            getSymbolTableItem (Symbol i) = symbolTab ! i

            fTxtReloc :: MonadThrow m' => RelocationTableItem a -> m' (Maybe RelocationMonad)
            fTxtReloc RelocationTableItem { .. } =
                case getSymbolTableItem lrSymbol of
                    SymbolTableItemTxt { .. } -> Just <$> mkRelocation @a lrRelocation lrAddress stiTxtOffset 0
                    _ -> return Nothing

        (_reloc, relocTxt) <- splitMapReverseM fTxtReloc relocations

        let
            fTxt :: TextChunk a -> Builder
            fTxt (InstructionChunk i) = serializeInstruction @a i
            fTxt (BuilderChunk _ bu) = bu

            txt = relocate (toLazyByteString $ mconcat $ P.map fTxt text) (fold relocTxt)

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
        -- bss
        ---------------------------------------------------------------------

        -- when (0 /= P.length bss) $ do
        --     bssSecN <- getNextSectionN
        --     addNewSection
        --         ElfSection
        --             { esName      = ".bss"
        --             , esType      = SHT_NOBITS
        --             , esFlags     = SHF_WRITE .|. SHF_ALLOC
        --             , esAddr      = 0
        --             , esAddrAlign = 1
        --             , esEntSize   = 0
        --             , esN         = bssSecN
        --             , esLink      = 0
        --             , esInfo      = 0
        --             , esData      = ElfSectionData empty
        --             }

        ---------------------------------------------------------------------
        -- symbols
        ---------------------------------------------------------------------

        let
            fSymbol :: SymbolTableItem -> ElfSymbolXX 'ELFCLASS64
            fSymbol SymbolTableItemTxtUnallocated {} = error "internal error: SymbolTableItemTxtUnallocated"
            fSymbol SymbolTableItemTxt { stiTxtName = Nothing, .. } =
                let
                    steName  = "$" ++ show stiTxtN ++ "@" ++ (show $ getSectionOffset stiTxtOffset)
                    steBind  = STB_Local
                    steType  = STT_NoType
                    steShNdx = textSecN
                    steValue = fromIntegral stiTxtOffset
                    steSize  = 0
                in
                    ElfSymbolXX{..}
            fSymbol SymbolTableItemTxt { stiTxtName = Just name, .. } =
                let
                    steName  = name
                    steBind  = STB_Global
                    steType  = STT_NoType
                    steShNdx = textSecN
                    steValue = fromIntegral stiTxtOffset
                    steSize  = 0
                in
                    ElfSymbolXX{..}
            fSymbol SymbolTableItemBSSUnallocated {} = error "internal error: SymbolTableItemBSSUnallocated"
            fSymbol SymbolTableItemBSS {} = undefined

            symbolTable = sortOn steBind $ fSymbol <$> symbols
            isLocal s = steBind s == STB_Local
            numLocal = fromIntegral $ P.length $ P.takeWhile isLocal symbolTable

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
                , esInfo      = numLocal + 1
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
