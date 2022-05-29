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
    , ArchElfClass

    -- * Assembler directives
    , label
    , labelExtern
    , ltorg
    , offset
    , emit
    , emitReloc
    , align
    , Access (..)
    , Data (..)
    , Visibility (..)
    , allocate
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
import Data.Singletons.Sigma
import Data.Singletons.TH
import Data.Word

import Asm.Data
import Asm.Relocation

class (Integral (RelocationType a)) => KnownArch a where
    data Instruction a :: Type
    type RelocationType a = t | t -> a

    instructionSize :: Num b => Instruction a -> b
    serializeInstruction :: Instruction a -> Builder

    ltorgAlign :: Proxy a -> Int
    mkRelocation ::
            MonadThrow m =>
        RelocationType a ->
           SectionOffset -> -- p (he address of the place being relocated)
           SectionOffset -> -- s (is the address of the symbol)
                   Int64 -> -- a (the addend for the relocation)
                             m (RelocationMonad ())

type ArchElfClass :: Type -> ElfClass
type family ArchElfClass a = t | t -> a

type CodeMonad a m = (MonadThrow m, MonadState (CodeState a) m)
data Symbol
    = SymbolLocal Int
    | SymbolGlobal Int

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

data SymbolTableItem c
    = SymbolTableItemTxtUnallocated
        { stiTxtUAlign  :: Int
        , stiTxtULength :: Int
        , stiTxtUData   :: Builder
        }
    | SymbolTableItemTxt
        { stiTxtName   :: Maybe String
        , stiTxtOffset :: SectionOffset
        }
    | SymbolTableItemDataUnallocated
        { stiDataUAlignment  :: Int
        , stiDataUVisibility :: Visibility
        , stiDataUAccess     :: Access
        , stiDataUData       :: Data
        }
    | SymbolTableItemElfSymbol
        { stiElfSymbol :: ElfSymbolXX c
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

        , symbolsLocalNext      :: Int
        , symbolsLocalReversed  :: [SymbolTableItem (ArchElfClass a)]
        , symbolsNext           :: Int
        , symbolsReversed       :: [SymbolTableItem (ArchElfClass a)]

        , relocations           :: [RelocationTableItem a]
        }

codeStateInit :: CodeState a
codeStateInit = CodeState 0 [] 0 [] 0 [] []

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
            SymbolTableItemTxt Nothing <$> emitBuilder stiTxtULength stiTxtUData
        f x = return x
    symbols'      <- mapM f =<< gets symbolsReversed
    symbolsLocal' <- mapM f =<< gets symbolsLocalReversed
    let
        fm CodeState { .. } =
            CodeState
                { symbolsReversed = symbols'
                , symbolsLocalReversed = symbolsLocal'
                , ..
                }
    modify fm

ltorg :: forall a m . (KnownArch a, CodeMonad a m) => m ()
ltorg = ltorg' >> align' (ltorgAlign (Proxy @a))

align :: forall a m . (KnownArch a, CodeMonad a m) => Int -> m ()
align a | a < (ltorgAlign (Proxy @a)) = $chainedError "align is too small"
        | otherwise                   = align' a

data Visibility
    = Local
    | Global String
data Access
    = RO
    | RW
        deriving Eq
data Data
    = Uninitialized Int
    | Initialized ByteString
        deriving Eq

dataSize :: Data -> Int
dataSize (Uninitialized i) = i
dataSize (Initialized bs) = fromIntegral $ BSL.length bs

addSymbolGlobal :: MonadState (CodeState a) m => SymbolTableItem (ArchElfClass a) -> m Symbol
addSymbolGlobal newSymbolItem = state f where
    f CodeState {..} =
        ( SymbolGlobal symbolsNext
        , CodeState
            { symbolsNext = 1 + symbolsNext
            , symbolsReversed = newSymbolItem : symbolsReversed
            , ..
            }
        )

addSymbolLocal :: MonadState (CodeState a) m => SymbolTableItem (ArchElfClass a) -> m Symbol
addSymbolLocal newSymbolItem = state f where
    f CodeState {..} =
        ( SymbolLocal symbolsLocalNext
        , CodeState
            { symbolsLocalNext = 1 + symbolsLocalNext
            , symbolsLocalReversed = newSymbolItem : symbolsLocalReversed
            , ..
            }
        )

allocate :: MonadState (CodeState a) m => Visibility -> Access -> Int -> Data -> m Symbol
allocate stiDataUVisibility stiDataUAccess stiDataUAlignment stiDataUData =
    let
        s = SymbolTableItemDataUnallocated { .. }
    in
        case stiDataUVisibility of
            Local    -> addSymbolLocal  s
            Global _ -> addSymbolGlobal s

allocateBSS :: MonadState (CodeState a) m => Int -> Int -> m Symbol
allocateBSS alignment size = allocate Local RW alignment $ Uninitialized size

--------------------------------------------------------------------------------
-- pool

emitPool' :: MonadState (CodeState a) m => Maybe String -> SectionOffset -> m Symbol
emitPool' stiTxtName stiTxtOffset =
    let
        s = SymbolTableItemTxt { .. }
    in
        case stiTxtName of
            Nothing -> addSymbolLocal  s
            Just _  -> addSymbolGlobal s

emitPool :: MonadState (CodeState a) m => Int -> Int -> Builder -> m Symbol
emitPool stiTxtUAlign stiTxtULength stiTxtUData =
    addSymbolLocal $ SymbolTableItemTxtUnallocated { .. }

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

zeroIndexStringItem :: IsElfClass c => ElfSymbolXX c
zeroIndexStringItem = ElfSymbolXX "" 0 0 0 0 0

data ElfCompositionState c
    = ElfCompositionState
        { ecsNextSectionN :: ElfSectionIndex
        , ecsElfReversed  :: [ElfXX c]
        , ecsSymbols      :: Array Int (SymbolTableItem c)
        }

data SymbolAllocationInfo tag
    = SymbolAllocationInfo
        { saiTag      :: tag
        , saiAlignmet :: Int
        , saiSize     :: Int
        }

-----------------------------------------
-- move this to a separate layout module

-- FIXME: unify with the other align function (?)
align2 :: (MonadState Int m', MonadThrow m') => Int ->  m' ()
align2 0                       = return ()
align2 1                       = return ()
align2 a | a < 0               = $chainedError "negative align"
         | not (isPower2 a)    = $chainedError "align is not a power of 2"
         | otherwise           = modify $ \ o -> (o + a - 1) .&. complement (a - 1)

-- FIXME: optimize layout
layout :: MonadThrow m => [SymbolAllocationInfo tag] -> m [(tag, Int)]
layout sais =
    let
        f :: (MonadThrow m, MonadState Int m) => SymbolAllocationInfo tag -> m (tag, Int)
        f SymbolAllocationInfo { .. } = do
            align2 saiAlignmet
            position <- MS.get
            modify (+ saiSize)
            return (saiTag, position)
    in
        evalStateT (mapM f sais) 0

--
-----------------------------------------

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

splitMapReverseM :: Monad m => (a -> m (Maybe b)) -> [a] -> m ([a], [b])
splitMapReverseM f l = foldlM f' ([], []) l
    where
        f' (a, b) x = f'' <$> f x
            where
                f'' Nothing = (x : a, b)
                f'' (Just b') = (a, b' : b)

mkSymbolName :: Show n => String -> n -> String
mkSymbolName s n = "$" ++ s ++ "@" ++ show n -- FIXME: use hexadecimal

assemble :: forall a m . (MonadCatch m, KnownArch a, IsElfClass (ArchElfClass a)) =>
                                                        StateT (CodeState a) m () -> m Elf
assemble m = do

    CodeState {..} <- execStateT (m >> ltorg') codeStateInit

    let
        elfCompositionStateInit :: ElfCompositionState (ArchElfClass a)
        elfCompositionStateInit = ElfCompositionState 1 [h] symbolsArray
            where
                h = ElfHeader
                    { ehData       = ELFDATA2LSB -- FIXME: Wrong
                    , ehOSABI      = ELFOSABI_SYSV -- FIXME: ???
                    , ehABIVersion = 0
                    , ehType       = ET_REL
                    , ehMachine    = EM_AARCH64 -- FIXME: Wrong
                    , ehEntry      = 0
                    , ehFlags      = 0
                    }
                symbols = P.reverse symbolsLocalReversed ++ P.reverse symbolsReversed
                symbolsArray = listArray (0, P.length symbols - 1) symbols

    ElfCompositionState { .. } <- flip execStateT elfCompositionStateInit $ do

        ---------------------------------------------------------------------
        -- txt
        ---------------------------------------------------------------------

        let
            text = P.reverse textReversed

            symbolToSymbolTableIndex :: Symbol -> Int
            symbolToSymbolTableIndex (SymbolLocal i) = i
            symbolToSymbolTableIndex (SymbolGlobal i) = i + symbolsLocalNext

            getSymbolTableItem :: MonadState (ElfCompositionState (ArchElfClass a)) m' =>
                Symbol -> m' (SymbolTableItem (ArchElfClass a))
            getSymbolTableItem s = do
                table <- gets ecsSymbols
                return $ table ! symbolToSymbolTableIndex s

            fTxtReloc :: (MonadThrow m', MonadState (ElfCompositionState (ArchElfClass a)) m') =>
                RelocationTableItem a -> m' (Maybe (RelocationMonad ()))
            fTxtReloc RelocationTableItem { .. } = do
                s <- getSymbolTableItem lrSymbol
                case s of
                    SymbolTableItemTxt { .. } -> Just <$> mkRelocation @a lrRelocation lrAddress stiTxtOffset 0
                    _ -> return Nothing

        (reloc, relocTxt) <- splitMapReverseM fTxtReloc relocations

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
        -- data
        ---------------------------------------------------------------------

        -- dataSecN   <- getNextSectionN
        -- addNewSection
        --     ElfSection
        --         { esName      = ".data"
        --         , esType      = SHT_PROGBITS
        --         , esFlags     = SHF_WRITE .|. SHF_ALLOC
        --         , esAddr      = 0
        --         , esAddrAlign = 1
        --         , esEntSize   = 0
        --         , esN         = dataSecN
        --         , esLink      = 0
        --         , esInfo      = 0
        --         , esData      = ElfSectionData empty
        --         }

        ---------------------------------------------------------------------
        -- bss
        ---------------------------------------------------------------------

        let
            findSymbolsFunc :: [SymbolAllocationInfo Int] -> (Int, SymbolTableItem c) -> [SymbolAllocationInfo Int]
            findSymbolsFunc sais ( n
                                 , i@SymbolTableItemDataUnallocated { stiDataUAccess = RW
                                                                    , stiDataUData = Uninitialized _
                                                                    , ..
                                                                    }
                                 ) =
                let
                    newSAI = SymbolAllocationInfo
                        { saiTag      = n
                        , saiAlignmet = stiDataUAlignment
                        , saiSize     = dataSize $ stiDataUData i
                        }
                in
                    newSAI : sais
            findSymbolsFunc sais _ = sais

            findSymbols :: MonadState (ElfCompositionState elfClass) m' => m' [SymbolAllocationInfo Int]
            findSymbols = do
                table <- gets ecsSymbols
                return $ P.foldl findSymbolsFunc [] $ P.zip [0 ..] $ elems table

            modifySymbols :: MonadState (ElfCompositionState c) m' =>
                (Array Int (SymbolTableItem c) -> Array Int (SymbolTableItem c)) -> m' ()
            modifySymbols f = modify f'
                where
                    f' ElfCompositionState { .. } =
                        ElfCompositionState
                            { ecsSymbols = f ecsSymbols
                            , ..
                            }

        bss <- layout =<< (P.reverse <$> findSymbols)

        when (0 /= P.length bss) $ do
            bssSecN <- getNextSectionN
            addNewSection
                ElfSection
                    { esName      = ".bss"
                    , esType      = SHT_NOBITS
                    , esFlags     = SHF_WRITE .|. SHF_ALLOC
                    , esAddr      = 0
                    , esAddrAlign = 8 -- FIXME: arch-specific?
                    , esEntSize   = 0
                    , esN         = bssSecN
                    , esLink      = 0
                    , esInfo      = 0
                    , esData      = ElfSectionData empty
                    }

            let
                accumFunc :: IsElfClass c => SymbolTableItem c -> Int -> SymbolTableItem c
                accumFunc SymbolTableItemDataUnallocated { .. } n =
                    SymbolTableItemElfSymbol $ ElfSymbolXX { .. }
                        where
                            (steName, steBind) = case stiDataUVisibility of
                                Local       -> (mkSymbolName "bss" n, STB_Local)
                                Global name -> (name, STB_Global)
                            steType  = STT_Object
                            steShNdx = bssSecN
                            steValue = fromIntegral n
                            steSize  = fromIntegral $ dataSize stiDataUData
                accumFunc _ _ = error "internal error: FIXME"

                modifySymbolsFunction :: IsElfClass c => Array Int (SymbolTableItem c) -> Array Int (SymbolTableItem c)
                modifySymbolsFunction a = accum accumFunc a bss

            modifySymbols modifySymbolsFunction

        ---------------------------------------------------------------------
        -- symbols
        ---------------------------------------------------------------------

        let
            fSymbol :: IsElfClass c => SymbolTableItem c -> ElfSymbolXX c
            -- This should have been allocated in ltorg'
            fSymbol SymbolTableItemTxtUnallocated {} = error "internal error: SymbolTableItemTxtUnallocated"
            fSymbol SymbolTableItemTxt { stiTxtName = Nothing, .. } =
                let
                    steName  = mkSymbolName "text" $ getSectionOffset stiTxtOffset
                    steBind  = STB_Local
                    steType  = STT_NoType
                    steShNdx = textSecN
                    steValue = fromIntegral stiTxtOffset
                    steSize  = 0
                in
                    ElfSymbolXX { .. }
            fSymbol SymbolTableItemTxt { stiTxtName = Just name, .. } =
                let
                    steName  = name
                    steBind  = STB_Global
                    steType  = STT_NoType
                    steShNdx = textSecN
                    steValue = fromIntegral stiTxtOffset
                    steSize  = 0
                in
                    ElfSymbolXX { .. }
            fSymbol SymbolTableItemElfSymbol { .. } = stiElfSymbol
            fSymbol SymbolTableItemDataUnallocated {} = error "internal error: SymbolTableItemDataUnallocated"

            numLocal = fromIntegral symbolsLocalNext

        symbolTable <- (fmap fSymbol . elems) <$> gets ecsSymbols

        -- FIXME: ELFDATA2LSB -- should be arch dependent
        (symbolTableData, stringTableData) <- serializeSymbolTable ELFDATA2LSB (zeroIndexStringItem : symbolTable)

        symtabSecN   <- getNextSectionN
        strtabSecN   <- getNextSectionN

        addNewSection
            ElfSection
                { esName      = ".symtab"
                , esType      = SHT_SYMTAB
                , esFlags     = 0
                , esAddr      = 0
                , esAddrAlign = 8
                , esEntSize   = symbolTableEntrySize $ fromSing $ sing @(ArchElfClass a)
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
        -- .rela.text
        ---------------------------------------------------------------------

        when (0 /= P.length reloc) $ do

            let
                f :: RelocationTableItem a -> RelaXX (ArchElfClass a)
                f RelocationTableItem { .. } =
                    let
                        relaOffset = fromIntegral $ getSectionOffset lrAddress
                        relaSym    = 1 + (fromIntegral $ symbolToSymbolTableIndex lrSymbol)
                        relaType   = fromIntegral lrRelocation
                        relaAddend = 0
                    in
                        RelaXX { .. }
                reloc' = serializeBList ELFDATA2LSB $ fmap f reloc -- FIXME: ELFDATA2LSB: Wrong

            relocSecN   <- getNextSectionN
            addNewSection
                ElfSection
                    { esName      = ".rela.text"
                    , esType      = SHT_RELA
                    , esFlags     = SHF_EXT 64 -- FIXME (SHF_INFO_LINK This section headers sh_info field holds a section header table index) https://docs.oracle.com/cd/E23824_01/html/819-0690/chapter6-94076.html#chapter6-10675
                    , esAddr      = 0
                    , esAddrAlign = 8
                    , esEntSize   = 0x18 -- FIXME
                    , esN         = relocSecN
                    , esLink      = fromIntegral symtabSecN
                    , esInfo      = fromIntegral textSecN
                    , esData      = ElfSectionData reloc'
                    }

        ---------------------------------------------------------------------
        -- section table
        ---------------------------------------------------------------------

        addNewSection ElfSectionTable

    return $ sing :&: (ElfList $ P.reverse ecsElfReversed)
