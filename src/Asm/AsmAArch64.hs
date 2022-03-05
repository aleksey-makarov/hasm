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

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Asm.AsmAArch64
    (
    -- * Data
      CodeState
    , CodeMonad
    , Register
    , Label

    -- * Instructions
    , instr
    , adr
    , b
    , MovData (..)
    , movk
    , movn
    , movz
    , ldr
    , svc

    -- * Relocations

    -- * Assembler directives
    , ascii, asciiz
    , byte, short, word, long
    , word8, word16, word32, word64

    , label
    , ltorg
    , exportSymbol

    -- * Registers
    , x0, x1, x2, x8
    , w0, w1

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

$(singletons [d| data RegisterWidth = X | W |])

newtype TextAddress  = TextAddress  { getTextAddress  :: Int64 }
    deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)

newtype Instruction = Instruction { getInstruction :: Word32 }
    deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)

newtype Label = Label Int

instructionSize :: Num b => b
instructionSize = 4

type CodeMonad m = (MonadThrow m, MonadState CodeState m)

--------------------------------------------------------------------------------
-- registers

type Register :: RegisterWidth -> Type
newtype Register c = R Word32

x0, x1, x2, x8 :: Register 'X
x0 = R 0
x1 = R 1
x2 = R 2
x8 = R 8

w0, w1 :: Register 'W
w0 = R 0
w1 = R 1

--------------------------------------------------------------------------------
-- state

data LabelRelocation
    = LabelRelocation
        { lrLabel      :: Label
        , lrRelocation :: ElfRelocationType_AARCH64
        , lrAddress    :: TextAddress
        }

data TextChunk
    = InstructionChunk
        { icInstruction :: Instruction
        , icRelocation :: Maybe LabelRelocation
        }
    | BuilderChunk
        { bcLength :: Int
        , bcData   :: Builder
        }

data LabelTableUnallocatedEntry
    = LabelTableUnallocatedEntry
        { ltueIndex  :: Int
        , ltueAlign  :: Int
        , ltueLength :: Int
        , ltueData   :: Builder
        }

data CodeState
    = CodeState
        { textOffset            :: TextAddress   -- Should always be aligned by instructionSize
        , textReversed          :: [TextChunk]
        , symbolsRefersed       :: [(String, Label)]
        , labelTableNext        :: Int
        , labelTableUnallocated :: [LabelTableUnallocatedEntry]
        , labelTable            :: [(Int, TextAddress)]
        }

codeStateInit :: CodeState
codeStateInit = CodeState 0 [] [] 0 [] []

offset :: MonadState CodeState m => m TextAddress
offset = gets textOffset

--------------------------------------------------------------------------------
-- text

emit' :: CodeMonad m => TextChunk -> m TextAddress
emit' c = state f where
    f CodeState {..} =
        ( textOffset
        , CodeState { textReversed = c : textReversed
                    , textOffset = l + textOffset
                    , ..
                    }
        )
    l = case c of
        InstructionChunk _ _ -> instructionSize
        BuilderChunk i _ -> fromIntegral i

emit :: CodeMonad m => Instruction -> Maybe LabelRelocation -> m TextAddress
emit i lr = emit' $ InstructionChunk i lr

instrReloc :: CodeMonad m => Word32 -> Label -> ElfRelocationType_AARCH64 -> m ()
instrReloc w l r = do
    a <- offset
    void $ emit (Instruction w) (Just $ LabelRelocation l r a)

instr :: CodeMonad m => Word32 -> m ()
instr w = void $ emit (Instruction w) Nothing

-- IMPORTANT: this can leave text array in unaligned state so
-- this should not be exported
emitBuilder :: CodeMonad m => Int -> Builder -> m TextAddress
emitBuilder l bu | l < 0     = error "internal error: chunk length < 0"
                 | l == 0    = offset
                 | otherwise = emit' $ BuilderChunk l bu

-- IMPORTANT: this can leave text array in unaligned state so
-- this should not be exported
emitBuilderN :: CodeMonad m => Int -> m ()
emitBuilderN n = void $ emitBuilder n $ mconcat $ P.map BSB.word8 $ P.replicate n 0

-- IMPORTANT: this can leave text array in unaligned state so
-- this should not be exported
align' :: CodeMonad m => Int -> m ()
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
ltorg' :: CodeMonad m => m ()
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

ltorg :: CodeMonad m => m ()
ltorg = ltorg' >> align' instructionSize -- FIXME: should it be aligned at 8 to be shure it can be jumped to (?)

isPower2 :: (Bits i, Num i) => i -> Bool
isPower2 n = n .&. (n - 1) == 0

align :: CodeMonad m => Int -> m ()
align a | a < instructionSize = $chainedError "align is too small"
        | otherwise           = align' a

--------------------------------------------------------------------------------
-- pool

emitPool' :: MonadState CodeState m => TextAddress -> m Label
emitPool' a = state f where
    f CodeState {..} =
        ( Label labelTableNext
        , CodeState { labelTableNext = labelTableNext + 1
                    , labelTable = (labelTableNext, a) : labelTable
                    , ..
                    }
        )

emitPool :: MonadState CodeState m => Int -> Int -> Builder -> m Label
emitPool ltueAlign ltueLength ltueData = state f where
    f CodeState {..} = let ltueIndex = labelTableNext in
        ( Label labelTableNext
        , CodeState { labelTableNext = labelTableNext + 1
                    , labelTableUnallocated = (LabelTableUnallocatedEntry { .. }) : labelTableUnallocated
                    , ..
                    }
        )

label :: MonadState CodeState m => m Label
label = offset >>= emitPool'

--------------------------------------------------------------------------------
-- instructions

b64 :: forall w . SingI w => Register w -> Word32
b64 _ = case sing @ w of
    SX -> 1
    SW -> 0

type Word9  = Word16
type Word21 = Word32
type Word26 = Word32

mask :: (Num b, Bits b, Ord b) => Int -> b
mask n = (1 `shift` n) - 1

fitN :: Int -> Int64 -> Maybe Word32
fitN bitN w =
    if (if w >= 0 then h == 0 else h == m) -- FIXME: wrong! (???) what about the bit at (bitN - 1)???
        then Just $ fromIntegral (w .&. m)
        else Nothing
    where
        m = mask bitN
        h = w .&. complement m

fixWord :: Integral a => Int -> a -> Word32
fixWord bitN v = mask bitN .&. fromIntegral v

-- | C6.2.10 ADR
adr_ :: Register 'X -> Word21 -> Word32
adr_ (R n) imm21 = 0x10000000 .|. imm .|. n
    where
        imm21' = fixWord 21 imm21 -- FIXME: Word21 should keep verified integer
        immlo = imm21' .&. 3
        immhi = imm21' `shiftR` 2
        imm   = (immhi `shift` 5) .|. (immlo `shift` 29)

class ArgADR a where
    adr :: CodeMonad m => Register 'X -> a -> m ()
instance ArgADR Word21 where
    adr r w = instr $ adr_ r w
instance ArgADR Label where
    adr r l = instrReloc (adr_ r 0) l R_AARCH64_ADR_PREL_LO21

-- offsetToImm21 :: MonadThrow m => TextAddress -> m Word21
-- offsetToImm21 (TextAddress o)
--   | not $ isBitN 21 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral (o .&. mask 21)

-- | C6.2.26 B
b_ :: Word26 -> Word32
b_ imm26 = 0x14000000 .|. imm26'
    where
        imm26' = fixWord 26 imm26 -- FIXME: Word26 should keep verified integer

class ArgB a where
    b :: CodeMonad m => a -> m ()
instance ArgB Word26 where
    b imm26 = instr $ b_ imm26
instance ArgB Label where
    b l = instrReloc (b_ 0) l R_AARCH64_JUMP26

-- offsetToImm26 :: MonadThrow m => TextAddress -> m Word26
-- offsetToImm26 (TextAddress o)
--   | o .&. 0x3 /= 0    = $chainedError $ "offset is not aligned: " ++ show o
--   | not $ isBitN 28 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral ((o `shiftR` 2) .&. mask 26)

-- | C6.2.190 MOVК
data MovData = LSL0  Word16
             | LSL16 Word16
             | LSL32 Word16
             | LSL48 Word16

movDataToInstrBits :: MovData -> Word32
movDataToInstrBits d = s `shift` 21 .|. (fromIntegral w `shift` 5)
    where
        (s, w) = case d of
            (LSL0  w') -> (0, w')
            (LSL16 w') -> (1, w')
            (LSL32 w') -> (2, w')
            (LSL48 w') -> (3, w')

movk :: (CodeMonad m, SingI w) => Register w -> MovData -> m ()
movk r@(R n) d = instr $ (b64 r `shift` 31)
                      .|. 0x72800000
                      .|. movDataToInstrBits d
                      .|. n

-- | C6.2.191 MOVN
movn :: (CodeMonad m, SingI w) => Register w -> MovData -> m ()
movn r@(R n) d = instr $ (b64 r `shift` 31)
                      .|. 0x12800000
                      .|. movDataToInstrBits d
                      .|. n

-- | C6.2.192 MOVZ
movz :: (CodeMonad m, SingI w) => Register w -> MovData -> m ()
movz r@(R n) d = instr $ (b64 r `shift` 31)
                      .|. 0x52800000
                      .|. movDataToInstrBits d
                      .|. n

-- | C6.2.132 LDR (literal)
ldr :: (CodeMonad m, SingI w) => Register w -> Word9 -> m ()
ldr r@(R n) imm9 = instr $ (b64 r `shift` 30)
                        .|. 0x18000000
                        .|. (imm9' `shift` 5)
                        .|. n
    where
        imm9' = fixWord 9 imm9 -- FIXME: Word9 should keep verified integer

-- offsetToImm9 :: MonadThrow m => TextAddress -> m Word9
-- offsetToImm9 (TextAddress o)
--   | o .&. 0x3 /= 0    = $chainedError $ "offset is not aligned: " ++ show o
--   | not $ isBitN 11 o = $chainedError "offset is too big"
--   | otherwise         = return $ fromIntegral ((o `shiftR` 2) .&. mask 9)


-- | C6.2.317 SVC
svc ::CodeMonad m => Word16 -> m ()
svc imm = instr $ 0xd4000001 .|. (fromIntegral imm `shift` 5)

--------------------------------------------------------------------------------
-- asm directives

ascii, asciiz:: MonadState CodeState m => String -> m Label
ascii s = emitPool 1 l bu
    where
        bu = stringUtf8 s
        l = fromIntegral $ BSL.length $ toLazyByteString bu
asciiz s = ascii (s <> "\0")

word8, byte :: MonadState CodeState m => Word8 -> m Label
word8 w = emitPool 1 1 $ BSB.word8 w
byte = word8

word16, short :: MonadState CodeState m => Word16 -> m Label
word16 w = emitPool 2 2 $ BSB.word16LE w
short = word16

word32, word :: MonadState CodeState m => Word32 -> m Label
word32 w = emitPool 4 4 $ BSB.word32LE w
word = word32

word64, long :: MonadState CodeState m => Word64 -> m Label
word64 w = emitPool 8 8 $ BSB.word64LE w
long = word64

exportSymbol :: MonadState CodeState m => String -> Label -> m ()
exportSymbol s r = modify f where
    f CodeState {..} =
        CodeState { symbolsRefersed = (s, r) : symbolsRefersed
                  , ..
                  }

--------------------------------------------------------------------------------
-- relocation

relocate ::      MonadThrow m =>
    ElfRelocationType_AARCH64 ->
                  Instruction ->
                  TextAddress ->
                  TextAddress ->
                        Int64 -> m Instruction
relocate R_AARCH64_JUMP26 (Instruction w) (TextAddress p) (TextAddress s) a = do
    let
        x = (s + a - p) `shiftR` 2
    imm <- $maybeAddContext "imm does not fit" $ fitN 26 x
    return $ Instruction $ (w .&. 0xfc000000) .|. imm
relocate R_AARCH64_ADR_PREL_LO21 (Instruction w) (TextAddress p) (TextAddress s) a = do
    let
        x = (s + a - p)
    imm21 <- $maybeAddContext "imm does not fit" $ fitN 21 x
    let
        immlo = imm21 .&. 3
        immhi = imm21 `shiftR` 2
        imm   = (immhi `shift` 5) .|. (immlo `shift` 29)
    return $ Instruction $ (w .&. 0x9f00001f) .|. imm
relocate rl _ _ _ _ = $chainedError ("relocation is not implemented: " <> show rl)

--------------------------------------------------------------------------------
-- assembler

zeroIndexStringItem :: ElfSymbolXX 'ELFCLASS64
zeroIndexStringItem = ElfSymbolXX "" 0 0 0 0 0

assemble :: forall m . MonadCatch m => StateT CodeState m () -> m Elf
assemble m = do

    CodeState {..} <- execStateT (m >> ltorg') codeStateInit

    let
        -- labels

        labelArray :: UArray Int Int64
        labelArray = array (0, P.length labelTable - 1) (P.map (bimap id getTextAddress) labelTable)

        labelToTextAddress :: Label -> TextAddress
        labelToTextAddress (Label i) = TextAddress $ labelArray ! i

        -- txt

        text = P.reverse textReversed

        fTxt' :: MonadThrow m => Instruction -> Maybe LabelRelocation -> m Instruction
        fTxt' i Nothing                         = return i
        fTxt' i (Just (LabelRelocation { .. })) = relocate lrRelocation i lrAddress (labelToTextAddress lrLabel) 0

        fTxt :: MonadThrow m => TextChunk -> m Builder
        fTxt (InstructionChunk i rl) = word32LE <$> getInstruction <$> fTxt' i rl
        fTxt (BuilderChunk _ bu) = return bu

    txt <- toLazyByteString <$> mconcat <$> mapM fTxt text

    flip evalStateT 1 $ do

        let
            getNextSectionN = state $ \ n -> (n, n + 1)

        textSecN     <- getNextSectionN
        shstrtabSecN <- getNextSectionN
        strtabSecN   <- getNextSectionN
        symtabSecN   <- getNextSectionN

        -- symbols

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

        -- resolve symbolTable

        (symbolTableData, stringTableData) <- serializeSymbolTable ELFDATA2LSB (zeroIndexStringItem : symbolTable)

        return $ SELFCLASS64 :&: ElfList
            [ ElfHeader
                { ehData       = ELFDATA2LSB
                , ehOSABI      = ELFOSABI_SYSV
                , ehABIVersion = 0
                , ehType       = ET_REL
                , ehMachine    = EM_AARCH64
                , ehEntry      = 0
                , ehFlags      = 0
                }
            , ElfSection
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
            , ElfSection
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
            , ElfSection
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
            , ElfSection
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
            , ElfSectionTable
            ]
