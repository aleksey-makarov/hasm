{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Elf
import Data.Version
import Options.Applicative
import System.Posix.Files

import Paths_hasm

import Asm.LdDummy

data Options
  = GoLink
    { optFrom :: String
    , optTo   :: String
    }
  | PrintVersion

optsGoLink :: Parser Options
optsGoLink = do
  optFrom <- argument str (metavar "INPUT_FILE")
  optTo   <- argument str (metavar "OUTPUT_FILE")
  pure GoLink {..}

optsPrintVersion :: Parser Options
optsPrintVersion = flag' PrintVersion (short 'v' <> long "version"  <> help "Print version")

opts :: ParserInfo Options
opts = info (optsGoLink <|> optsPrintVersion <**> helper)
  (  fullDesc
  <> progDesc "ELF object file linker"
  <> header "hlddummy - linker"
  )

main :: IO ()
main = execParser opts >>= main'

main' :: Options -> IO ()

main' PrintVersion = putStrLn $ showVersion version

main' (GoLink inf outf) = do
  readFileStrict inf >>= parseElf >>= ldDummy >>= serializeElf >>= BSL.writeFile outf
  makeFileExecutable outf

makeFileExecutable :: FilePath -> IO ()
makeFileExecutable path = do
    m <- fileMode <$> getFileStatus path
    setFileMode path $ m .|. ownerExecuteMode

-- | Read the file strictly but return lazy bytestring
readFileStrict :: FilePath -> IO BSL.ByteString
readFileStrict path = BSL.fromStrict <$> BS.readFile path
