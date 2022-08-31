{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.Catch
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Elf
import Data.Singletons
import Data.Version
import Options.Applicative
import System.Posix.Files

import Paths_hasm

data Options
  = GoLink
    { optFrom :: [String]
    , optTo   :: String
    }
  | PrintType
  | PrintVersion

optsGoLink :: Parser Options
optsGoLink = do
  optTo   <- argument str (metavar "OUTPUT_FILE")
  optFrom <- some $ argument str (metavar "INPUT_FILES...")
  pure GoLink {..}

optsPrintType :: Parser Options
optsPrintType = flag' PrintType (short 't' <> long "type"  <> help "Print dhall type of configuration file")

optsPrintVersion :: Parser Options
optsPrintVersion = flag' PrintVersion (short 'v' <> long "version"  <> help "Print version")

opts :: ParserInfo Options
opts = info (optsGoLink <|> optsPrintType <|> optsPrintVersion <**> helper)
  (  fullDesc
  <> progDesc "ELF object file linker"
  <> header "hld - linker"
  )

ld :: Sing a -> [ElfListXX a] -> ElfListXX a
ld = undefined

ld' :: MonadThrow m => [Elf] -> m Elf
ld' (e : _) = return e -- FIXME:
ld' [] = error "Internal error, should not happen (`some` was used in parser)"

main :: IO ()
main = execParser opts >>= main'

main' :: Options -> IO ()

main' PrintVersion = putStrLn $ showVersion version

main' PrintType = putStrLn "Not implemented"

main' GoLink { .. } = do
  mapM (\ x -> readFileStrict x >>= parseElf) optFrom >>= ld' >>= serializeElf >>= BSL.writeFile optTo
  makeFileExecutable optTo

makeFileExecutable :: FilePath -> IO ()
makeFileExecutable path = do
    m <- fileMode <$> getFileStatus path
    setFileMode path $ m .|. ownerExecuteMode

-- | Read the file strictly but return lazy bytestring
readFileStrict :: FilePath -> IO BSL.ByteString
readFileStrict path = BSL.fromStrict <$> BS.readFile path
