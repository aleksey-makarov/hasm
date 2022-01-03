{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Elf
import Data.Version
import Options.Applicative

import Paths_hasm

import Asm.DummyLd

data Options
  = GoLink
    { optFrom :: String
    , optTo   :: String
    }
  | PrintType
  | PrintVersion

optsGoLink :: Parser Options
optsGoLink = do
  optFrom <- argument str (metavar "INPUT_FILE")
  optTo   <- argument str (metavar "OUTPUT_FILE")
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

main :: IO ()
main = execParser opts >>= main'

main' :: Options -> IO ()

main' PrintVersion = putStrLn $ showVersion version

main' PrintType = undefined

main' (GoLink inf outf) = readFileStrict inf >>= parseElf >>= dummyLd >>= serializeElf >>= BSL.writeFile outf

-- | Read the file strictly but return lazy bytestring
readFileStrict :: FilePath -> IO BSL.ByteString
readFileStrict path = BSL.fromStrict <$> BS.readFile path
