{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

import Options.Applicative
import GitHash

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

main' PrintVersion = putStrLn $ concat [giTag gi, " (", giBranch gi, "@", giHash gi, ")", dirty]
  where
    dirty = if giDirty gi then " dirty" else ""
    gi = $$tGitInfoCwd

main' PrintType = undefined

main' (GoLink inf outf) = putStrLn $ "GoLink " ++ inf ++ " " ++ outf
