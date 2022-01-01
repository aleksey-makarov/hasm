{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

import Options.Applicative

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

main' :: Options -> IO ()
main' PrintType = putStrLn "PrintType"
main' PrintVersion = putStrLn "PrintVersion"
main' (GoLink inf outf) = putStrLn $ "GoLink " ++ inf ++ " " ++ outf

main :: IO ()
main = execParser opts >>= main'
