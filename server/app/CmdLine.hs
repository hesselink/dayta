module CmdLine where

import Options.Applicative

data Opts = Opts
  { configFile :: FilePath
  } deriving (Show, Eq)

opts :: Parser Opts
opts = Opts
  <$> strOption
      (  short 'c'
      <> long "config"
      <> metavar "CONFIG_FILE"
      <> help "Location of the configuration file."
      <> showDefault
      <> value "dayta.yaml"
      )

optsWithInfo :: ParserInfo Opts
optsWithInfo = info (opts <**> helper)
  ( fullDesc
  <> progDesc "Run the Dayta server"
  <> header "dayta - record time series data"
  )

getOpts :: IO Opts
getOpts = execParser optsWithInfo
