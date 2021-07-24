{-# LANGUAGE DerivingStrategies #-}

module ClientCLI where

import Prelude
import Options.Applicative

data ClientCLI = ClientCLI
  { ccliWalletId :: Maybe Integer
  , ccliHost     :: String
  , ccliPort     :: Int
  , ccliInstUUID :: Maybe String
  } deriving stock Show

clientCliParser :: Parser ClientCLI
clientCliParser = ClientCLI
      <$> optional (option auto
          (   long "wallet-id"
           <> short 'w'
           <> metavar "INT"
           <> help "Wallet id to interact with"))
      <*> strOption
          (   long "host"
           <> short 'h'
           <> help "Host of Yield Farming server"
           <> value "localhost")
      <*> option auto
          (   long "port"
           <> short 'p'
           <> showDefault
           <> value 8080
           <> metavar "INT"
           <> help "Port of Yield Farming server" )
      <*> optional (strOption
          (   long "uuid"
           <> short 'u'
           <> help "UUID of contract instance"))

clientOpts :: ParserInfo ClientCLI
clientOpts =
    info (clientCliParser <**> helper)
    ( fullDesc
    <> progDesc "Client for Yield Farming contract allowing to deposit and withdraw tokens"
    <> header "yf-client - a client for yf-server" )