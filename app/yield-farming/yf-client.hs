{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}

module Main
    ( main
    ) where

import           Data.UUID    hiding        (toString, fromString)
import           System.IO
import           Text.Read                  (readMaybe)
import qualified Data.Char                  as Char
import           Options.Applicative (execParser)
import           System.Exit (die)

import ClientCLI
import Client

main :: IO ()
main = do
    params@ClientCLI{..} <- execParser clientOpts
    uuid <- case (ccliInstUUID, ccliWalletId) of
        (Nothing, Nothing) -> do
            die "Neither wallet id nor wallet instance id Passed"
        (Just iuuid, _) -> pure $ read iuuid
        (Nothing, Just walId) -> read <$> readFile ('W' : show walId ++ ".cid")

    hSetBuffering stdout NoBuffering
    putStrLn $ "Yield Farming contract instance id for Wallet " ++ show ccliWalletId ++ ": " ++ show uuid
    go params uuid
  where
    go :: ClientCLI -> UUID -> IO a
    go params@ClientCLI{..} uuid = do
        cmd <- readCommand
        case cmd of
            Deposit amt  -> deposit ccliHost ccliPort uuid amt
            Withdraw amt -> withdraw ccliHost ccliPort uuid amt
            Balance walId  -> walletBalance ccliHost ccliPort  uuid walId
            ContractBalance -> contractBalance ccliHost ccliPort uuid
            Harvest -> harvest ccliHost ccliPort uuid
        go params uuid

    readCommand :: IO Command
    readCommand = do
        putStr "Enter command (deposit amt, withdraw amt, balance walletId, contractBalance, harvest): "
        s <- getLine
        maybe (putStrLn "Couldn't parse command" >> readCommand) return $ readMaybe (capitalized s)

capitalized :: String -> String
capitalized (h : t) = Char.toUpper h : t
capitalized [] = []