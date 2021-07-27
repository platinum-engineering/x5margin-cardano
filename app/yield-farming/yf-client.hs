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

import           Control.Monad.Trans.Reader (runReaderT)
import           Data.UUID    hiding        (toString, fromString)
import           System.IO
import           Text.Read                  (readMaybe)
import qualified Data.Char                  as Char
import           Options.Applicative (execParser)
import           System.Exit (die)

import           Wallet.Emulator.Wallet  (walletPubKey, Wallet (..))
import           Plutus.V1.Ledger.Crypto (pubKeyHash)
import           ClientCLI
import           Client

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
        flip runReaderT (YfClientEnv ccliHost ccliPort uuid) $ case cmd of
            Deposit amt  -> deposit amt
            Withdraw amt -> withdraw amt
            Balance walId  -> walletBalance walId
            PublicKey -> walletPublicKey
            ContractBalance -> contractBalance
            PendingReward walId ->
                pendingReward $ pubKeyHash $ walletPubKey $ Wallet $ fromIntegral walId
            Harvest -> harvest
            UserStakes walId ->
                userStakes $ pubKeyHash $ walletPubKey $ Wallet $ fromIntegral walId
        go params uuid

    readCommand :: IO Command
    readCommand = do
        putStr "Enter command (publicKey, deposit amt, withdraw amt, balance walletId, contractBalance, pendingReward walletId, harvest, userStakes walletId): "
        s <- getLine
        maybe (putStrLn "Couldn't parse command" >> readCommand) return $ readMaybe (capitalized s)

capitalized :: String -> String
capitalized (h : t) = Char.toUpper h : t
capitalized [] = []