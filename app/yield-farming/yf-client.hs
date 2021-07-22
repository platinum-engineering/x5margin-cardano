{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class      (MonadIO (..))
import Data.Proxy                  (Proxy (..))
import Data.Text                   (pack)
import Data.UUID            hiding (toString, fromString)
import Data.Aeson (encode)
import Network.HTTP.Req
import System.Environment          (getArgs)
import System.IO
import Text.Read                   (readMaybe)
import Data.List
import qualified Data.Char       as Char
import qualified Data.ByteString.Lazy.Char8 as B

import Plutus.V1.Ledger.Value (Value, flattenValue, toString)

import qualified Platinum.Contracts.YieldFarming.Env as YF
import qualified Platinum.Contracts.YieldFarming.OffChain as YF

main :: IO ()
main = do
    [i :: Int] <- map read <$> getArgs
    uuid       <- read <$> readFile ('W' : show i ++ ".cid")
    hSetBuffering stdout NoBuffering
    putStrLn $ "Yield Farming contract instance id for Wallet " ++ show i ++ ": " ++ show uuid
    go uuid
  where
    go :: UUID -> IO a
    go uuid = do
        cmd <- readCommand
        case cmd of
            Deposit amt  -> deposit uuid amt
            Withdraw amt -> withdraw uuid amt
            Funds walId -> walletFunds uuid walId
        go uuid

    readCommand :: IO Command
    readCommand = do
        putStr "Enter command (deposit amt, withdraw amt, funds walletId): "
        s <- getLine
        maybe (putStrLn "Couldn't parse command" >> readCommand) return $ readMaybe (capitalized s)

-- Wallet API https://github.com/input-output-hk/plutus/blob/master/plutus-pab/src/Cardano/Wallet/API.hs
data Command
    = Deposit Integer
    | Withdraw Integer
    | Funds Integer
    deriving (Show, Read, Eq, Ord)

deposit :: UUID -> Integer -> IO ()
deposit uuid amt = handle h $ runReq defaultHttpConfig $ do
    let lov = amt * 1_000_000
    liftIO $ putStrLn $ B.unpack $ encode $ YF.AssetClassTransferParams YF.adaAssetClass lov
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "deposit")
        (ReqBodyJson $ YF.AssetClassTransferParams YF.adaAssetClass lov)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)

    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request to deposit " ++ show amt ++ " ADA sent"
        else "After deposit request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in deposit"
        threadDelay 1_000_000 >> deposit uuid amt

withdraw :: UUID -> Integer -> IO ()
withdraw uuid amt = handle h $ runReq defaultHttpConfig $ do
    let lov = amt * 1_000_000
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "withdraw")
        (ReqBodyJson $ YF.AssetClassTransferParams YF.adaAssetClass lov)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Request to withdraw " ++ show amt ++ " ADA sent"
        else "After withdraw request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in withdraw"
        threadDelay 1_000_000 >> withdraw uuid amt

walletFunds :: UUID -> Integer -> IO ()
walletFunds uuid walletId = handle h $ runReq defaultHttpConfig $ do
    v <- req
        GET
        (http "127.0.0.1" /: "wallet"  /: pack (show walletId) /: "total-funds")
        NoReqBody
        (Proxy :: Proxy (JsonResponse Value))
        (port 8080)
    let prettyValue =
            intercalate ", " .
            map (\(_, t, am) -> (if t == "" then "ADA" else toString t) ++ ": " ++ show am) .
            flattenValue
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Wallet " ++ show walletId ++ " funds: " ++ prettyValue (responseBody v)
        else "After wallet funds request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in wallet funds"
        threadDelay 1_000_000 >> walletFunds uuid walletId

capitalized :: String -> String
capitalized (h : t) = Char.toUpper h : map Char.toLower t
capitalized [] = []