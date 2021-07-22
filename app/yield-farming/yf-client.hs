{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ViewPatterns        #-}

module Main
    ( main
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (pack)
import           Data.UUID    hiding        (toString, fromString)
import qualified Data.Aeson               as A
import           Network.HTTP.Req
import           System.Environment         (getArgs)
import           System.IO
import           Text.Read                  (readMaybe)
import           Data.List
import qualified Data.Char                  as Char
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Aeson.Encode.Pretty (encodePretty)

import           Plutus.PAB.Webserver.Types (ContractInstanceClientState (..))
import           Plutus.V1.Ledger.Value     (Value, flattenValue, toString)
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))

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
            Funds walId  -> walletBalance uuid walId
            ContractBalance -> contractBalance uuid
        go uuid

    readCommand :: IO Command
    readCommand = do
        putStr "Enter command (deposit amt, withdraw amt, funds walletId, contractBalance): "
        s <- getLine
        maybe (putStrLn "Couldn't parse command" >> readCommand) return $ readMaybe (capitalized s)

-- Wallet API https://github.com/input-output-hk/plutus/blob/master/plutus-pab/src/Cardano/Wallet/API.hs
data Command
    = Deposit Integer
    | Withdraw Integer
    | Funds Integer
    | ContractBalance
    deriving (Show, Read, Eq, Ord)

baseUrlInstance :: Url 'Http
baseUrlInstance = http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance"

baseUrlWallet :: Url 'Http
baseUrlWallet = http "127.0.0.1" /: "wallet"

deposit :: UUID -> Integer -> IO ()
deposit uuid amt = handle h $ runReq defaultHttpConfig $ do
    let lov = amt * 1_000_000
    -- liftIO $ putStrLn $ B.unpack $ encode $ YF.AssetClassTransferParams YF.adaAssetClass lov
    initSt <- liftIO $ getStatus uuid
    v <- req
            POST
            (baseUrlInstance /: pack (show uuid) /: "endpoint" /: "deposit")
            (ReqBodyJson $ YF.AssetClassTransferParams YF.adaAssetClass lov)
            (Proxy :: Proxy (JsonResponse ()))
            (port 8080)

    liftIO $ if responseStatusCode v == 200 then do
        putStrLn $ "Request to deposit " ++ show amt ++ " ADA sent. Waiting for result"
        printResult uuid initSt
    else
        putStrLn $ "After deposit request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in deposit"
        threadDelay 1_000_000 >> deposit uuid amt

withdraw :: UUID -> Integer -> IO ()
withdraw uuid amt = handle h $ runReq defaultHttpConfig $ do
    let lov = amt * 1_000_000
    initSt <- liftIO $ getStatus uuid
    v <- req
            POST
            (baseUrlInstance /: pack (show uuid) /: "endpoint" /: "withdraw")
            (ReqBodyJson $ YF.AssetClassTransferParams YF.adaAssetClass lov)
            (Proxy :: Proxy (JsonResponse ()))
            (port 8080)
    liftIO $ if responseStatusCode v == 200 then do
        putStrLn $ "Request to withdraw " ++ show amt ++ " ADA sent. Waiting for result"
        printResult uuid initSt
    else
        putStrLn $ "After withdraw request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in withdraw"
        threadDelay 1_000_000 >> withdraw uuid amt

contractBalance :: UUID -> IO ()
contractBalance uuid = handle h $ runReq defaultHttpConfig $ do
    initSt <- liftIO $ getStatus uuid
    v <- req
            POST
            (baseUrlInstance /: pack (show uuid) /: "endpoint" /: "scriptBalance")
            (ReqBodyJson ())
            (Proxy :: Proxy (JsonResponse ()))
            (port 8080)

    liftIO $ if responseStatusCode v == 200 then do
        putStrLn $ "Contract balance requested. Waiting for result"
        printResult uuid initSt
    else
        putStrLn $ "After deposit request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in contract balance"
        threadDelay 1_000_000 >> contractBalance uuid

walletBalance :: UUID -> Integer -> IO ()
walletBalance uuid walletId = handle h $ runReq defaultHttpConfig $ do
    v <- req
            GET
            (baseUrlWallet /: pack (show walletId) /: "total-funds")
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
        putStrLn $ show e <> " happened in walletBalance"
        threadDelay 1_000_000 >> walletBalance uuid walletId

type Status = ContractInstanceClientState A.Value

getStatus :: UUID -> IO Status
getStatus uuid = runReq defaultHttpConfig $ do
    v <-
        req
            GET
            (baseUrlInstance /: pack (show uuid) /: "status")
            NoReqBody
            (Proxy :: Proxy (JsonResponse Status))
            (port 8080)
    pure $ responseBody v

waitForResult :: UUID -> Status -> IO Status
waitForResult uuid initSt' = do
    let initSt = cicCurrentState initSt'
    cur' <- getStatus uuid
    let cur = cicCurrentState cur'

    if (logs cur == logs initSt
        && err cur == err initSt
        && observableState cur == observableState initSt) then do
        threadDelay 1_000_000
        waitForResult uuid initSt'
    else
        pure cur'

printResult :: UUID -> Status -> IO ()
printResult uuid initSt = do
    result <- waitForResult uuid initSt
    putStrLn "Result: "
    putStrLn $ B.unpack $ encodePretty $ observableState . cicCurrentState $ result

capitalized :: String -> String
capitalized (h : t) = Char.toUpper h : t
capitalized [] = []