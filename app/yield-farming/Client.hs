{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}

module Client
     ( deposit
     , withdraw
     , contractBalance
     , walletBalance
     , harvest
     , Command (..)
     ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (pack)
import           Data.UUID    hiding        (toString, fromString)
import qualified Data.Aeson               as A
import           Network.HTTP.Req
import           Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Plutus.PAB.Webserver.Types (ContractInstanceClientState (..))
import           Plutus.V1.Ledger.Value     (Value, flattenValue, toString)
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))

import qualified Platinum.Contracts.YieldFarming.Env as YF
import qualified Platinum.Contracts.YieldFarming.OffChain as YF

-- Wallet API https://github.com/input-output-hk/plutus/blob/master/plutus-pab/src/Cardano/Wallet/API.hs
data Command
    = Deposit Integer
    | Withdraw Integer
    | Balance Integer
    | ContractBalance
    | Harvest
    deriving (Show, Read, Eq, Ord)

baseUrlInstance :: String -> Url 'Http
baseUrlInstance host = http (pack host) /: "api"  /: "new" /: "contract" /: "instance"

baseUrlWallet :: String -> Url 'Http
baseUrlWallet host = http (pack host) /: "wallet"

deposit :: String -> Int -> UUID -> Integer -> IO ()
deposit host p uuid amt = handle h $ runReq defaultHttpConfig $ do
    let lov = amt * 1_000_000
    -- liftIO $ putStrLn $ B.unpack $ encode $ YF.AssetClassTransferParams YF.adaAssetClass lov
    initSt <- liftIO $ getStatus host p uuid
    v <- req
            POST
            (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "deposit")
            (ReqBodyJson $ YF.AssetClassTransferParams YF.adaAssetClass lov)
            (Proxy :: Proxy (JsonResponse ()))
            (port p)

    liftIO $ if responseStatusCode v == 200 then do
        putStrLn $ "Request to deposit " ++ show amt ++ " ADA sent. Waiting for result"
        printResult host p uuid initSt
    else
        putStrLn $ "After deposit request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in deposit"
        threadDelay 1_000_000 >> deposit host p uuid amt

withdraw :: String -> Int -> UUID -> Integer -> IO ()
withdraw host p uuid amt = handle h $ runReq defaultHttpConfig $ do
    let lov = amt * 1_000_000
    initSt <- liftIO $ getStatus host p uuid
    v <- req
            POST
            (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "withdraw")
            (ReqBodyJson $ YF.AssetClassTransferParams YF.adaAssetClass lov)
            (Proxy :: Proxy (JsonResponse ()))
            (port p)
    liftIO $ if responseStatusCode v == 200 then do
        putStrLn $ "Request to withdraw " ++ show amt ++ " ADA sent. Waiting for result"
        printResult host p uuid initSt
    else
        putStrLn $ "After withdraw request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in withdraw"
        threadDelay 1_000_000 >> withdraw host p uuid amt

harvest :: String -> Int -> UUID -> IO ()
harvest host p uuid = handle h $ runReq defaultHttpConfig $ do
    initSt <- liftIO $ getStatus host p uuid
    v <- req
            POST
            (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "harvest")
            (ReqBodyJson $ YF.HarvestParams YF.adaAssetClass)
            (Proxy :: Proxy (JsonResponse ()))
            (port p)
    liftIO $ if responseStatusCode v == 200 then do
        putStrLn $ "Request to harvest from ADA pool. Waiting for result"
        printResult host p uuid initSt
    else
        putStrLn $ "After harvedt request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in harvest"
        threadDelay 1_000_000 >> harvest host p uuid

contractBalance :: String -> Int -> UUID -> IO ()
contractBalance host p uuid = handle h $ runReq defaultHttpConfig $ do
    initSt <- liftIO $ getStatus host p uuid
    v <- req
            POST
            (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "scriptBalance")
            (ReqBodyJson ())
            (Proxy :: Proxy (JsonResponse ()))
            (port p)

    liftIO $ if responseStatusCode v == 200 then do
        putStrLn $ "Contract balance requested. Waiting for result"
        printResult host p uuid initSt
    else
        putStrLn $ "After deposit request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in contract balance"
        threadDelay 1_000_000 >> contractBalance host p uuid

walletBalance :: String -> Int -> UUID -> Integer -> IO ()
walletBalance host p uuid walletId = handle h $ runReq defaultHttpConfig $ do
    v <- req
            GET
            (baseUrlWallet host /: pack (show walletId) /: "total-funds")
            NoReqBody
            (Proxy :: Proxy (JsonResponse Value))
            (port p)
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
        threadDelay 1_000_000 >> walletBalance host p uuid walletId

type Status = ContractInstanceClientState A.Value

getStatus :: String -> Int -> UUID -> IO Status
getStatus host p uuid = runReq defaultHttpConfig $ do
    v <-
        req
            GET
            (baseUrlInstance host /: pack (show uuid) /: "status")
            NoReqBody
            (Proxy :: Proxy (JsonResponse Status))
            (port p)
    pure $ responseBody v

waitForResult :: String -> Int -> UUID -> Status -> IO Status
waitForResult host p uuid initSt' = do
    let initSt = cicCurrentState initSt'
    cur' <- getStatus host p uuid
    let cur = cicCurrentState cur'

    if (logs cur == logs initSt
        && err cur == err initSt
        && observableState cur == observableState initSt) then do
        threadDelay 1_000_000
        waitForResult host p uuid initSt'
    else
        pure cur'

printResult :: String -> Int -> UUID -> Status -> IO ()
printResult host p uuid initSt = do
    result <- waitForResult host p uuid initSt
    putStrLn "Result: "
    putStrLn $ B.unpack $ encodePretty $ observableState . cicCurrentState $ result