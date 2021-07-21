{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main
    ( main
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class                  (MonadIO (..))
import Data.Aeson                              (Result (..), fromJSON)
import Data.Monoid                             (Last (..))
import Data.Proxy                              (Proxy (..))
import Data.Text                               (pack)
import Data.UUID
import Network.HTTP.Req
import Plutus.PAB.Webserver.Types
import System.Environment                      (getArgs)
import System.IO
import Text.Read                               (readMaybe)

import           Platinum.Contracts.YieldFarming.PAB
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
        go uuid

    readCommand :: IO Command
    readCommand = do
        putStr "Enter command (Deposit amt, Withdraw amt): "
        s <- getLine
        maybe (putStrLn "Couldn't parse command" >> readCommand) return $ readMaybe s

data Command
    = Deposit Integer
    | Withdraw Integer
    deriving (Show, Read, Eq, Ord)

deposit :: UUID -> Integer -> IO ()
deposit uuid amt = handle h $ runReq defaultHttpConfig $ do
    let lov = amt * 1_000_000
    v <- req
        POST
        (http "127.0.0.1" /: "api"  /: "new" /: "contract" /: "instance" /: pack (show uuid) /: "endpoint" /: "deposit")
        (ReqBodyJson $ YF.AssetClassTransferParams YF.adaAssetClass lov)
        (Proxy :: Proxy (JsonResponse ()))
        (port 8080)
    liftIO $ putStrLn $ if responseStatusCode v == 200
        then "Deposited " ++ show lov ++ " lovelace"
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
        then "Withdrew " ++ show lov ++ " lovelace"
        else "After withdraw request failure http code returned: " <> show (responseStatusCode v)
  where
    h :: HttpException -> IO ()
    h e = do
        putStrLn $ show e <> " happened in withdraw"
        threadDelay 1_000_000 >> withdraw uuid amt