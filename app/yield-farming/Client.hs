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
     , walletPublicKey
     , pendingReward
     , harvest
     , userStakes
     , Command (..)
     , YfClientEnv (..)
     ) where

import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import           Control.Concurrent
import           Control.Exception          (handle)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad              (when)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (pack)
import           Data.UUID    hiding        (toString, fromString)
import qualified Data.Aeson               as A
import           Network.HTTP.Req
import           Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Plutus.PAB.Webserver.Types (ContractInstanceClientState (..))
import           Plutus.V1.Ledger.Value     (flattenValue, toString)
import           Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import           Ledger

import qualified Platinum.Contracts.YieldFarming.Env as YF
import qualified Platinum.Contracts.YieldFarming.OffChain as YF

-- Wallet API https://github.com/input-output-hk/plutus/blob/master/plutus-pab/src/Cardano/Wallet/API.hs
data Command
    = Deposit Integer
    | Withdraw Integer
    | Balance Int
    | ContractBalance
    | Harvest
    | PendingReward Int
    | UserStakes Int
    | PublicKey
    deriving (Show, Read, Eq, Ord)

data YfClientEnv = YfClientEnv {
    ceHost :: String,
    cePort :: Int,
    ceUUID :: UUID
} deriving (Eq, Show)

type RIO = ReaderT YfClientEnv IO

baseUrlInstance :: String -> Url 'Http
baseUrlInstance host = http (pack host) /: "api"  /: "new" /: "contract" /: "instance"

baseUrlWallet :: String -> Url 'Http
baseUrlWallet host = http (pack host) /: "wallet"

deposit :: Integer -> RIO ()
deposit amt = do
    uuid <- asks ceUUID
    endpoint "deposit" (const $ pure True) $ \host p -> req
        POST
        (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "deposit")
        (ReqBodyJson $ YF.AssetClassTransferParams YF.adaAssetClass $ amt * 1_000_000)
        (Proxy :: Proxy (JsonResponse ()))
        (port p)

withdraw :: Integer -> RIO ()
withdraw amt = do
    uuid <- asks ceUUID
    endpoint "withdraw" (const $ pure True) $ \host p -> req
        POST
        (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "withdraw")
        (ReqBodyJson $ YF.AssetClassTransferParams YF.adaAssetClass $ amt * 1_000_000)
        (Proxy :: Proxy (JsonResponse ()))
        (port p)

harvest :: RIO ()
harvest = do
    uuid <- asks ceUUID
    endpoint "harvest" (const $ pure True) $ \host p -> req
        POST
        (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "harvest")
        (ReqBodyJson $ YF.HarvestParams YF.adaAssetClass)
        (Proxy :: Proxy (JsonResponse ()))
        (port p)

pendingReward :: PubKeyHash -> RIO ()
pendingReward pkh = do
    uuid <- asks ceUUID
    endpoint "pendingReward" (const $ pure True) $ \host p -> req
        POST
        (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "pendingReward")
        (ReqBodyJson $ YF.PendingRewardParams pkh YF.adaAssetClass)
        (Proxy :: Proxy (JsonResponse ()))
        (port p)

userStakes :: PubKeyHash -> RIO ()
userStakes pkh = do
    uuid <- asks ceUUID
    endpoint "userStakes" (const $ pure True) $ \host p -> req
        POST
        (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "userStakes")
        (ReqBodyJson pkh)
        (Proxy :: Proxy (JsonResponse ()))
        (port p)

contractBalance :: RIO ()
contractBalance = do
    uuid <- asks ceUUID
    endpoint "contractBalance" (const $ pure True) $ \host p -> req
        POST
        (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "contractBalance")
        (ReqBodyJson ())
        (Proxy :: Proxy (JsonResponse ()))
        (port p)

walletBalance :: Int -> RIO ()
walletBalance walletId = do
    let prettyValue =
            intercalate ", " .
            map (\(_, t, am) -> (if t == "" then "ADA" else toString t) ++ ": " ++ show am) .
            flattenValue
    endpoint
        "balance"
        (\v -> False <$ liftIO (putStrLn $ "Wallet " ++ show walletId ++ " balance: " ++ prettyValue v)) $ \host p ->
            req
                GET
                (baseUrlWallet host /: pack (show walletId) /: "total-funds")
                NoReqBody
                (Proxy :: Proxy (JsonResponse Value))
                (port p)

walletPublicKey :: RIO ()
walletPublicKey = do
    uuid <- asks ceUUID
    endpoint "publicKey" (const $ pure True) $ \host p -> req
        POST
        (baseUrlInstance host /: pack (show uuid) /: "endpoint" /: "publicKey")
        (ReqBodyJson ())
        (Proxy :: Proxy (JsonResponse ()))
        (port p)

endpoint
    :: HttpResponse resp
    => String
    -> (HttpResponseBody resp -> RIO Bool)
    -> (String -> Int -> Req resp)
    -> RIO ()
endpoint method afterReq reqAct = do
    env@YfClientEnv{..} <- ask
    let run a = runReaderT a env
    handleHttpEx method $ runReq defaultHttpConfig $ do
        initSt <- liftIO $ run $ getStatus ceUUID
        v <- reqAct ceHost cePort
        liftIO $ if responseStatusCode v == 200 then do
            putStrLn $ method <> " requested. Waiting for result"
            wait <- run $ afterReq (responseBody v)
            when wait $ run $ printResult ceUUID initSt
        else
            putStrLn $ "After " <> method <> " request failure http code returned: " <> show (responseStatusCode v)

handleHttpEx :: forall x . String -> IO x -> RIO x
handleHttpEx method act = liftIO $ handle h act
  where
    h :: HttpException -> IO x
    h e = do
        putStrLn $ show e <> " happened in " <> method
        threadDelay 1_000_000 >> act

type Status = ContractInstanceClientState A.Value

getStatus :: UUID -> RIO Status
getStatus uuid = do
    YfClientEnv{..} <- ask
    runReq defaultHttpConfig $ do
        v <- req
                GET
                (baseUrlInstance ceHost /: pack (show uuid) /: "status")
                NoReqBody
                (Proxy :: Proxy (JsonResponse Status))
                (port cePort)
        pure $ responseBody v

waitForResult :: UUID -> Status -> RIO Status
waitForResult uuid initSt' = do
    let initSt = cicCurrentState initSt'
    cur' <- getStatus uuid
    let cur = cicCurrentState cur'

    if (logs cur == logs initSt
        && err cur == err initSt
        && observableState cur == observableState initSt) then do
        liftIO $ threadDelay 1_000_000
        waitForResult uuid initSt'
    else
        pure cur'

printResult :: UUID -> Status -> RIO ()
printResult uuid initSt = do
    result <- waitForResult uuid initSt
    liftIO $ putStrLn "Result: "
    liftIO $ putStrLn $ B.unpack $ encodePretty $ observableState . cicCurrentState $ result