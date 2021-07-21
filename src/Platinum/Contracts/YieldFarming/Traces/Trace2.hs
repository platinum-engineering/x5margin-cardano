{-# LANGUAGE NumericUnderscores #-}

module Platinum.Contracts.YieldFarming.Traces.Trace2
       ( trace2IO
       ) where

import Prelude
import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Monoid                (Last (..))

import Ledger (pubKeyHash)
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

import Platinum.Contracts.YieldFarming.OffChain
import Platinum.Contracts.YieldFarming.Env

trace2IO :: IO ()
trace2IO = runEmulatorTraceIO trace

trace :: EmulatorTrace ()
trace = do
    let pkh1 = pubKeyHash $ walletPubKey $ Wallet 1
    let pkh2 = pubKeyHash $ walletPubKey $ Wallet 2
    let pkh3 = pubKeyHash $ walletPubKey $ Wallet 3
    Extras.logInfo $ "Wallet pk hashes: " <> show [pkh1, pkh2, pkh3]

    hOwner <- activateContractWallet (Wallet 1) ownerEndpoints

    callEndpoint @"init" hOwner $
        InitLPParams
        { ilpAssetClasses = [adaAssetClass]
        }
    void $ Emulator.waitNSlots 4

    Last m <- observableState hOwner
    case m of
        Nothing -> Extras.logError @String "Error starting LP"
        Just env -> do
            h2 <- activateContractWallet (Wallet 2) (userEndpoints env)
            h3 <- activateContractWallet (Wallet 3) (userEndpoints env)

            callEndpoint @"deposit" h2 $
                AssetClassTransferParams
                { actpAC = adaAssetClass
                , actpAmount = 10_000_000
                }
            void $ Emulator.waitNSlots 1


            callEndpoint @"deposit" h3 $
                AssetClassTransferParams
                { actpAC = adaAssetClass
                , actpAmount = 10_000_000
                }
            void $ Emulator.waitNSlots 10

            callEndpoint @"withdraw" h2 $
                AssetClassTransferParams
                { actpAC = adaAssetClass
                , actpAmount = 10_000_000
                }
            void $ Emulator.waitNSlots 10

            callEndpoint @"withdraw" h3 $
                AssetClassTransferParams
                { actpAC = adaAssetClass
                , actpAmount = 10_000_000
                }
            s <- Emulator.waitNSlots 1

            Extras.logInfo $ "reached slot " ++ show s