{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    ( main
    ) where

import           Prelude
import           Control.Monad                       (forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text)

import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Wallet.Emulator.Types               (Wallet (..))
import           Wallet.Types                        (ContractInstanceId (..))
import           Plutus.Contract

import qualified Platinum.Contracts.YieldFarming.Env as YF
import qualified Platinum.Contracts.YieldFarming.OffChain as YF
import           Platinum.Contracts.YieldFarming.PAB

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString
        @(Builtin YieldFarmingContractPABActions)
        "Starting Yield Farming PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidTestSetup <- Simulator.activateContract (Wallet 1) TestSetup
    initLPParams      <- waitForLast cidTestSetup
    void $ Simulator.waitUntilFinished cidTestSetup

    cidOwner <- Simulator.activateContract (Wallet 1) $ Init initLPParams
    liftIO $ writeFile "Wowner.cid" $ show $ unContractInstanceId cidOwner
    env <- waitForLast cidOwner

    forM_ testWallets $ \w ->
        when (w /= Wallet 1) $ do
            cid <- Simulator.activateContract w $ RunInstance env
            liftIO $ putStrLn $ show w <> " cid: " <> show (unContractInstanceId cid)
            liftIO $ writeFile ('W' : show (getWallet w) ++ ".cid") $ show $ unContractInstanceId cid

    void $ liftIO getLine
    shutdown

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

handleYFPABActions ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin YieldFarmingContractPABActions))) effs
    )
    => ContractEffect (Builtin YieldFarmingContractPABActions)
    ~> Eff effs
handleYFPABActions = handleBuiltin getSchema getContract where
    getSchema = \case
        TestSetup      -> endpointsToSchemas @EmptySchema
        Init _         -> endpointsToSchemas @EmptySchema
        RunInstance _  -> endpointsToSchemas @YF.YFUserEndpoints
    getContract = \case
        TestSetup       -> SomeBuiltin   testInitialSetup
        Init initParams -> SomeBuiltin @_ @EmptySchema $ YF.initLP initParams
        RunInstance env -> SomeBuiltin $ YF.userEndpoints env

handlers :: SimulatorEffectHandlers (Builtin YieldFarmingContractPABActions)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin YieldFarmingContractPABActions) []
    $ interpret handleYFPABActions

testInitialSetup :: Contract (Last YF.InitLPParams) EmptySchema Text ()
testInitialSetup = do
    tell $ Last $ Just $ YF.InitLPParams [YF.adaAssetClass]

testWallets :: [Wallet]
testWallets = [Wallet i | i <- [1 .. 4]]