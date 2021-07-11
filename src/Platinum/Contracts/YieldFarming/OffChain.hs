{-# LANGUAGE TupleSections #-}

module Platinum.Contracts.YieldFarming.OffChain where

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Control.Monad                hiding (fmap)
import           Schema                       (ToSchema)

import           Plutus.Contract.StateMachine
import           PlutusTx.Prelude
import           Plutus.Contract              as Contract
import qualified PlutusTx.AssocMap            as AMap
import           Ledger                       hiding (singleton)
import           Plutus.V1.Ledger.Value       (assetClassValue)
import           Playground.TH                (mkSchemaDefinitions)
import           Prelude                      (Show (..))

import           Platinum.Contracts.YieldFarming.OnChain
import           Platinum.Contracts.YieldFarming.Constants (stringConstants)

yfClient :: StateMachineClient YieldFarmingDatum YieldFarmingRedeemer
yfClient =
    mkStateMachineClient $
    StateMachineInstance
        (yieldFarmingStateMachine stringConstants)
        (yieldFarmingInstance stringConstants)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

-------------------------------------------------
-- Off-chain endpoints
-------------------------------------------------

data InitLPParams = InitLPParams {
    ilpAssetClasses :: [AssetClass]
} deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

initLP
    :: InitLPParams
    -> Contract w s Text ()
initLP InitLPParams{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    curSlot <- Contract.currentSlot
    let emptyPool = emptyPoolInfo curSlot
    let yfInitDatum =
           YieldFarmingDatum
           { yfdPools = AMap.fromList $ map (, emptyPool) ilpAssetClasses,
             yfdUsers = AMap.empty,
             yfdOwner = pkh
           }
    void $ mapErrorSM $ runInitialise yfClient yfInitDatum mempty
    logInfo $ "Started liquidity pool " ++ show yfInitDatum

-------------------------------------------------
-- On-chain endpoints
-------------------------------------------------

data AssetClassTransferParams = AssetClassTransferParams {
    actpAC     :: !AssetClass,
    actpAmount :: !Integer
} deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data TransferParams = TransferParams {
    actpAmounts :: !Value
} deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

deposit :: AssetClassTransferParams -> Contract w s Text ()
deposit AssetClassTransferParams{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    curSlot <- Contract.currentSlot
    void $ mapErrorSM $ runStep yfClient $
        MkTransfer pkh (assetClassValue actpAC actpAmount) curSlot

withdraw :: AssetClassTransferParams -> Contract w s Text ()
withdraw AssetClassTransferParams{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    curSlot <- Contract.currentSlot
    void $ mapErrorSM $ runStep yfClient $
        MkTransfer pkh (assetClassValue actpAC (-actpAmount)) curSlot

transfer :: TransferParams -> Contract w s Text ()
transfer TransferParams{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    curSlot <- Contract.currentSlot
    void $ mapErrorSM $ runStep yfClient $
        MkTransfer pkh actpAmounts curSlot

type YFEndpoints =
        Endpoint "init"  InitLPParams
    .\/ Endpoint "deposit"  AssetClassTransferParams
    .\/ Endpoint "withdraw" AssetClassTransferParams
    .\/ Endpoint "transfer" TransferParams

endpoints :: Contract () YFEndpoints Text ()
endpoints =
    (init' `select`
     deposit' `select`
     withdraw' `select`
     transfer') >>
    endpoints
  where
    init' = endpoint @"init" >>= initLP
    deposit'   = endpoint @"deposit"   >>= deposit
    withdraw' = endpoint @"withdraw" >>= withdraw
    transfer' = endpoint @"transfer" >>= transfer

mkSchemaDefinitions ''YFEndpoints

-- myToken :: KnownCurrency
-- myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

-- mkKnownCurrencies ['myToken]