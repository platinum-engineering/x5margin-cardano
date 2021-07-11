{-# LANGUAGE TupleSections #-}

module Platinum.Contracts.YieldFarming.OffChain where

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Control.Monad                hiding (fmap)
import           Schema                       (ToSchema)

import           Plutus.V1.Ledger.Value       (assetClass)
import           Plutus.Contract.StateMachine
import           PlutusTx.Prelude
import           Plutus.Contract              as Contract
import qualified PlutusTx.AssocMap            as AMap
import           Ledger                       hiding (singleton)
import           Plutus.V1.Ledger.Value       (assetClassValue)
import           Playground.TH                (mkSchemaDefinitions)
import           Prelude                      (Show (..))
import qualified Plutus.Contracts.Currency     as UC

import           Platinum.Contracts.YieldFarming.OnChain
import           Platinum.Contracts.YieldFarming.Constants (stringConstants, rewardTokenName)

yfClient :: StateMachineClient YieldFarmingDatum YieldFarmingRedeemer
yfClient =
    mkStateMachineClient $
    StateMachineInstance
        (yieldFarmingStateMachine stringConstants)
        (yieldFarmingInstance stringConstants)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

mapErrorC :: Contract w s UC.CurrencyError a -> Contract w s Text a
mapErrorC = mapError $ pack . show

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
    ownerPkh <- pubKeyHash <$> Contract.ownPubKey

    -- Create reward tokens
    -- Make initial emission of reward token
    oneShotCur <- mapErrorC $ UC.forgeContract ownerPkh [(rewardTokenName, 1000000000)]
    let rewardAssetClass = assetClass (UC.currencySymbol oneShotCur) rewardTokenName
    logInfo $ "Reward tokens minted " ++ show rewardAssetClass

    -- Create pool
    -- TODO check that no RWRD asset class among ilpAssetClasses
    curSlot <- Contract.currentSlot
    let emptyPool = emptyPoolInfo curSlot
    let yfInitDatum =
           YieldFarmingDatum
           { yfdPools = AMap.fromList $ map (, emptyPool) ilpAssetClasses,
             yfdUsers = AMap.empty,
             yfdOwner = ownerPkh,
             yfdRewardToken = rewardAssetClass
           }
    void $ mapErrorSM $ runInitialise yfClient yfInitDatum (UC.forgedValue oneShotCur)
    logInfo $ "Started liquidity pool with constant amount of reward tokens " ++ show yfInitDatum

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