{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Platinum.Contracts.YieldFarming.OnChain where

import           Control.Monad                  (void)
import           Data.Aeson                     (FromJSON, ToJSON)
import           GHC.Generics                   (Generic)
import           Ledger                         (TxInfo (..), ScriptContext (..), Slot (..), AssetClass, Address, Datum(Datum), ScriptContext, TxOutTx, Validator, Value (..), PubKeyHash)
import qualified Ledger
import qualified Ledger.Ada                     as Ada
import           Ledger.AddressMap              (UtxoMap)
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import           Plutus.Contract
import           Plutus.Contract.Schema         ()
import           Plutus.Trace.Emulator          (EmulatorTrace, observableState)
import qualified Plutus.Trace.Emulator          as Trace
import qualified PlutusTx
import           Plutus.V1.Ledger.Value         (flattenValue, valueOf, assetClass, assetClassValueOf)
import           PlutusTx.Prelude
import           Schema                         (ToArgument, ToSchema)
import           Wallet.Emulator                (Wallet (..))

import qualified Data.ByteString.Char8          as C
import qualified Prelude
import           Data.Maybe                     (catMaybes)
import           Data.Void                      (Void)
import qualified Control.Monad.Freer.Extras.Log as Extras
import qualified PlutusTx.AssocMap              as AMap
import           Plutus.Contract.StateMachine
import           Ledger.TimeSlot (posixTimeRangeToSlotRange)

import           Platinum.Contracts.YieldFarming.Constants
import           Platinum.Contracts.YieldFarming.Types
import           Platinum.Contracts.Utils

{-# INLINABLE stateTransition #-}
stateTransition
    :: State YieldFarmingDatum
    -> YieldFarmingRedeemer
    -> Maybe (TxConstraints Void Void, State YieldFarmingDatum)
stateTransition s MkTransfer{..} = do
    let YieldFarmingDatum{..} = stateData s
    let curPoolValues = stateValue s
    -- TODO move it to tx somehow?
    let changes = flattenValue tAmount
    flip mapM_ changes $ \(c, t, am) -> do
        let ac = assetClass c t
        guard (am + valueOf curPoolValues c t >= 0)
        pool <- AMap.lookup ac yfdPools
        newPool <- updatePool
            (tSender, ac, tSlot)
            (pool, assetClassValueOf curPoolValues ac)
        Just ()
        -- TODO recompute user debt
    --user <- AMap.lookup tSender yfdUsers
    --let updatedUser = UserInfo {uiRewardExcess = 0, uiAmount = uiAmount user + tAmount}
    --AMap.insert tSender updatedUser yfdUsers
    Just
        (mempty,
         State emptyYieldFarmingDatum (curPoolValues <> tAmount))

{-# INLINABLE updatePool #-}
updatePool :: (PubKeyHash, AssetClass, Slot) -> (PoolInfo, Integer) -> Maybe PoolInfo
updatePool (sender, asset, slot) (pInfo, totalSupplyInPool)
  | piSlotWhenRewardUpdated pInfo > slot = Nothing
  | otherwise = Just $ PoolInfo {
     piRewardsPerShare =
         piRewardsPerShare pInfo +
         getSlot (piSlotWhenRewardUpdated pInfo - slot) * rewardPerSlot % totalSupplyInPool,
     piSlotWhenRewardUpdated = slot
  }

{-# INLINABLE domainChecks #-}
domainChecks :: YieldFarmingDatum -> YieldFarmingRedeemer -> ScriptContext -> Bool
domainChecks YieldFarmingDatum{..} MkTransfer{..} ctx =
    -- check slot correspondence
    let txInfo = scriptContextTxInfo ctx in
    let validRange = posixTimeRangeToSlotRange (txInfoValidRange txInfo) in

    -- check supported pools
    -- check valid amounts
    -- let supportedLiqPools = getValue yfdPoolsBalances in
        -- AMap.fromList $ Ledger.flattenValue $ yfdPoolsBalances datum in
    -- let changes = Ledger.flattenValues amount in
    True

-- {-# INLINABLE areAssetClassesSupported #-}
-- | Return asset class which is not supported by the contract if any.
-- areAssetClassesSupported :: YieldFarmingDatum -> Value -> Maybe (CurrencySymbol, TokenName)
-- areAssetClassesSupported datum amount = undefined

yieldFarmingStateMachine :: StringConstants -> StateMachine YieldFarmingDatum YieldFarmingRedeemer
yieldFarmingStateMachine StringConstants{..} = StateMachine
    { smTransition  = stateTransition
    , smFinal       = const False
    , smCheck       = domainChecks
    , smThreadToken = Just scThreadToken
    }

{-# INLINABLE mkYieldFarmingValidator #-}
mkYieldFarmingValidator :: StringConstants -> YieldFarmingDatum -> YieldFarmingRedeemer -> ScriptContext -> Bool
mkYieldFarmingValidator sc = mkValidator (yieldFarmingStateMachine sc)

yieldFarmingInstance :: Scripts.TypedValidator YieldFarming
yieldFarmingInstance = Scripts.mkTypedValidator @YieldFarming
    ($$(PlutusTx.compile [|| mkYieldFarmingValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode stringConstants)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @YieldFarmingDatum @YieldFarmingRedeemer

yieldFarmingValidator :: Validator
yieldFarmingValidator = Scripts.validatorScript yieldFarmingInstance

data YieldFarming
instance Scripts.ValidatorTypes YieldFarming where
    type instance RedeemerType YieldFarming = YieldFarmingRedeemer
    type instance DatumType YieldFarming = YieldFarmingDatum