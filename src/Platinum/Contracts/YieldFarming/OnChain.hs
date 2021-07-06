module Platinum.Contracts.YieldFarming.OnChain where

import           Control.Monad                  (void)
import           Data.Aeson                     (FromJSON, ToJSON)
import           GHC.Generics                   (Generic)
import           Ledger                         (TxInfo (..), ScriptContext (..), Slot (..), Address,
                                                 Datum(Datum), TxOutTx, Validator, Value (..), PubKeyHash)
import           Plutus.V1.Ledger.Value         (AssetClass, TokenName, CurrencySymbol, assetClass, tokenName, singleton, assetClassValue)
import qualified Ledger
import qualified Ledger.Ada                     as Ada
import           Ledger.AddressMap              (UtxoMap)
import qualified Ledger.Constraints             as Con
import qualified Ledger.Typed.Scripts           as Scripts
import           Plutus.Contract
import           Plutus.Contract.Schema         ()
import           Plutus.Trace.Emulator          (EmulatorTrace, observableState)
import qualified Plutus.Trace.Emulator          as Trace
import qualified PlutusTx
import           PlutusTx.Ratio                 (truncate)
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
import           Platinum.Contracts.Utils (lookupDefault, guard)

{-# INLINABLE stateTransition #-}
stateTransition
    :: StringConstants
    -> State YieldFarmingDatum
    -> YieldFarmingRedeemer
    -> Maybe (TxConstraints Void Void, State YieldFarmingDatum)
stateTransition StringConstants{..} s MkTransfer{..} = do
    let datum = stateData s
    let curPoolValues = stateValue s
    -- TODO move it to tx somehow?
    let changes = flattenValue tAmount
    let handleAssetClass
            :: (TxConstraints Void Void, YieldFarmingDatum)
            -> (CurrencySymbol, TokenName, Integer)
            -> Maybe (TxConstraints Void Void, YieldFarmingDatum)
        handleAssetClass (constraints, yd) (c, t, am) = do
            let ac = assetClass c t
            pool <- AMap.lookup ac $ yfdPools yd
            updatedPool <- updatePool
                (tSender, ac, tSlot)
                (pool, assetClassValueOf curPoolValues ac)
            let user = lookupDefault (UserInfo (fromInteger 0) mempty) tSender (yfdUsers yd)
            let userAmount = assetClassValueOf (uiAmount user) ac
            -- Check if a user has enough funds
            let newUserAmount = am + userAmount
            guard (newUserAmount >= 0)
            let outstandingReward =
                    assetClassValue (scRewardAssetClass) $
                        truncate $ fromInteger userAmount * piRewardsPerShare updatedPool - uiRewardExcess user
            let newConstraints = constraints <>
                    if (userAmount > 0) then Con.mustPayToPubKey tSender outstandingReward
                    else mempty
            let updatedUser = UserInfo {
                    uiRewardExcess = fromInteger newUserAmount * piRewardsPerShare updatedPool,
                    uiAmount = singleton c t am <> uiAmount user
                }
            return $ (
                newConstraints,
                YieldFarmingDatum {
                    yfdPools = AMap.insert ac updatedPool $ yfdPools yd,
                    yfdUsers =
                        if (newUserAmount > 0) then AMap.insert tSender updatedUser $ yfdUsers yd
                        else AMap.delete tSender (yfdUsers yd)
                })
    (constraints, newDatum) <- foldlM handleAssetClass (mempty, datum) changes
    Just (constraints, State newDatum (curPoolValues <> tAmount))

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
yieldFarmingStateMachine consts@StringConstants{..} = StateMachine
    { smTransition  = stateTransition consts
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