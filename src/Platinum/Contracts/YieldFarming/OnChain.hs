module Platinum.Contracts.YieldFarming.OnChain where

import           Ledger                         (PubKeyHash, ScriptContext (..), Slot (..), Validator)
import           Plutus.V1.Ledger.Value         (Value, AssetClass, TokenName, CurrencySymbol, assetClass,
                                                 singleton, assetClassValue, flattenValue, assetClassValueOf)
import qualified Ledger.Constraints             as Con
import qualified Ledger.Typed.Scripts           as Scripts
import           Plutus.Contract.Schema         ()
import qualified PlutusTx
import           PlutusTx.Ratio                 (truncate)
import           PlutusTx.Prelude
import qualified PlutusTx.AssocMap              as AMap
import           Plutus.Contract.StateMachine

import           Platinum.Contracts.YieldFarming.Constants
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
            updatedPool <- updatePool tSlot (pool, assetClassValueOf curPoolValues ac)
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
-- TODO consider a case when a pool is empty
updatePool :: Slot -> (PoolInfo, Integer) -> Maybe PoolInfo
updatePool slot (pInfo, totalSupplyInPool)
  | piSlotWhenRewardUpdated pInfo > slot = Nothing
  | otherwise = Just $ PoolInfo {
     piRewardsPerShare =
         piRewardsPerShare pInfo +
         getSlot (piSlotWhenRewardUpdated pInfo - slot) * rewardPerSlot % totalSupplyInPool,
     piSlotWhenRewardUpdated = slot
  }

{-# INLINABLE domainChecks #-}
domainChecks :: YieldFarmingDatum -> YieldFarmingRedeemer -> ScriptContext -> Bool
domainChecks _ _ _ =
    -- check slot correspondence
    -- let txInfo = scriptContextTxInfo ctx in
    -- let validRange = posixTimeRangeToSlotRange (txInfoValidRange txInfo) in

    -- -- check supported pools
    -- -- check valid amounts
    -- -- let supportedLiqPools = getValue yfdPoolsBalances in
    --     -- AMap.fromList $ Ledger.flattenValue $ yfdPoolsBalances datum in
    -- -- let changes = Ledger.flattenValues amount in
    True

-- {-# INLINABLE areAssetClassesSupported #-}
-- | Return asset class which is not supported by the contract if any.
-- areAssetClassesSupported :: YieldFarmingDatum -> Value -> Maybe (CurrencySymbol, TokenName)
-- areAssetClassesSupported datum amount = undefined

{-# INLINABLE yieldFarmingStateMachine #-}
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


----------------------------------------------------------------
--- TYPES
----------------------------------------------------------------

-- For some reason types have to be defined at the same file as yieldFarmingInstance
-- to avoid error 'GHC Core to PLC plugin: E042:Error: Unsupported feature: Kind: *'.

data PoolInfo = PoolInfo {
    piRewardsPerShare       :: !Rational,
    piSlotWhenRewardUpdated :: !Slot
}

data UserInfo = UserInfo {
    uiRewardExcess :: !Rational,
    uiAmount       :: !Value
}


data YieldFarmingDatum = YieldFarmingDatum {
    yfdPools :: AMap.Map AssetClass PoolInfo,
    yfdUsers :: AMap.Map PubKeyHash UserInfo
}

{-# INLINABLE emptyYieldFarmingDatum #-}
emptyYieldFarmingDatum :: YieldFarmingDatum
emptyYieldFarmingDatum = YieldFarmingDatum {
    yfdPools = AMap.empty,
    yfdUsers = AMap.empty
}

data YieldFarmingRedeemer
    -- Either deposit or withdrawal for several asset classes
    = MkTransfer {
        tSender :: !PubKeyHash,
        tAmount :: !Value,
        -- | Slot when the operation should performed
        tSlot   :: !Slot
    }

data YieldFarming
instance Scripts.ValidatorTypes YieldFarming where
    type instance RedeemerType YieldFarming = YieldFarmingRedeemer
    type instance DatumType YieldFarming = YieldFarmingDatum

PlutusTx.unstableMakeIsData ''PoolInfo
PlutusTx.makeLift ''PoolInfo

PlutusTx.unstableMakeIsData ''UserInfo
PlutusTx.makeLift ''UserInfo

PlutusTx.unstableMakeIsData ''YieldFarmingRedeemer
PlutusTx.makeLift ''YieldFarmingRedeemer

PlutusTx.unstableMakeIsData ''YieldFarmingDatum
PlutusTx.makeLift ''YieldFarmingDatum