module Platinum.Contracts.YieldFarming.OnChain where

import           Ledger                         (PubKeyHash, ScriptContext (..), Slot (..), Validator,
                                                 Extended (..), Interval (..), interval, member, txInfoValidRange,
                                                 LowerBound (..))
import           Ledger.TimeSlot                (posixTimeRangeToSlotRange)
import           Plutus.V1.Ledger.Value         (Value, AssetClass, TokenName, CurrencySymbol, assetClass,
                                                 assetClassValue, flattenValue, assetClassValueOf)
import qualified Ledger.Constraints             as Con
import qualified Ledger.Typed.Scripts           as Scripts
import           Plutus.Contract.Schema         ()
import qualified PlutusTx
import           PlutusTx.Ratio                 (truncate)
import           PlutusTx.Prelude
import qualified PlutusTx.AssocMap              as AMap
import           Plutus.Contract.StateMachine
import qualified Prelude

import           Platinum.Contracts.YieldFarming.Constants (StringConstants (..), rewardPerSlot, requestValidityIntervalLength)
import           Platinum.Contracts.Utils                  (lookupDefault, guard)

{-# INLINABLE stateTransition #-}
stateTransition
    :: StringConstants
    -> State YieldFarmingDatum
    -> YieldFarmingRedeemer
    -> Maybe (TxConstraints Void Void, State YieldFarmingDatum)
stateTransition consts s MkTransfer{..} = do
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
            -- Update pool's rewared-per-share and last slot with submitted slot
            updatedPool <- updatePool tSlot (pool, assetClassValueOf curPoolValues ac)
            (userConstraint, newUsers) <-
                handleAssetClassTransferNRewardUser
                    consts
                    tSender
                    (ac, am)
                    (piRewardsPerShare updatedPool)
                    (yfdUsers yd)
            return (
                constraints <> userConstraint,
                YieldFarmingDatum {
                    yfdPools = AMap.insert ac updatedPool $ yfdPools yd,
                    yfdUsers = newUsers,
                    yfdOwner = yfdOwner datum

                })
    (constraints, newDatum) <- foldlM handleAssetClass (mempty, datum) changes
    Just (Con.mustBeSignedBy tSender <> constraints,
          State newDatum (curPoolValues <> tAmount))

-- | Recompute reward-per-share value: add accumulated reward between
-- last known slot and current slot.
-- Also update last known slot.
{-# INLINABLE updatePool #-}
updatePool :: Slot -> (PoolInfo, Integer) -> Maybe PoolInfo
updatePool slot (pInfo, totalSupplyInPool)
  | piSlotWhenRewardUpdated pInfo > slot = Nothing
  | otherwise = Just $ PoolInfo {
     piRewardsPerShare =
         piRewardsPerShare pInfo +
         if totalSupplyInPool > 0 then
             getSlot (piSlotWhenRewardUpdated pInfo - slot) * rewardPerSlot % totalSupplyInPool
         else fromInteger 0,
     piSlotWhenRewardUpdated = slot
  }

-- TODO emit actual transfers with constraints
-- | Update user amount of current asset class.
-- Also recompute user's reward correction in order to assess future reward payouts.
{-# INLINABLE handleAssetClassTransferNRewardUser #-}
handleAssetClassTransferNRewardUser
    :: StringConstants
    -- ^ String constants with scRewardAssetClass
    -> PubKeyHash
    -> (AssetClass, Integer)
    -> Rational
    -- ^ Reward per share of pool corresponding to asset class
    -> AMap.Map PubKeyHash UserInfo
    -> Maybe (TxConstraints Void Void, AMap.Map PubKeyHash UserInfo)
handleAssetClassTransferNRewardUser StringConstants{..} sender (ac, transfered) rewardPerShare users = do
    let user = lookupDefault (UserInfo (fromInteger 0) mempty) sender users
    let userAmount = assetClassValueOf (uiAmount user) ac
    -- Check if the user has enough funds after this transfer
    let newUserAmount = userAmount + transfered
    guard (newUserAmount >= 0)

    -- Reward for current user amount for an interval of slots,
    -- starting from the one, when the user deposited/withdrawn last time.
    let outstandingReward =
            assetClassValue scRewardAssetClass $
                truncate $ fromInteger userAmount * rewardPerShare - uiRewardExcess user

    -- Constraint for a transaction which transfer user's reward to them.
    -- If user didn't have any tokens before then send nothing.
    let userConstraint =
            if userAmount > 0 then Con.mustPayToPubKey sender outstandingReward
            else mempty

    let updatedUser = UserInfo {
            uiRewardExcess = fromInteger newUserAmount * rewardPerShare,
            uiAmount = assetClassValue ac transfered <> uiAmount user
        }

    -- Remove user from map, if there are no their token in the pool anymore.
    let newUsers =
            if newUserAmount > 0 then AMap.insert sender updatedUser users
            else AMap.delete sender users
    return (userConstraint, newUsers)

{-# INLINABLE domainChecks #-}
domainChecks :: YieldFarmingDatum -> YieldFarmingRedeemer -> ScriptContext -> Bool
domainChecks _ MkTransfer{..} ctx =
    -- check slot correspondence
    traceIfFalse "tSlot is out of acceptable bounds" slotInAcceptableBounds
  where
      txInfo = scriptContextTxInfo ctx
      validRange = posixTimeRangeToSlotRange (txInfoValidRange txInfo)

      -- lower bound must be finite and [lower bound; lower bound + 20] has to contain tSlot
      slotInAcceptableBounds :: Bool
      slotInAcceptableBounds = case ivFrom validRange of
          LowerBound (Finite l) _ -> tSlot `member` interval l (l + Slot requestValidityIntervalLength)
          _        -> False

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

type YFS = StateMachine YieldFarmingDatum YieldFarmingRedeemer

yieldFarmingInstance :: StringConstants -> Scripts.TypedValidator YFS
yieldFarmingInstance sconsts = Scripts.mkTypedValidator @YFS
    ($$(PlutusTx.compile [|| mkYieldFarmingValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sconsts)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @YieldFarmingDatum @YieldFarmingRedeemer

yieldFarmingValidator :: StringConstants -> Validator
yieldFarmingValidator = Scripts.validatorScript . yieldFarmingInstance


----------------------------------------------------------------
--- TYPES
----------------------------------------------------------------

-- For some reason types have to be defined at the same file as yieldFarmingInstance
-- to avoid error 'GHC Core to PLC plugin: E042:Error: Unsupported feature: Kind: *'.

data PoolInfo = PoolInfo {
    piRewardsPerShare       :: !Rational,
    piSlotWhenRewardUpdated :: !Slot
} deriving stock (Prelude.Show)

{-# INLINABLE emptyPoolInfo #-}
emptyPoolInfo :: Slot -> PoolInfo
emptyPoolInfo creationSlot = PoolInfo {
    piRewardsPerShare = fromInteger 0,
    piSlotWhenRewardUpdated = creationSlot
}

data UserInfo = UserInfo {
    uiRewardExcess :: !Rational,
    uiAmount       :: !Value
} deriving stock (Prelude.Show)

data YieldFarmingDatum = YieldFarmingDatum {
    yfdPools :: AMap.Map AssetClass PoolInfo,
    yfdUsers :: AMap.Map PubKeyHash UserInfo,
    yfdOwner :: PubKeyHash
} deriving stock (Prelude.Show)

{-# INLINABLE emptyYieldFarmingDatum #-}
emptyYieldFarmingDatum :: PubKeyHash -> YieldFarmingDatum
emptyYieldFarmingDatum owner = YieldFarmingDatum {
    yfdPools = AMap.empty,
    yfdUsers = AMap.empty,
    yfdOwner = owner
}

data YieldFarmingRedeemer
    -- Either deposit or withdrawal for several asset classes
    = MkTransfer {
        tSender :: !PubKeyHash,
        -- | Amount to be transferred.
        -- Amount corresponding to a specific asset class in Value
        -- might be either deposit or withdrawal depending on amount sign.
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