module Platinum.Contracts.YieldFarming.OnChain where

import           Ledger                         (PubKeyHash, ScriptContext (..), Slot (..), Validator,
                                                 Extended (..), Interval (..), to, member, txInfoValidRange,
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

import           Platinum.Contracts.YieldFarming.Env (Env (..), rewardPerSlot, requestValidityIntervalLength)
import           Platinum.Contracts.Utils            (lookupDefault, guard)

{-# INLINABLE stateTransition #-}
stateTransition
    :: Env
    -> State YieldFarmingDatum
    -> YieldFarmingRedeemer
    -> Maybe (TxConstraints Void Void, State YieldFarmingDatum)
stateTransition env s MkTransfer{..} = do
    -- For some reason nft thread state token presents in stateValue
    -- but it shouldn't present in output value... Hence, we need to substract it
    let subThrToken = assetClassValue (envThreadToken env) (negate 1)
    let datum = stateData s
    let poolFunds = stateValue s
    -- TODO move it to tx somehow?
    let changes = flattenValue tAmount
    let handleAssetClass
            :: (TxConstraints Void Void, YieldFarmingDatum, Value)
            -> (CurrencySymbol, TokenName, Integer)
            -> Maybe (TxConstraints Void Void, YieldFarmingDatum, Value)
        handleAssetClass (constraints, yd, totRewards) (c, t, am) = do
            let ac = assetClass c t
            pool <- AMap.lookup ac $ yfdPools yd
            -- Update pool's rewared-per-share and last slot with submitted slot
            updatedPool <- updatePool tSlot (pool, assetClassValueOf poolFunds ac)
            (userConstraint, newUsers, userReward) <-
                handleAssetClassTransferNRewardUser
                    env
                    tSender
                    (ac, am)
                    (piRewardsPerShare updatedPool)
                    (yfdUsers yd)
            return (
                constraints <> userConstraint,
                YieldFarmingDatum {
                    yfdPools = AMap.insert ac updatedPool $ yfdPools yd,
                    yfdUsers = newUsers
                },
                totRewards + userReward
                )
    (constraints, newDatum, totRewards) <-
        foldlM
            handleAssetClass
            (Con.mustBeSignedBy tSender, datum, mempty)
            changes
    Just (constraints,
          State newDatum (poolFunds <> tAmount <> scale (-1) totRewards <> subThrToken))

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
             getSlot (slot - piSlotWhenRewardUpdated pInfo) * rewardPerSlot % totalSupplyInPool
         else fromInteger 0,
     piSlotWhenRewardUpdated = slot
  }

-- | Update user amount of current asset class.
-- Also recompute user's reward correction in order to assess future reward payouts.
{-# INLINABLE handleAssetClassTransferNRewardUser #-}
handleAssetClassTransferNRewardUser
    :: Env
    -- ^ Environment of the contract
    -> PubKeyHash
    -> (AssetClass, Integer)
    -> Rational
    -- ^ Reward per share of pool corresponding to asset class
    -> AMap.Map PubKeyHash UserInfo
    -> Maybe (TxConstraints Void Void, AMap.Map PubKeyHash UserInfo, Value)
handleAssetClassTransferNRewardUser Env{..} sender (ac, transfered) rewardPerShare users = do
    let user = lookupDefault (UserInfo (fromInteger 0) mempty) sender users
    let userAmount = assetClassValueOf (uiAmounts user) ac
    -- Check if the user has enough funds after this transfer
    let newUserAmount = userAmount + transfered
    guard (newUserAmount >= 0)

    let updatedUser = UserInfo {
            uiRewardExcess = fromInteger newUserAmount * rewardPerShare,
            uiAmounts      = assetClassValue ac transfered <> uiAmounts user
        }

    -- Remove user from map, if there are no their token in the pool anymore.
    let newUsers =
            if newUserAmount > 0 then AMap.insert sender updatedUser users
            else AMap.delete sender users
    let (rewardConstr, rewardAmount) = rewardTxConstraint user
    return (transferTxConstraint <> rewardConstr, newUsers, rewardAmount)
  where
    transferTxConstraint :: TxConstraints Void Void
    transferTxConstraint
        -- Can't do
        --     Con.mustPayToTheScript sender $ assetClassValue ac transfered
        -- because out type of TxConstraints becomes PubKeyHash,
        -- and it doesn't match with TxConstraints Void Void of 'smTransition'.
        | transfered > 0 = mempty
        | transfered < 0 =
            Con.mustPayToPubKey sender $ assetClassValue ac (negate transfered)
        | otherwise = mempty

    -- | Build TxContraint which checks that user is paid a reward
    -- for currently stacked amount
    rewardTxConstraint :: UserInfo -> (TxConstraints Void Void, Value)
    rewardTxConstraint user =
        let userAmount = assetClassValueOf (uiAmounts user) ac in

        -- Reward for current user amount for an interval of slots,
        -- starting from the one, when the user deposited/withdrawn last time.
        let outstandingReward =
                assetClassValue envRewardToken $
                    truncate $ fromInteger userAmount * rewardPerShare - uiRewardExcess user in

        -- Constraint for a transaction which transfer user's reward to them.
        -- If user didn't have any tokens before then send nothing.
        if userAmount > 0 then
            (Con.mustPayToPubKey sender outstandingReward, outstandingReward)
            --(mempty, outstandingReward)
        else
            (mempty, mempty)

{-# INLINABLE domainChecks #-}
domainChecks :: YieldFarmingDatum -> YieldFarmingRedeemer -> ScriptContext -> Bool
domainChecks _ MkTransfer{..} ctx = True
    -- TODO limit reward slot somehow
    -- check slot correspondence
    -- traceIfFalse "tSlot is out of acceptable bounds" slotInAcceptableBounds
  where
      txInfo = scriptContextTxInfo ctx
      validRange = posixTimeRangeToSlotRange (txInfoValidRange txInfo)

      -- lower bound must be finite and [lower bound; lower bound + 20] has to contain tSlot
      slotInAcceptableBounds :: Bool
      slotInAcceptableBounds = case ivFrom validRange of
          LowerBound (Finite l) _ -> tSlot `member` to (l + Slot requestValidityIntervalLength)
          _        -> False

{-# INLINABLE yieldFarmingStateMachine #-}
yieldFarmingStateMachine :: Env -> StateMachine YieldFarmingDatum YieldFarmingRedeemer
yieldFarmingStateMachine env@Env{..} = StateMachine
    { smTransition  = stateTransition env
    , smFinal       = const False
    , smCheck       = domainChecks
    , smThreadToken = Just envThreadToken
    }

{-# INLINABLE mkYieldFarmingValidator #-}
mkYieldFarmingValidator :: Env -> YieldFarmingDatum -> YieldFarmingRedeemer -> ScriptContext -> Bool
mkYieldFarmingValidator = mkValidator . yieldFarmingStateMachine

type YFS = StateMachine YieldFarmingDatum YieldFarmingRedeemer

yieldFarmingInstance :: Env -> Scripts.TypedValidator YFS
yieldFarmingInstance sconsts = Scripts.mkTypedValidator @YFS
    ($$(PlutusTx.compile [|| mkYieldFarmingValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sconsts)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @YieldFarmingDatum @YieldFarmingRedeemer

yieldFarmingValidator :: Env -> Validator
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
    uiAmounts      :: !Value
} deriving stock (Prelude.Show)

data YieldFarmingDatum = YieldFarmingDatum {
    yfdPools :: !(AMap.Map AssetClass PoolInfo),
    yfdUsers :: !(AMap.Map PubKeyHash UserInfo)
} deriving stock (Prelude.Show)

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