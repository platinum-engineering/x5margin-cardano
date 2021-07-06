-- | Total reward for user u is computed as a sum:
-- R_u = S_1 * R_b * (1/tot_supply[1] + 1/tot_supply[2] + ... + 1/tot_supply[x])
--     + S_2 * R_b * (1/tot_supply[x + 1] + 1/tot_supply[x + 2] + ... + 1/tot_supply[n]) ...
--     + S_3 * ...
-- where R_b is a reward for a block, constant
-- where tot_supply[s] - is a total supply of the contract in slot s;
-- where S_1 is a balance of user in the first segment of slots [1 ... x]
--       S_2 is a balance of user in the second segment of slots [x+1 ... n], etc;
-- It's implied that S_k != S_{k+1}.
-- So until S_k is a constant we can just sum up 1/tot_supply[s] in piRewardsPerShare at each slot.
-- But in reality we don't do it at each slot,
-- we update piRewardsPerShare only when tot_supply changes (adding up: number of blocks * 1/tot_supply * R_b).
-- So at every moment of time, we can easy compute R_i.
--
-- Some difficulties arise when user start to deposit and withdraw tokens (so, when S_i changes).
-- Whenever it happens, we transfer their outstanding reward to them, and start accumulating
-- new sum of 1/tot_supply[x].
-- Let's denote R_b * (1/tot_supply[l] + 1/tot_supply[l+1] + ... + 1/tot_supply[r]) corresponding to S_i as Σ_i.
-- Then R_u = S_1 * Σ_1 + S_2 * Σ_2 + S_3 * Σ_3...
-- The behaviour of the contract, that whenever S_i changes (either due to deposit or withdrawal),
-- we pay off S_i * Σ_i reward to a user, and start accumulating next Σ_{i+1}.
--
-- However, we wouldn't like to store all Σ_i for each user
-- (they are different among users because
-- when one S_i changes, others are still the same, so no need fresh Σ_i to be produced for them).
-- But we can achieve it using the following trick:
-- S_i * Σ_i = (Σ_1 + Σ_2 + ... Σ_i) * S_i - (Σ_1 + Σ_2 + ... + Σ_{i-1}) * S_i
-- So, storing only the value of Σ_1 + Σ_2 + ... Σ_i (which is, in contrast, the same for every user)
-- and (Σ_1 + Σ_2 + ... + Σ_{i-1}) * S_i for every user,
-- we can calculate outstanding reward S_i * Σ_i.
-- We will call (Σ_1 + Σ_2 + ... + Σ_{i-1}) * S_i as rewardExcess_i, in a sense it's a delta
-- which has to be subtracted to get correct reward for some block segment where balance (S_i) was constant.

module Platinum.Contracts.YieldFarming.Types where

import qualified PlutusTx
import           Ledger                         (PubKeyHash, Value, Slot, AssetClass)
import qualified PlutusTx.AssocMap              as AMap
import           PlutusTx.Prelude

data PoolInfo = PoolInfo {
    -- TODO change Double to something more stable
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
        tSlot   :: Slot
    }

PlutusTx.unstableMakeIsData ''PoolInfo
PlutusTx.makeLift ''PoolInfo

PlutusTx.unstableMakeIsData ''UserInfo
PlutusTx.makeLift ''UserInfo

PlutusTx.unstableMakeIsData ''YieldFarmingDatum
PlutusTx.makeLift ''YieldFarmingDatum

PlutusTx.unstableMakeIsData ''YieldFarmingRedeemer
PlutusTx.makeLift ''YieldFarmingRedeemer