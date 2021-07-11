module Platinum.Contracts.YieldFarming.Constants where

import Plutus.V1.Ledger.Value (AssetClass, assetClass, tokenName, TokenName)
import qualified PlutusTx
import PlutusTx.Prelude

-- | How far away requested withdrawal slot might be. In slots
{-# INLINABLE requestValidityIntervalLength #-}
requestValidityIntervalLength :: Integer
requestValidityIntervalLength = 20

{-# INLINABLE rewardPerSlot #-}
rewardPerSlot :: Integer
rewardPerSlot  = 100

{-# INLINABLE rewardTokenName #-}
rewardTokenName :: TokenName
rewardTokenName = "LP_RWRD"

{-# INLINABLE threadToken #-}
threadToken :: AssetClass
threadToken = assetClass "ff" (tokenName "PLATINUM_YIELD_FARM_THREAD_TOKEN")

data StringConstants = StringConstants {
    scThreadToken      :: !AssetClass
}

{-# INLINABLE stringConstants #-}
stringConstants :: StringConstants
stringConstants = StringConstants
    { scThreadToken = threadToken
    }

PlutusTx.unstableMakeIsData ''StringConstants
PlutusTx.makeLift ''StringConstants