module Platinum.Contracts.YieldFarming.Env where

import           Prelude                (Show (..))
import           Data.Aeson             (ToJSON, FromJSON)
import           GHC.Generics           (Generic)

import           Plutus.V1.Ledger.Value (AssetClass, TokenName, assetClass)
import           Plutus.V1.Ledger.Ada   (adaSymbol, adaToken)
import qualified PlutusTx
import           PlutusTx.Prelude
import           Ledger                 (PubKeyHash)

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

{-# INLINABLE threadTokenName #-}
threadTokenName :: TokenName
threadTokenName = "PLATINUM_YIELD_FARM_THREAD_TOKEN"

{-# INLINABLE adaAssetClass #-}
adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken

data Env = Env {
    envThreadToken :: !AssetClass,
    envRewardToken :: !AssetClass,
    envOwner       :: !PubKeyHash
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''Env
PlutusTx.makeLift ''Env