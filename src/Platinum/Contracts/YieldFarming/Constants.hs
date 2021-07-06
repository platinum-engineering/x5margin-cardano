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
{-# LANGUAGE OverloadedStrings          #-}

module Platinum.Contracts.YieldFarming.Constants where

import Plutus.V1.Ledger.Value (AssetClass, assetClass, tokenName)
import qualified PlutusTx
import PlutusTx.Prelude

-- | How far away requested withdrawal slot might be. In slots
requestValidityIntervalLength :: Integer
requestValidityIntervalLength = 20

{-# INLINABLE rewardPerSlot #-}
rewardPerSlot :: Integer
rewardPerSlot  = 100


{-# INLINABLE rewardAssetClass #-}
rewardAssetClass :: AssetClass
rewardAssetClass = assetClass "" "RWRD"

{-# INLINABLE threadToken #-}
threadToken :: AssetClass
threadToken = assetClass "" (tokenName "PLATINUM_YIELD_FARM_ST_01")

data StringConstants = StringConstants {
    scRewardAssetClass :: AssetClass,
    scThreadToken      :: AssetClass
}

{-# INLINABLE stringConstants #-}
stringConstants :: StringConstants
stringConstants = StringConstants
    { scRewardAssetClass = rewardAssetClass,
      scThreadToken = threadToken
    }

PlutusTx.makeLift ''StringConstants