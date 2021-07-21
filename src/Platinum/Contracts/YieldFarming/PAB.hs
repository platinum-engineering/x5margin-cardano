module Platinum.Contracts.YieldFarming.PAB where

import           Prelude

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)

import qualified Platinum.Contracts.YieldFarming.Env as YF
import qualified Platinum.Contracts.YieldFarming.OffChain as YF

data YieldFarmingContractPABActions
    = TestSetup
    -- ^ Emit mock tokens. Distribute them among mock wallets.
    | Init YF.InitLPParams
    | RunInstance YF.Env
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty YieldFarmingContractPABActions where
    pretty = viaShow