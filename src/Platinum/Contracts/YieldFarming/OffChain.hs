{-# LANGUAGE TupleSections #-}

module Platinum.Contracts.YieldFarming.OffChain
       ( ownerEndpoints
       , userEndpoints
       , initLP
       , InitLPParams (..)
       , AssetClassTransferParams (..)
       , TransferParams (..)
       , YFOwnerEndpoints
       , YFUserEndpoints
       ) where

import qualified Prelude as P
import           Prelude                      (Show (..))
import           PlutusTx.Prelude

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Text                    (Text, pack)
import           Data.Monoid                  (Last (..))
import           GHC.Generics                 (Generic)
import           Control.Monad                hiding (fmap)
import           Schema                       (ToSchema)

import           Plutus.V1.Ledger.Value       (assetClass)
import           Plutus.Contract.StateMachine

import           Plutus.Contract              as Contract
import qualified PlutusTx.AssocMap            as AMap
import           Ledger                       hiding (singleton)
import           Plutus.V1.Ledger.Value       (assetClassValue, toString, unAssetClass)
import qualified Plutus.Contracts.Currency     as UC

import           Platinum.Contracts.YieldFarming.OnChain
import           Platinum.Contracts.YieldFarming.Env (Env (..), threadTokenName, rewardTokenName)

yfClient :: Env -> StateMachineClient YieldFarmingDatum YieldFarmingRedeemer
yfClient env =
    mkStateMachineClient $
    StateMachineInstance
        (yieldFarmingStateMachine env)
        (yieldFarmingInstance env)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

mapErrorC :: Contract w s UC.CurrencyError a -> Contract w s Text a
mapErrorC = mapError $ pack . show

-------------------------------------------------
-- Off-chain endpoints
-------------------------------------------------

data InitLPParams = InitLPParams {
    ilpAssetClasses :: [AssetClass]
} deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

initLP
    :: InitLPParams
    -> Contract (Last Env) s Text ()
initLP InitLPParams{..} = do
    ownerPkh <- pubKeyHash <$> Contract.ownPubKey

    -- Initial emission of reward tokens and create NFT thread token
    let totRewardAmount = 1111111111
    currencySymbol <- UC.currencySymbol <$>
                      mapErrorC (UC.forgeContract ownerPkh [(rewardTokenName, totRewardAmount), (threadTokenName, 1)])
    let rewardAssetClass = assetClass currencySymbol rewardTokenName
    let threadAssetClass = assetClass currencySymbol threadTokenName
    logInfo $ "Reward tokens and thread NFT tokens minted " ++ show (rewardAssetClass, threadAssetClass)

    -- Create pool
    -- TODO check that no RWRD asset class among ilpAssetClasses
    curSlot <- Contract.currentSlot
    let env = Env {
        envOwner = ownerPkh,
        envRewardToken = rewardAssetClass,
        envThreadToken = threadAssetClass
    }
    tell $ Last $ Just env
    let emptyPool = emptyPoolInfo curSlot
    let yfInitDatum =
           YieldFarmingDatum
           { yfdPools = AMap.fromList $ map (, emptyPool) ilpAssetClasses,
             yfdUsers = AMap.empty
           }
    void $ mapErrorSM $ runInitialise (yfClient env) yfInitDatum (assetClassValue rewardAssetClass totRewardAmount)
    logInfo $ "Started liquidity pool with constant amount of reward tokens " ++ show yfInitDatum

-------------------------------------------------
-- On-chain endpoints
-------------------------------------------------

data AssetClassTransferParams = AssetClassTransferParams {
    actpAC     :: !AssetClass,
    actpAmount :: !Integer
} deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data TransferParams = TransferParams {
    actpAmounts :: !Value
} deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

deposit :: Env -> AssetClassTransferParams -> Contract w s Text (PubKeyHash, Integer)
deposit env AssetClassTransferParams{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    curSlot <- Contract.currentSlot
    void $ mapErrorSM $ runStep (yfClient env) $
        MkTransfer pkh (assetClassValue actpAC actpAmount) curSlot
    let tn = toString $ snd $ unAssetClass actpAC
    logInfo $
        show pkh <> " deposited " <> show actpAmount <> " of " <> if P.null tn then "ADA" else tn
    pure (pkh, actpAmount)

withdraw :: Env -> AssetClassTransferParams -> Contract w s Text (PubKeyHash, Integer)
withdraw env AssetClassTransferParams{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    curSlot <- Contract.currentSlot
    void $ mapErrorSM $ runStep (yfClient env) $
        MkTransfer pkh (assetClassValue actpAC (-actpAmount)) curSlot
    let tn = toString $ snd $ unAssetClass actpAC
    logInfo $
        show pkh <> " withdrew " <> show actpAmount <> " of " <> if P.null tn then "ADA" else tn
    pure (pkh, actpAmount)

transfer :: Env -> TransferParams -> Contract w s Text ()
transfer env TransferParams{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    curSlot <- Contract.currentSlot
    void $ mapErrorSM $ runStep (yfClient env) $
        MkTransfer pkh actpAmounts curSlot

type YFOwnerEndpoints =
        Endpoint "init" InitLPParams

type YFUserEndpoints =
        Endpoint "deposit"  AssetClassTransferParams
    .\/ Endpoint "withdraw" AssetClassTransferParams
    .\/ Endpoint "transfer" TransferParams

ownerEndpoints :: Contract (Last Env) YFOwnerEndpoints Text ()
ownerEndpoints = initLP' >> ownerEndpoints
  where
    initLP' = handleError logError $ endpoint @"init"  >>= initLP

data UserEndpointsReturn
    = Deposited PubKeyHash Integer
    | Withdrew PubKeyHash Integer
    deriving (Show, Generic, FromJSON, ToJSON)

userEndpoints :: Env -> Contract (Last UserEndpointsReturn) YFUserEndpoints Text ()
userEndpoints env =
    (wrapEndp @"deposit"  (P.uncurry Deposited)  deposit `select`
     wrapEndp @"withdraw" (P.uncurry Withdrew) withdraw) >>
    userEndpoints env
  where
    wrapEndp
      :: forall l p a . (HasEndpoint l p YFUserEndpoints, FromJSON p)
      => (a -> UserEndpointsReturn)
      -> (Env -> p -> Contract (Last UserEndpointsReturn) YFUserEndpoints Text a)
      -> Contract (Last UserEndpointsReturn) YFUserEndpoints Text ()
    wrapEndp wrapRet endp = handleError logError $ do
        p <- endpoint @l
        res <- endp env p
        tell $ Last $ Just $ wrapRet res

-- mkSchemaDefinitions ''YFEndpoints

-- myToken :: KnownCurrency
-- myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

-- mkKnownCurrencies ['myToken]