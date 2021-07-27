{-# LANGUAGE TupleSections #-}

module Platinum.Contracts.YieldFarming.OffChain
       ( ownerEndpoints
       , userEndpoints
       , initLP
       , InitLPParams (..)
       , AssetClassTransferParams (..)
       , PendingRewardParams (..)
       , HarvestParams (..)
       , TransferParams (..)
       , YFOwnerEndpoints
       , YFUserEndpoints
       , UserEndpointsReturn
       ) where

import qualified Prelude as P
import           Prelude                      (Show (..))
import           PlutusTx.Prelude

import qualified Data.Map                     as Map
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
import qualified Ledger.AddressMap            as Ledger
import           Ledger.Typed.Tx              (tyTxOutData, tyTxOutTxOut)
import           Plutus.V1.Ledger.Value       (assetClassValue, assetClassValueOf, toString, unAssetClass)
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
-- Off-chain endpoints
-------------------------------------------------

data AssetClassTransferParams = AssetClassTransferParams {
    actpToken  :: !AssetClass,
    actpAmount :: !Integer
} deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data HarvestParams = HarvestParams {
    hpToken :: !AssetClass
} deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PendingRewardParams = PendingRewardParams {
    prpPkh   :: PubKeyHash,
    prpToken :: !AssetClass
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
        MkTransfer pkh (assetClassValue actpToken actpAmount) curSlot
    let tn = toString $ snd $ unAssetClass actpToken
    logInfo $
        show pkh <> " deposited " <> show actpAmount <> " of " <> if P.null tn then "ADA" else tn
    pure (pkh, actpAmount)

withdraw :: Env -> AssetClassTransferParams -> Contract w s Text (PubKeyHash, Integer)
withdraw env AssetClassTransferParams{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    curSlot <- Contract.currentSlot
    void $ mapErrorSM $ runStep (yfClient env) $
        MkTransfer pkh (assetClassValue actpToken (-actpAmount)) curSlot
    let tn = toString $ snd $ unAssetClass actpToken
    logInfo $
        show pkh <> " withdrew " <> show actpAmount <> " of " <> if P.null tn then "ADA" else tn
    pure (pkh, actpAmount)

harvest :: Env -> HarvestParams -> Contract w s Text PubKeyHash
harvest env HarvestParams{..} = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    -- let tokenName = fromString $ if hpToken == "ADA" then "" else hpToken
    -- m <- mapErrorSM $ getOnChainState client
    curSlot <- Contract.currentSlot
    void $ mapErrorSM $ runStep (yfClient env) $
        MkHarvest pkh curSlot hpToken
    let tn = toString $ snd $ unAssetClass hpToken
    logInfo $
        show pkh <> " harvested rewards from " <> if P.null tn then "ADA" else tn <> " pool"
    pure pkh

pendingReward :: Env -> PendingRewardParams -> Contract w s Text Value
pendingReward env PendingRewardParams{..} = do
    let ac = prpToken
    let tn = toString $ snd $ unAssetClass ac
    curSlot <- Contract.currentSlot
    m <- mapErrorSM $ getOnChainState (yfClient env)
    case m of
        Nothing          -> do
            logError @P.String "No running contract found"
            pure mempty
        Just ((o, _), _) -> do
            let datum = tyTxOutData o
            let poolFunds = txOutValue $ tyTxOutTxOut o
            let totalSupplyInPool = assetClassValueOf poolFunds ac
            case transferNRewardUser env prpPkh (ac, 0) curSlot (datum, totalSupplyInPool) of
                Nothing -> do
                    logInfo @P.String "Couldn't compute pending reward"
                    pure mempty
                Just (_, reward, _) -> do
                    logInfo $
                        show prpPkh <> " pending reward in " <> if P.null tn then "ADA" else tn <> ": " <> show reward
                    pure reward

getUserStakes :: Env -> PubKeyHash -> Contract w s Text Value
getUserStakes env pkh = do
    m <- mapErrorSM $ getOnChainState (yfClient env)
    case m of
        Nothing          -> do
            logError @P.String "No running contract found"
            pure mempty
        Just ((o, _), _) -> do
            let users = yfdUsers (tyTxOutData o)
            let res = maybe mempty uiAmounts $ AMap.lookup pkh users
            logInfo $
                show pkh <> " stakes: " <> show res
            pure res

contractBalance :: Env -> Contract w s Text Value
contractBalance env = do
    let scrAddr = yieldFarmingAddress env
    utxo <- utxoAt scrAddr
    let res = snd $ head $ Map.toList $ Ledger.values $ Ledger.AddressMap $ Map.singleton scrAddr utxo
    logInfo $ "Script balance: " <> show res
    pure res

walletPubKey :: Contract w s Text PubKeyHash
walletPubKey = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    logInfo $ "Wallet public key: " <> show pkh
    pure pkh

-- transfer :: Env -> TransferParams -> Contract w s Text ()
-- transfer env TransferParams{..} = do
--     pkh <- pubKeyHash <$> Contract.ownPubKey
--     curSlot <- Contract.currentSlot
--     void $ mapErrorSM $ runStep (yfClient env) $
--         MkTransfer pkh actpAmounts curSlot

-------------------------------------------------
-- Off-chain machinery
-------------------------------------------------

type YFOwnerEndpoints =
        Endpoint "init" InitLPParams

type YFUserEndpoints =
        Endpoint "deposit"  AssetClassTransferParams
    .\/ Endpoint "withdraw" AssetClassTransferParams
    .\/ Endpoint "pendingReward" PendingRewardParams
    .\/ Endpoint "harvest" HarvestParams
    .\/ Endpoint "userStakes" PubKeyHash
    .\/ Endpoint "contractBalance" ()
    .\/ Endpoint "publicKey" ()
    -- .\/ Endpoint "transfer" TransferParams

ownerEndpoints :: Contract (Last Env) YFOwnerEndpoints Text ()
ownerEndpoints = initLP' >> ownerEndpoints
  where
    initLP' = handleError logError $ endpoint @"init"  >>= initLP

data UserEndpointsReturn
    = Deposit PubKeyHash Integer
    | Withdraw PubKeyHash Integer
    | ContractBalance Value
    | PendingReward Value
    | Harvest PubKeyHash
    | UserStakes Value
    | PublicKey PubKeyHash
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

userEndpoints :: Env -> Contract (Last UserEndpointsReturn) YFUserEndpoints Text ()
userEndpoints env =
    (wrapEndp @"deposit"         (P.uncurry Deposit)  deposit `select`
     wrapEndp @"withdraw"        (P.uncurry Withdraw) withdraw  `select`
     wrapEndp @"pendingReward"   PendingReward        pendingReward `select`
     wrapEndp @"harvest"         Harvest              harvest `select`
     wrapEndp @"userStakes"      UserStakes           getUserStakes `select`
     wrapEndp @"contractBalance" ContractBalance      (const . contractBalance) `select`
     wrapEndp @"publicKey"       PublicKey            (const $ const $ walletPubKey)
    ) >>
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