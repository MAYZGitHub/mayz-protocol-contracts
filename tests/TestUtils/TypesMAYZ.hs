{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3
module TestUtils.TypesMAYZ where

--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Control.Monad.IO.Class  as MonadIO
import qualified Control.Monad.Reader    as MReader
import qualified Data.List               as DataList
import           Prelude                 as P
import qualified Text.Read               as TextRead

-- IOG imports
import qualified Ledger
import qualified Ledger.Address          as LedgerAddress
import qualified Plutus.V2.Ledger.Api    as LedgerApiV2
import qualified PlutusTx
import qualified Wallet.Emulator.Types   as WalletEmulatorTypes (XPrv)

-- Project imports
import qualified Generic.Types           as T
import qualified Protocol.Protocol.Types as ProtocolT
import           TestUtils.Types

--------------------------------------------------------------------------------
-- App Monad
--------------------------------------------------------------------------------
newtype AppConfig
    = AppConfig { appTestParams :: TestParams }

newtype AppM a
    = AppM { runApp :: MReader.ReaderT AppConfig IO a }
    deriving (Applicative, Functor, Monad, MonadIO.MonadIO, MReader.MonadReader AppConfig)

--------------------------------------------------------------------------------
data TestCompiledCodeScripts
    = TestCompiledCodeScripts
          { tccsProtocolPolicyID_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsProtocolValidator_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsScriptPolicyID_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsScriptValidator_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsSwapOfferPolicyID_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsSwapOfferValidator_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsBuyOrderPolicyID_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsBuyOrderValidator_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsDelegationPolicyID_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsDelegationValidator_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsInvestUnitValidator_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsFundPolicy_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsFundValidator_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsFundHoldingPolicyID_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          , tccsFundHoldingValidator_Pre :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
          }

instance Show TestCompiledCodeScripts where
  show _ = "<TestCompiledCodeScripts>"

data TestParams
    = TestParams
          { tpProtocolPolicyID                           :: LedgerApiV2.MintingPolicy
          , tpProtocolPolicyID_CS                        :: LedgerApiV2.CurrencySymbol
          , tpProtocolValidator                          :: LedgerApiV2.Validator
          , tpProtocolValidator_Hash                     :: LedgerApiV2.ValidatorHash
          , tpScriptPolicyID                             :: LedgerApiV2.MintingPolicy
          , tpScriptPolicyID_CS                          :: LedgerApiV2.CurrencySymbol
          , tpScriptValidator                            :: LedgerApiV2.Validator
          , tpScriptValidator_Hash                       :: LedgerApiV2.ValidatorHash
          , tpSwapOfferPolicyID                          :: LedgerApiV2.MintingPolicy
          , tpSwapOfferPolicyID_CS                       :: LedgerApiV2.CurrencySymbol
          , tpSwapOfferValidator                         :: LedgerApiV2.Validator
          , tpSwapOfferValidator_Hash                    :: LedgerApiV2.ValidatorHash
          , tpBuyOrderPolicyID                           :: LedgerApiV2.MintingPolicy
          , tpBuyOrderPolicyID_CS                        :: LedgerApiV2.CurrencySymbol
          , tpBuyOrderValidator                          :: LedgerApiV2.Validator
          , tpBuyOrderValidator_Hash                     :: LedgerApiV2.ValidatorHash
          , tpDelegationPolicyID                         :: LedgerApiV2.MintingPolicy
          , tpDelegationPolicyID_CS                      :: LedgerApiV2.CurrencySymbol
          , tpDelegationValidator                        :: LedgerApiV2.Validator
          , tpDelegationValidator_Hash                   :: LedgerApiV2.ValidatorHash
          , tpInvestUnitValidator                        :: LedgerApiV2.Validator
          , tpInvestUnitValidator_Hash                   :: LedgerApiV2.ValidatorHash
          , tpFundPolicy                                 :: LedgerApiV2.MintingPolicy
          , tpFundPolicy_CS                              :: LedgerApiV2.CurrencySymbol
          , tpFundValidator                              :: LedgerApiV2.Validator
          , tpFundValidator_Hash                         :: LedgerApiV2.ValidatorHash
          , tpFundHoldingPolicyID                        :: LedgerApiV2.MintingPolicy
          , tpFundHoldingPolicyID_CS                     :: LedgerApiV2.CurrencySymbol
          , tpFundHoldingValidator                       :: LedgerApiV2.Validator
          , tpFundHoldingValidator_Hash                  :: LedgerApiV2.ValidatorHash
          , tpProtocolPolicyID_TxOutRef                  :: Ledger.TxOutRef
          , tpFundPolicy_TxOutRef                        :: Ledger.TxOutRef
          , tpTokenEmergencyAdminPolicy_CS               :: LedgerApiV2.CurrencySymbol
          , tpTokenAdminPolicy_CS                        :: LedgerApiV2.CurrencySymbol
          , tpFundFT_TN                                  :: LedgerApiV2.TokenName
          , tpTokenMAYZ_CS                               :: LedgerApiV2.CurrencySymbol
          , tpTokenMAYZ_TN                               :: LedgerApiV2.TokenName
          , tpProtocolAdmins                             :: [T.WalletPaymentPKH]
          , tpFundAdmins                                 :: [T.WalletPaymentPKH]
          , tpDelegatorsAdmins                           :: [T.WalletPaymentPKH]
          , tpSwapOfferAdmin                             :: T.WalletPaymentPKH
          , tpDelegationAdmin                            :: T.WalletPaymentPKH  
          , tpOraclePrivateKey                           :: WalletEmulatorTypes.XPrv
          , tpOraclePaymentPubKey                        :: LedgerAddress.PaymentPubKey
          , tpFundCategory                               :: ProtocolT.FundCategory
          , tpFundLifeTime                               :: ProtocolT.MinMaxDef LedgerApiV2.POSIXTime
          , tpRequiredMAYZForSwapOffer                   :: Integer
          , tpRequiredMAYZForBuyOrder                    :: Integer
          , tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 :: ProtocolT.MinMaxDef Integer
          , tp_MinMaxDef_CommissionSwapOffer_InBPx1e3    :: ProtocolT.MinMaxDef Integer
          , tp_MinMaxDef_CommissionBuyOrder_InBPx1e3     :: ProtocolT.MinMaxDef Integer
          , tpShare_InBPx1e2_Protocol                    :: Integer
          , tpShare_InBPx1e2_Managers                    :: Integer
          , tpShare_InBPx1e2_Delegators                  :: Integer
          , tpBeginAt                                    :: LedgerApiV2.POSIXTime
          , tpDeadline                                   :: LedgerApiV2.POSIXTime
          , tpClosedAt                                   :: Maybe LedgerApiV2.POSIXTime
          , tpCommission_PerYear_InBPx1e3                  :: Integer
          , tpCommissions_Table_Numerator_1e6              :: [Integer]
          , tpTransactionDate                            :: LedgerApiV2.POSIXTime
          , tpDepositDate                                :: LedgerApiV2.POSIXTime
          , tpWithdrawDate                               :: LedgerApiV2.POSIXTime
          , tpCollectCommissionsDate                     :: LedgerApiV2.POSIXTime
          , tpReIdxDate                                  :: LedgerApiV2.POSIXTime
          }
    deriving (Eq, Show)

instance Eq WalletEmulatorTypes.XPrv where
  (==) :: WalletEmulatorTypes.XPrv -> WalletEmulatorTypes.XPrv -> Bool
  _ == _ = True

instance Show WalletEmulatorTypes.XPrv where
  show :: WalletEmulatorTypes.XPrv -> String
  show _ = "<XPrv>"

-----------------------------------------------------------------
data TestEntity = Protocol_TestEntity | ProtocolTxoutRefNeeded_TestEntity | Fund_TestEntity | FundHolding_TestEntity | InvestUnit_TestEntity | SwapOffer_TestEntity deriving (P.Show)

instance Pretty TestEntity where
  pretty entity = stripSuffix "_TestEntity" (P.show entity)

data TestToken = ProtocolID_TestToken | FundID_TestToken | InvestUnitID_TestToken | FundFT_TestToken | FundHoldingID_TestToken | SwapOfferID_TestToken deriving (P.Show)

instance Pretty TestToken where
  pretty token = stripSuffix "_TestToken" (P.show token)

data PolicyTestRedeemer = SwapOffer_MintID_TestRedeemer | SwapOffer_BurnID_TestRedeemer | Fund_MintID_TestRedeemer | Fund_BurnID_TestRedeemer | Fund_MintFT_TestRedeemer | Fund_BurnFT_TestRedeemer | Script_MintID_TestRedeemer | Script_BurnID_TestRedeemer | BuyOrder_MintID_TestRedeemer | BuyOrder_BurnID_TestRedeemer | Protocol_MintID_TestRedeemer | Delegation_MintID_TestRedeemer | Delegation_BurnID_TestRedeemer | FundHolding_MintID_TestRedeemer | FundHolding_BurnID_TestRedeemer deriving
        ( P.Eq
        , P.Read
        , P.Show
        )

instance Pretty PolicyTestRedeemer where
  pretty redeemer = stripSuffix "_TestRedeemer" (P.show redeemer)

data ValidatorTestRedeemer = SwapOffer_Delete_TestRedeemer | SwapOffer_UpdateStatus_TestRedeemer | SwapOffer_UpdateAskedCommissionRate_TestRedeemer | SwapOffer_UpdateSellRestrictions_TestRedeemer | SwapOffer_UpdateMinADA_TestRedeemer | SwapOffer_Deposit_TestRedeemer | SwapOffer_Withdraw_TestRedeemer | SwapOffer_SwapFTxADA_TestRedeemer | SwapOffer_SwapADAxFT_TestRedeemer | SwapOffer_Emergency_TestRedeemer | Fund_Delete_TestRedeemer | Fund_DatumUpdate_TestRedeemer | Fund_UpdateMinADA_TestRedeemer | Fund_FundHoldingAdd_TestRedeemer | Fund_FundHoldingDelete_TestRedeemer | Fund_Emergency_TestRedeemer | Fund_Finish_TestRedeemer | BuyOrder_Delete_TestRedeemer | BuyOrder_UpdateStatus_TestRedeemer | BuyOrder_UpdateMinADA_TestRedeemer | BuyOrder_Deposit_TestRedeemer | BuyOrder_Withdraw_TestRedeemer | BuyOrder_FillOrder_TestRedeemer | Protocol_DatumUpdate_TestRedeemer | Protocol_UpdateMinADA_TestRedeemer | Protocol_Emergency_TestRedeemer | InvestUnit_ReIndexing_TestRedeemer | InvestUnit_UpdateMinADA_TestRedeemer | InvestUnit_Emergency_TestRedeemer | InvestUnit_Delete_TestRedeemer | FundHolding_UpdateMinADA_TestRedeemer | FundHolding_Deposit_TestRedeemer | FundHolding_Withdraw_TestRedeemer | FundHolding_Collect_Protocol_Commission_TestRedeemer | FundHolding_Collect_Delegators_Commission_TestRedeemer | FundHolding_Collect_Managers_Commission_TestRedeemer | FundHolding_ReIndexing_TestRedeemer | FundHolding_BalanceAssets_TestRedeemer | FundHolding_Emergency_TestRedeemer | FundHolding_Delete_TestRedeemer | Delegation_Delete_TestRedeemer | Delegation_Deposit_TestRedeemer | Delegation_Withdraw_TestRedeemer | Delegation_UpdateMinADA_TestRedeemer | Script_Delete_TestRedeemer | Generic_TestRedeemer deriving
        ( P.Eq
        , P.Read
        , P.Show
        )

instance Pretty ValidatorTestRedeemer where
  pretty redeemer = stripSuffix "_TestRedeemer" (P.show redeemer)

-- Helper function to convert string into the appropriate ValidatorTestRedeemer
getValidatorTestRedeemer :: Maybe String -> Maybe ValidatorTestRedeemer
getValidatorTestRedeemer (Just str) =
  TextRead.readMaybe (str ++ "_TestRedeemer")
getValidatorTestRedeemer _ = Nothing

getPolicyTestRedeemer :: Maybe String -> Maybe PolicyTestRedeemer
getPolicyTestRedeemer (Just str) = TextRead.readMaybe (str ++ "_TestRedeemer")
getPolicyTestRedeemer _          = Nothing

-----------------------------------------------------------------

data TestTransactions = Protocol_Create_Tx | Protocol_DatumUpdate_Tx | Protocol_UpdateMinADA_Tx | Protocol_Emergency_Tx | Fund_Create_Tx | Fund_DatumUpdate_Tx | Fund_UpdateMinADA_Tx | Fund_Deposit_Tx | Fund_Withdraw_Tx | Fund_ReIndexing_Tx | Fund_Finish_Tx | Fund_Emergency_Tx | Fund_Delete_Tx | FundHolding_Create_Tx | FundHolding_UpdateMinADA_Tx | FundHolding_Collect_Protocol_Commission_Tx | FundHolding_Collect_Managers_Commission_Tx | FundHolding_Collect_Delegators_Commission_Tx | FundHolding_BalanceAssets_Tx | FundHolding_Emergency_Tx | FundHolding_Delete_Tx | InvestUnit_UpdateMinADA_Tx | InvestUnit_Emergency_Tx | SwapOffer_Create_Tx | SwapOffer_UpdateStatus_Tx | SwapOffer_UpdateAskedCommissionRate_Tx | SwapOffer_UpdateSellRestrictions_Tx | SwapOffer_UpdateMinADA_Tx | SwapOffer_Deposit_Tx | SwapOffer_Withdraw_Tx | SwapOffer_SwapFTxADA_Tx | SwapOffer_SwapADAxFT_Tx | SwapOffer_Emergency_Tx | SwapOffer_Delete_Tx deriving
        ( P.Show
        )

-----------------------------------------------------------------
data RedeemerLog
    = RedeemerLogValidator (Maybe ValidatorTestRedeemer)
    | RedeemerLogPolicy (Maybe PolicyTestRedeemer)
    deriving (P.Eq)

instance P.Show RedeemerLog where
  show = getRedeemerNameFromLog

getRedeemerNameFromLog :: RedeemerLog -> String
getRedeemerNameFromLog (RedeemerLogValidator (Just r)) = pretty r
getRedeemerNameFromLog (RedeemerLogPolicy (Just r))    = pretty r
-- getRedeemerNameFromLog (RedeemerLogValidator Nothing) = "Nothing"
-- getRedeemerNameFromLog (RedeemerLogPolicy Nothing) = "Nothing"
-- getRedeemerNameFromLog s = error $ "Redeemer not found "
getRedeemerNameFromLog _                               = error "Redeemer not found "

-- Function to extract the base name from a redeemer name
extractBaseName :: String -> String
extractBaseName redeemerName =
  case DataList.break (== '_') redeemerName of
    (baseName, _) -> baseName

getRedeemerScriptNameFromLog :: RedeemerLog -> String
getRedeemerScriptNameFromLog (RedeemerLogValidator (Just r)) =
  let baseName = extractBaseName (pretty r)
   in baseName ++ "_Validator"
getRedeemerScriptNameFromLog (RedeemerLogPolicy (Just r)) =
  let baseName = extractBaseName (pretty r)
   in baseName ++ "_Policy"
getRedeemerScriptNameFromLog s = error $ "Redeemer not found " ++ show s
-----------------------------------------------------------------
