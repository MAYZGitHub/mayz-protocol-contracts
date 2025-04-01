{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.PABTypes where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics as GHCGenerics (Generic)
import qualified Ledger
import qualified Ledger.Address as LedgerAddress
import qualified Ledger.Crypto as Crypto
import qualified Ledger.Value as LedgerValue
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import qualified Schema
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Generic.Types as T
import qualified Protocol.BuyOrder.Types as BuyOrderT
import qualified Protocol.Delegation.Types as DelegationT
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.Fund.InvestUnit.Types as InvestUnitT
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Script.Types as ScriptT
import qualified Protocol.SwapOffer.Types as SwapOfferT
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

data ProtocolPABParams = ProtocolPABParams
    { pppProtocolVersion :: Integer
    , pppProtocolPolicyID_Params :: ProtocolT.PolicyParams
    , pppProtocolPolicyID :: LedgerApiV2.MintingPolicy
    , pppProtocolPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , pppProtocolValidator_Params :: ProtocolT.ValidatorParams
    , pppProtocolValidator :: LedgerApiV2.Validator
    , pppProtocolValidator_Hash :: LedgerApiV2.ValidatorHash
    , pppProtocolValidator_Address :: LedgerAddress.Address
    , pppScriptPolicyID_Params :: ScriptT.PolicyParams
    , pppScriptPolicyID :: LedgerApiV2.MintingPolicy
    , pppScriptPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , pppScriptValidator_Params :: ScriptT.ValidatorParams
    , pppScriptValidator :: LedgerApiV2.Validator
    , pppScriptValidator_Hash :: LedgerApiV2.ValidatorHash
    , pppScriptValidator_Address :: LedgerAddress.Address
    , pppInvestUnitValidator_Params :: InvestUnitT.ValidatorParams
    , pppInvestUnitValidator :: LedgerApiV2.Validator
    , pppInvestUnitValidator_Hash :: LedgerApiV2.ValidatorHash
    , pppInvestUnitValidator_Address :: LedgerAddress.Address
    , pppFundFactoryPABParams :: [FundFactoryPABParams]
    , pppTokenEmergencyAdmin_CS :: LedgerApiV2.CurrencySymbol
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data FundFactoryPABParams = FundFactoryPABParams
    { ffppFundVersion :: Integer
    , ffppFundValidator_Params :: FundT.ValidatorParams
    , ffppFundValidator :: LedgerApiV2.Validator
    , ffppFundValidator_Hash :: LedgerApiV2.ValidatorHash
    , ffppFundValidator_Address :: LedgerAddress.Address
    , ffppFundPABParams :: [FundPABParams]
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, Schema.ToSchema)

instance P.Show FundFactoryPABParams where
    show fp = "Fund Factory Params: " ++ P.show (ffppFundVersion fp)

--------------------------------------------------------------------------------2

data FundPABParams = FundPABParams
    { fppFundVersion :: Integer
    , fppFundPolicy_Params :: FundT.PolicyParams
    , fppFundPolicy :: LedgerApiV2.MintingPolicy
    , fppFundPolicy_CS :: LedgerApiV2.CurrencySymbol
    , fppFundValidator_Params :: FundT.ValidatorParams
    , fppFundValidator :: LedgerApiV2.Validator
    , fppFundValidator_Hash :: LedgerApiV2.ValidatorHash
    , fppFundValidator_Address :: LedgerAddress.Address
    , fppFundHoldingPolicyID_Params :: FundHoldingT.PolicyParams
    , fppFundHoldingPolicyID :: LedgerApiV2.MintingPolicy
    , fppFundHoldingPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , fppFundHoldingValidator_Params :: FundHoldingT.ValidatorParams
    , fppFundHoldingValidator :: LedgerApiV2.Validator
    , fppFundHoldingValidator_Hash :: LedgerApiV2.ValidatorHash
    , fppFundHoldingValidator_Address :: LedgerAddress.Address
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, Schema.ToSchema)

instance P.Show FundPABParams where
    show fp = "FundPABParams: " ++ P.show (fppFundPolicy_CS fp)

--------------------------------------------------------------------------------2

data ProtocolDeployParams = ProtocolDeployParams
    { pdpProtocolVersion :: Integer
    , pdpTokenEmergencyAdmin_CS :: LedgerApiV2.CurrencySymbol
    , pdpProtocolPolicyID_Params :: ProtocolT.PolicyParams
    , pdpProtocolPolicyID_CborHex :: P.String
    , pdpProtocolPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , pdpProtocolValidator_Params :: ProtocolT.ValidatorParams
    , pdpProtocolValidator_Hash :: LedgerApiV2.ValidatorHash
    , pdpProtocolValidator_CborHex :: P.String
    , pdpProtocolValidator_AddressTestnet :: P.String
    , pdpProtocolValidator_AddressMainnet :: P.String
    , pdpScriptPolicyID_Params :: ScriptT.PolicyParams
    , pdpScriptPolicyID_CborHex :: P.String
    , pdpScriptPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , pdpScriptValidator_Params :: ScriptT.ValidatorParams
    , pdpScriptValidator_Hash :: LedgerApiV2.ValidatorHash
    , pdpScriptValidator_CborHex :: P.String
    , pdpScriptValidator_AddressTestnet :: P.String
    , pdpScriptValidator_AddressMainnet :: P.String
    , pdpSwapOfferPolicyID_Params :: SwapOfferT.PolicyParams
    , pdpSwapOfferPolicyID_CborHex :: P.String
    , pdpSwapOfferPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , pdpSwapOfferValidator_Params :: SwapOfferT.ValidatorParams
    , pdpSwapOfferValidator_Hash :: LedgerApiV2.ValidatorHash
    , pdpSwapOfferValidator_CborHex :: P.String
    , pdpSwapOfferValidator_AddressTestnet :: P.String
    , pdpSwapOfferValidator_AddressMainnet :: P.String
    , pdpBuyOrderPolicyID_Params :: BuyOrderT.PolicyParams
    , pdpBuyOrderPolicyID_CborHex :: P.String
    , pdpBuyOrderPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , pdpBuyOrderValidator_Params :: BuyOrderT.ValidatorParams
    , pdpBuyOrderValidator_Hash :: LedgerApiV2.ValidatorHash
    , pdpBuyOrderValidator_CborHex :: P.String
    , pdpBuyOrderValidator_AddressTestnet :: P.String
    , pdpBuyOrderValidator_AddressMainnet :: P.String
    , pdpDelegationPolicyID_Params :: DelegationT.PolicyParams
    , pdpDelegationPolicyID_CborHex :: P.String
    , pdpDelegationPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , pdpDelegationValidator_Params :: DelegationT.ValidatorParams
    , pdpDelegationValidator_Hash :: LedgerApiV2.ValidatorHash
    , pdpDelegationValidator_CborHex :: P.String
    , pdpDelegationValidator_AddressTestnet :: P.String
    , pdpDelegationValidator_AddressMainnet :: P.String
    , pdpInvestUnitValidator_Params :: InvestUnitT.ValidatorParams
    , pdpInvestUnitValidator_Hash :: LedgerApiV2.ValidatorHash
    , pdpInvestUnitValidator_CborHex :: P.String
    , pdpInvestUnitValidator_AddressTestnet :: P.String
    , pdpInvestUnitValidator_AddressMainnet :: P.String
    , pdpFundFactoryDeployParams :: [FundFactoryDeployParams]
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data FactoryDeployParams = FactoryDeployParams
    { fpProtocolVersion :: Integer
    , fpFundVersion :: Integer
    , fpSwapOfferVersion :: Integer
    , fpBuyOrderVersion :: Integer
    , fpDelegationVersion :: Integer
    , fpScriptVersion :: Integer
    , fpProtocolPolicyID_Pre_CborHex :: P.String
    , fpProtocolValidator_Pre_CborHex :: P.String
    , fpScriptPolicyID_Pre_CborHex :: P.String
    , fpScriptValidator_Pre_CborHex :: P.String
    , fpSwapOfferPolicyID_Pre_CborHex :: P.String
    , fpSwapOfferValidator_Pre_CborHex :: P.String
    , fpBuyOrderPolicyID_Pre_CborHex :: P.String
    , fpBuyOrderValidator_Pre_CborHex :: P.String
    , fpDelegationPolicyID_Pre_CborHex :: P.String
    , fpDelegationValidator_Pre_CborHex :: P.String
    , fpInvestUnitValidator_Pre_CborHex :: P.String
    , fpFundPolicy_Pre_CborHex :: P.String
    , fpFundValidator_Pre_CborHex :: P.String
    , fpFundHoldingPolicyID_Pre_CborHex :: P.String
    , fpFundHoldingValidator_Pre_CborHex :: P.String
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data FundFactoryDeployParams = FundFactoryDeployParams
    { ffdpFundVersion :: Integer
    , ffdpFundPolicy_Pre_CborHex :: P.String
    , ffdpFundValidator_Pre_CborHex :: P.String
    , ffdpFundHoldingPolicyID_Pre_CborHex :: P.String
    , ffdpFundHoldingValidator_Pre_CborHex :: P.String
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data FundDeployParams = FundDeployParams
    { fdpFundVersion :: Integer
    , fdpFundPolicy_Params :: FundT.PolicyParams
    , fdpFundPolicy_CborHex :: P.String
    , fdpFundPolicy_CS :: LedgerApiV2.CurrencySymbol
    , fdpFundValidator_Params :: FundT.ValidatorParams
    , fdpFundValidator_Hash :: LedgerApiV2.ValidatorHash
    , fdpFundValidator_CborHex :: P.String
    , fdpFundValidator_AddressTestnet :: P.String
    , fdpFundValidator_AddressMainnet :: P.String
    , fdpFundHoldingPolicyID_Params :: FundHoldingT.PolicyParams
    , fdpFundHoldingPolicyID_CborHex :: P.String
    , fdpFundHoldingPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , fdpFundHoldingValidator_Params :: FundHoldingT.ValidatorParams
    , fdpFundHoldingValidator_Hash :: LedgerApiV2.ValidatorHash
    , fdpFundHoldingValidator_CborHex :: P.String
    , fdpFundHoldingValidator_AddressTestnet :: P.String
    , fdpFundHoldingValidator_AddressMainnet :: P.String
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data DeployAllPreParams = DeployAllPreParams
    {
        dapProtocolVersion :: Integer
    , dapFundVersion :: Integer
    , dapSwapOfferVersion :: Integer
    , dapBuyOrderVersion :: Integer
    , dapDelegationVersion :: Integer
    , dapScriptVersion :: Integer
    , dapProtocolPolicyID_Pre_CborHex :: P.String
    , dapProtocolValidator_Pre_CborHex :: P.String
    , dapScriptPolicyID_Pre_CborHex :: P.String
    , dapScriptValidator_Pre_CborHex :: P.String
    , dapSwapOfferPolicyID_Pre_CborHex :: P.String
    , dapSwapOfferValidator_Pre_CborHex :: P.String
    , dapBuyOrderPolicyID_Pre_CborHex :: P.String
    , dapBuyOrderValidator_Pre_CborHex :: P.String
    , dapDelegationPolicyID_Pre_CborHex :: P.String
    , dapDelegationValidator_Pre_CborHex :: P.String
    , dapInvestUnitValidator_Pre_CborHex :: P.String
    , dapFundPolicy_Pre_CborHex :: P.String
    , dapFundValidator_Pre_CborHex :: P.String
    , dapFundHoldingPolicyID_Pre_CborHex :: P.String
    , dapFundHoldingValidator_Pre_CborHex :: P.String
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

newtype PABSplitUtxOParams = PABSplitUtxOParams {psupSplitAmount :: Integer}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABMintFTParams = PABMintFTParams
    { pmfpPolicyNum :: Integer
    , pmfpTokenNameBase :: BuiltinByteString
    , pmfpDiifTokenNameCount :: Integer
    , pmfpAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABMintNFTParams = PABMintNFTParams
    { pmnpTokenNameBase :: BuiltinByteString
    , pmnpDiifTokenNameCount :: Integer
    , pmnpAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
data PABMintFundTokensParams = PABMintFundTokensParams
    { pmftpProtocolPABParams :: ProtocolPABParams
    , pmftpFundPABParams :: FundPABParams
    , pmftpAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABProtocolPrepareParams = PABProtocolPrepareParams
    { ppppProtocolPABParams :: ProtocolPABParams
    , ppppOraclePaymentPubKey :: LedgerAddress.PaymentPubKey
    , ppppAdmins :: [T.WalletPaymentPKH]
    , ppppTokenAdminPolicy_CS :: T.CS
    , ppppFundCategories :: [ProtocolT.FundCategory]
    , ppppFundLifeTime :: ProtocolT.MinMaxDef LedgerApiV2.POSIXTime
    , ppppRequiredMAYZForSwapOffer :: Integer
    , ppppRequiredMAYZForBuyOrder :: Integer
    , ppppCommissionFund_PerYear_InBPx1e3 :: ProtocolT.MinMaxDef Integer
    , ppppCommissionSwapOffer_InBPx1e3 :: ProtocolT.MinMaxDef Integer
    , ppppCommissionBuyOrder_InBPx1e3 :: ProtocolT.MinMaxDef Integer
    , ppppShare_InBPx1e2_Protocol :: Integer
    , ppppShare_InBPx1e2_Managers :: Integer
    , ppppShare_InBPx1e2_Delegators :: Integer
    , ppppDelegatorsAdmins :: [T.WalletPaymentPKH]
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABProtocolUpdateParams = PABProtocolUpdateParams
    { ppupProtocolPABParams :: ProtocolPABParams
    , ppupOraclePaymentPubKey :: LedgerAddress.PaymentPubKey
    , ppupAdmins :: [T.WalletPaymentPKH]
    , ppupTokenAdminPolicy_CS :: T.CS
    , ppupFundCategories :: [ProtocolT.FundCategory]
    , ppupFundLifeTime :: ProtocolT.MinMaxDef LedgerApiV2.POSIXTime
    , ppupRequiredMAYZForSwapOffer :: Integer
    , ppupRequiredMAYZForBuyOrder :: Integer
    , ppupCommissionFund_PerYear_InBPx1e3 :: ProtocolT.MinMaxDef Integer
    , ppupCommissionSwapOffer_InBPx1e3 :: ProtocolT.MinMaxDef Integer
    , ppupCommissionBuyOrder_InBPx1e3 :: ProtocolT.MinMaxDef Integer
    , ppupShare_InBPx1e2_Protocol :: Integer
    , ppupShare_InBPx1e2_Managers :: Integer
    , ppupShare_InBPx1e2_Delegators :: Integer
    , ppupDelegatorsAdmins :: [T.WalletPaymentPKH]
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

newtype PABProtocolScriptAddParams = PABProtocolScriptAddParams {ppsapProtocolPABParams :: ProtocolPABParams}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

newtype PABProtocolScriptDeleteParams = PABProtocolScriptDeleteParams {ppsdpProtocolPABParams :: ProtocolPABParams}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABProtocolEmergencyParams = PABProtocolEmergencyParams
    { ppepProtocolPABParams :: ProtocolPABParams
    , ppepAdmins :: [T.WalletPaymentPKH]
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data PABFundPrepareParams = PABFundPrepareParams
    { pfppProtocolPABParams :: ProtocolPABParams
    , pfppFundPABParams :: FundPABParams
    , pfppFundFT_TN :: T.TN
    , pfppAdmins :: [T.WalletPaymentPKH]
    , pfppTokenAdminPolicy_CS :: T.CS
    , pfppFundCategoryNumber :: Integer
    , pfppBeginAt :: LedgerApiV2.POSIXTime
    , pfppDeadline :: LedgerApiV2.POSIXTime
    , pfppClosedAt :: Maybe LedgerApiV2.POSIXTime
    , pfppCommission_PerYear_InBPx1e3 :: Integer
    , pfppInvestUnit :: T.InvestUnit
    , pfppInvestUnitPriceADA :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABFundUpdateParams = PABFundUpdateParams
    { pfupProtocolPABParams :: ProtocolPABParams
    , pfupFundPABParams :: FundPABParams
    , pfupAdmins :: [T.WalletPaymentPKH]
    , pfupTokenAdminPolicy_CS :: T.CS
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABFundScriptAddParams = PABFundScriptAddParams
    { pfsapProtocolPABParams :: ProtocolPABParams
    , pfsapFundPABParams :: FundPABParams
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABFundScriptDeleteParams = PABFundScriptDeleteParams
    { pfsdpProtocolPABParams :: ProtocolPABParams
    , pfsdpFundPABParams :: FundPABParams
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2

data PABFundEmergencyParams = PABFundEmergencyParams
    { pfepProtocolPABParams :: ProtocolPABParams
    , pfepFundPABParams :: FundPABParams
    , pfepAdmins :: [T.WalletPaymentPKH]
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

data PABFundHoldingAddParams = PABFundHoldingAddParams
    { pfhapProtocolPABParams :: ProtocolPABParams
    , pfhapFundPABParams :: FundPABParams
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundHoldingDeleteParams = PABFundHoldingDeleteParams
    { pfhdpProtocolPABParams :: ProtocolPABParams
    , pfhdpFundPABParams :: FundPABParams
    , pfhdpFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundDepositParams = PABFundDepositParams
    { pfdpProtocolPABParams :: ProtocolPABParams
    , pfdpFundPABParams :: FundPABParams
    , pfdpAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundWithdrawParams = PABFundWithdrawParams
    { pfwpProtocolPABParams :: ProtocolPABParams
    , pfwpFundPABParams :: FundPABParams
    , pfwpAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundReIndexingParams = PABFundReIndexingParams
    { pfripProtocolPABParams :: ProtocolPABParams
    , pfripFundPABParams :: FundPABParams
    , pfripTokensToAdd :: T.InvestUnit
    , pfripTokensToRemove :: T.InvestUnit
    , pfripFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundUpdateOracleParams = PABFundUpdateOracleParams
    { pfuopProtocolPABParams :: ProtocolPABParams
    , pfuopFundPABParams :: FundPABParams
    , pfuopPriceADA :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundUpdateOracleReIdxParams = PABFundUpdateOracleReIdxParams
    { pfuoripProtocolPABParams :: ProtocolPABParams
    , pfuoripFundPABParams :: FundPABParams
    , pfuoripTokensWithPricesADA :: T.InvestUnit
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundCollect_Protocol_CommissionParams = PABFundCollect_Protocol_CommissionParams
    { pfwpcpProtocolPABParams :: ProtocolPABParams
    , pfwpcpFundPABParams :: FundPABParams
    , pfwpcpFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
    , pfwpcpAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundCollect_Delegators_CommissionParams = PABFundCollect_Delegators_CommissionParams
    { pfwmcpProtocolPABParams :: ProtocolPABParams
    , pfwmcpFundPABParams :: FundPABParams
    , pfwmcpFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
    , pfwmcpAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

data PABFundCollect_Managers_CommissionParams = PABFundCollect_Managers_CommissionParams
    { pfwfcpProtocolPABParams :: ProtocolPABParams
    , pfwfcpFundPABParams :: FundPABParams
    , pfwfcpFundHoldingTxOutRef :: LedgerApiV2.TxOutRef
    , pfwfcpAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePOSIXTime :: LedgerApiV2.POSIXTime
examplePOSIXTime = 1658172331000

exampleTxOutRef :: LedgerApiV2.TxOutRef
exampleTxOutRef =
    LedgerApiV2.TxOutRef
        { LedgerApiV2.txOutRefId = "ed485b083eb5816c10c35a9d091d8af4cfdceef40c96578cae2b2266a8d976c9"
        , LedgerApiV2.txOutRefIdx = 1
        }

exampleTxOutRef1 :: LedgerApiV2.TxOutRef
exampleTxOutRef1 =
    LedgerApiV2.TxOutRef
        { LedgerApiV2.txOutRefId = "ed485b083eb5816c10c35a9d091d8af4cfdceef40c96578cae2b2266a8d976c9"
        , LedgerApiV2.txOutRefIdx = 2
        }

exampleTxOutRef000 :: LedgerApiV2.TxOutRef
exampleTxOutRef000 =
    LedgerApiV2.TxOutRef
        { LedgerApiV2.txOutRefId = "0000000000000000000000000000000000000000000000000000000000000000"
        , LedgerApiV2.txOutRefIdx = 0
        }

exampleAddress :: LedgerAddress.Address
exampleAddress = LedgerAddress.Address {LedgerApiV2.addressCredential = LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e", LedgerApiV2.addressStakingCredential = Nothing}

exampleAddress1 :: LedgerAddress.Address
exampleAddress1 = LedgerAddress.Address {LedgerApiV2.addressCredential = LedgerApiV2.PubKeyCredential $ LedgerApiV2.PubKeyHash "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c1ff", LedgerApiV2.addressStakingCredential = Nothing}

exampleValidatorHash :: LedgerApiV2.ValidatorHash
exampleValidatorHash = LedgerApiV2.ValidatorHash "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c916ed"

exampleMkValidator :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
exampleMkValidator _ _ _ = ()

exampleValidator :: LedgerApiV2.Validator
exampleValidator = LedgerApiV2.mkValidatorScript $$(PlutusTx.compile [||exampleMkValidator||])

exampleMkPolicy :: PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ()
exampleMkPolicy _ _ = ()

exampleMintingPolicy :: LedgerApiV2.MintingPolicy
exampleMintingPolicy = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [||exampleMkPolicy||])

exampleCS :: LedgerApiV2.CurrencySymbol
exampleCS = "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c916ed"

exampleTN :: LedgerApiV2.TokenName
exampleTN = "TN"

exampleNFT :: T.NFT
exampleNFT = LedgerValue.AssetClass (exampleCS, exampleTN)

exampleWalletPaymentPKH :: T.WalletPaymentPKH
exampleWalletPaymentPKH = "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"

exampleStakeCredentialPubKeyHash :: T.StakeCredentialPubKeyHash
exampleStakeCredentialPubKeyHash = "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e"

examplePaymentPubKey :: Ledger.PaymentPubKey
examplePaymentPubKey = LedgerAddress.PaymentPubKey $ Crypto.PubKey $ LedgerApiV2.LedgerBytes $ Ledger.getPubKeyHash $ Ledger.unPaymentPubKeyHash $ Ledger.PaymentPubKeyHash exampleWalletPaymentPKH

exampleBBS :: LedgerApiV2.BuiltinByteString
exampleBBS = "aaccff"

exampleBool :: Bool
exampleBool = True

exampleInteger :: Integer
exampleInteger = 3_000_000

exampleString :: P.String
exampleString = "aaccff"

exampleValue :: LedgerApiV2.Value
exampleValue = LedgerApiV2.singleton exampleCS exampleTN exampleInteger

exampleFlattenValue :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
exampleFlattenValue = OnChainHelpers.flattenValue exampleValue

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- exampleFundFT_TN :: LedgerApiV2.TokenName
-- exampleFundFT_TN = "FT"

-- exampleTokenMAYZ_TN :: LedgerApiV2.TokenName
-- exampleTokenMAYZ_TN = "MAYZ"

-- exampleTokenMAYZ_CS :: LedgerApiV2.CurrencySymbol
-- exampleTokenMAYZ_CS = exampleCS

-- exampleTokenMAYZ_AC :: LedgerValue.AssetClass
-- exampleTokenMAYZ_AC = LedgerValue.AssetClass (exampleTokenMAYZ_CS, exampleTokenMAYZ_TN)

-- exampleTokenAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
-- exampleTokenAdminPolicy_CS = exampleCS

-- exampleFundCategoryNumber :: Integer
-- exampleFundCategoryNumber = 0

-- exampleFundCategoryRequiredMAYZ :: Integer
-- exampleFundCategoryRequiredMAYZ = 7

-- exampleFundCategoryMaxUI :: Integer
-- exampleFundCategoryMaxUI = 100

-- exampleFundCategory :: ProtocolT.FundCategory
-- exampleFundCategory
--     = ProtocolT.FundCategory
--           { ProtocolT.fcCategoryNumber  = exampleFundCategoryNumber
--           , ProtocolT.fcRequiredMAYZ   = exampleFundCategoryRequiredMAYZ
--           , ProtocolT.fcMaxUI         = exampleFundCategoryMaxUI
--           }

-- exampleHoldingsIndex :: Integer
-- exampleHoldingsIndex = 1

-- exampleHoldingsCount :: Integer
-- exampleHoldingsCount  = 1

exampleInvestUnit :: T.InvestUnit
exampleInvestUnit = T.InvestUnit {T.iuValues = [(exampleCS, exampleTN, 100)]}

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

exampleProtocolPolicyParams :: ProtocolT.PolicyParams
exampleProtocolPolicyParams =
    ProtocolT.PolicyParams
        { ProtocolT.ppProtocolPolicyID_TxOutRef = exampleTxOutRef
        }

exampleProtocolValidatorParams :: ProtocolT.ValidatorParams
exampleProtocolValidatorParams =
    ProtocolT.ValidatorParams
        { ProtocolT.vpProtocolPolicyID_CS = exampleCS
        , ProtocolT.vpTokenEmergencyAdminPolicy_CS = exampleCS
        }

--------------------------------------------------------------------------------2

exampleFundPolicyParams :: FundT.PolicyParams
exampleFundPolicyParams =
    FundT.PolicyParams exampleCS exampleTxOutRef exampleValidatorHash

exampleFundValidatorParams :: FundT.ValidatorParams
exampleFundValidatorParams =
    FundT.ValidatorParams
        { FundT.vpProtocolPolicyID_CS = exampleCS
        , FundT.vpTokenEmergencyAdminPolicy_CS = exampleCS
        }

exampleInvestUnitValidatorParams :: InvestUnitT.ValidatorParams
exampleInvestUnitValidatorParams =
    InvestUnitT.ValidatorParams
        { InvestUnitT.vpProtocolPolicyID_CS = exampleCS
        , InvestUnitT.vpTokenEmergencyAdminPolicy_CS = exampleCS
        }

--------------------------------------------------------------------------------2

exampleScriptPolicyParams :: ScriptT.PolicyParams
exampleScriptPolicyParams =
    ScriptT.PolicyParams
        { ScriptT.ppProtocolPolicyID_CS = exampleCS
        }

exampleScriptValidatorParams :: ScriptT.ValidatorParams
exampleScriptValidatorParams =
    ScriptT.ValidatorParams exampleCS exampleCS

--------------------------------------------------------------------------------2

exampleFundHoldingPolicyParams :: FundHoldingT.PolicyParams
exampleFundHoldingPolicyParams =
    FundHoldingT.PolicyParams exampleCS

exampleFundHoldingValidatorParams :: FundHoldingT.ValidatorParams
exampleFundHoldingValidatorParams =
    FundHoldingT.ValidatorParams exampleCS exampleCS exampleCS

--------------------------------------------------------------------------------2

exampleSwapOfferPolicyParams :: SwapOfferT.PolicyParams
exampleSwapOfferPolicyParams =
    SwapOfferT.PolicyParams exampleCS exampleValidatorHash

exampleSwapOfferValidatorParams :: SwapOfferT.ValidatorParams
exampleSwapOfferValidatorParams =
    SwapOfferT.ValidatorParams exampleCS exampleCS

--------------------------------------------------------------------------------2

exampleProtocolPABParams :: ProtocolPABParams
exampleProtocolPABParams =
    ProtocolPABParams
        { pppProtocolVersion = ProtocolT.protocolVersion
        , pppProtocolPolicyID_Params = exampleProtocolPolicyParams
        , pppProtocolPolicyID = exampleMintingPolicy
        , pppProtocolPolicyID_CS = exampleCS
        , pppProtocolValidator_Params = exampleProtocolValidatorParams
        , pppProtocolValidator = exampleValidator
        , pppProtocolValidator_Hash = exampleValidatorHash
        , pppProtocolValidator_Address = exampleAddress
        , pppScriptPolicyID_Params = exampleScriptPolicyParams
        , pppScriptPolicyID = exampleMintingPolicy
        , pppScriptPolicyID_CS = exampleCS
        , pppScriptValidator_Params = exampleScriptValidatorParams
        , pppScriptValidator = exampleValidator
        , pppScriptValidator_Hash = exampleValidatorHash
        , pppScriptValidator_Address = exampleAddress
        , pppInvestUnitValidator_Params = exampleInvestUnitValidatorParams
        , pppInvestUnitValidator = exampleValidator
        , pppInvestUnitValidator_Hash = exampleValidatorHash
        , pppInvestUnitValidator_Address = exampleAddress
        , pppFundFactoryPABParams = []
        , pppTokenEmergencyAdmin_CS = exampleCS
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

exampleFundFactoryPABParams :: FundFactoryPABParams
exampleFundFactoryPABParams =
    FundFactoryPABParams
        { ffppFundVersion = FundT.fundVersion
        , ffppFundValidator_Params = exampleFundValidatorParams
        , ffppFundValidator = exampleValidator
        , ffppFundValidator_Hash = exampleValidatorHash
        , ffppFundValidator_Address = exampleAddress
        , ffppFundPABParams = [exampleFundPABParams]
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

exampleFundPABParams :: FundPABParams
exampleFundPABParams =
    FundPABParams
        { fppFundVersion = FundT.fundVersion
        , fppFundPolicy_Params = exampleFundPolicyParams
        , fppFundPolicy = exampleMintingPolicy
        , fppFundPolicy_CS = exampleCS
        , fppFundValidator_Params = exampleFundValidatorParams
        , fppFundValidator = exampleValidator
        , fppFundValidator_Hash = exampleValidatorHash
        , fppFundValidator_Address = exampleAddress
        , fppFundHoldingPolicyID_Params = exampleFundHoldingPolicyParams
        , fppFundHoldingPolicyID = exampleMintingPolicy
        , fppFundHoldingPolicyID_CS = exampleCS
        , fppFundHoldingValidator_Params = exampleFundHoldingValidatorParams
        , fppFundHoldingValidator = exampleValidator
        , fppFundHoldingValidator_Hash = exampleValidatorHash
        , fppFundHoldingValidator_Address = exampleAddress
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePABSplitUtxOParams :: PABSplitUtxOParams
examplePABSplitUtxOParams =
    PABSplitUtxOParams
        { psupSplitAmount = exampleInteger
        }

examplePABMintFTParams :: PABMintFTParams
examplePABMintFTParams =
    PABMintFTParams
        { pmfpPolicyNum = exampleInteger
        , pmfpTokenNameBase = exampleBBS
        , pmfpDiifTokenNameCount = exampleInteger
        , pmfpAmount = exampleInteger
        }

examplePABMintFundTokensParams :: PABMintFundTokensParams
examplePABMintFundTokensParams =
    PABMintFundTokensParams
        { pmftpProtocolPABParams = exampleProtocolPABParams
        , pmftpFundPABParams = exampleFundPABParams
        , pmftpAmount = exampleInteger
        }

examplePABMintNFTParams :: PABMintNFTParams
examplePABMintNFTParams =
    PABMintNFTParams
        { pmnpTokenNameBase = exampleBBS
        , pmnpDiifTokenNameCount = exampleInteger
        , pmnpAmount = exampleInteger
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePABProtocolPrepareParams :: PABProtocolPrepareParams
examplePABProtocolPrepareParams =
    PABProtocolPrepareParams
        { ppppProtocolPABParams = exampleProtocolPABParams
        , ppppOraclePaymentPubKey = examplePaymentPubKey
        , ppppAdmins = [exampleWalletPaymentPKH]
        , ppppTokenAdminPolicy_CS = exampleCS
        , ppppFundCategories = [ProtocolT.FundCategory exampleInteger exampleInteger exampleInteger]
        , ppppFundLifeTime = ProtocolT.mkMinMaxDef examplePOSIXTime examplePOSIXTime examplePOSIXTime
        , ppppRequiredMAYZForSwapOffer = exampleInteger
        , ppppRequiredMAYZForBuyOrder = exampleInteger
        , ppppCommissionFund_PerYear_InBPx1e3 = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger
        , ppppCommissionSwapOffer_InBPx1e3 = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger
        , ppppCommissionBuyOrder_InBPx1e3 = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger
        , ppppShare_InBPx1e2_Protocol = exampleInteger
        , ppppShare_InBPx1e2_Managers = exampleInteger
        , ppppShare_InBPx1e2_Delegators = exampleInteger
        , ppppDelegatorsAdmins = [exampleWalletPaymentPKH]
        }

examplePABProtocolUpdateParams :: PABProtocolUpdateParams
examplePABProtocolUpdateParams =
    PABProtocolUpdateParams
        { ppupProtocolPABParams = exampleProtocolPABParams
        , ppupOraclePaymentPubKey = examplePaymentPubKey
        , ppupAdmins = [exampleWalletPaymentPKH]
        , ppupTokenAdminPolicy_CS = exampleCS
        , ppupFundCategories = [ProtocolT.FundCategory exampleInteger exampleInteger exampleInteger]
        , ppupFundLifeTime = ProtocolT.mkMinMaxDef examplePOSIXTime examplePOSIXTime examplePOSIXTime
        , ppupRequiredMAYZForSwapOffer = exampleInteger
        , ppupRequiredMAYZForBuyOrder = exampleInteger
        , ppupCommissionFund_PerYear_InBPx1e3 = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger
        , ppupCommissionSwapOffer_InBPx1e3 = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger
        , ppupCommissionBuyOrder_InBPx1e3 = ProtocolT.mkMinMaxDef exampleInteger exampleInteger exampleInteger
        , ppupShare_InBPx1e2_Protocol = exampleInteger
        , ppupShare_InBPx1e2_Managers = exampleInteger
        , ppupShare_InBPx1e2_Delegators = exampleInteger
        , ppupDelegatorsAdmins = [exampleWalletPaymentPKH]
        }

examplePABProtocolScriptAddParams :: PABProtocolScriptAddParams
examplePABProtocolScriptAddParams =
    PABProtocolScriptAddParams
        { ppsapProtocolPABParams = exampleProtocolPABParams
        }

examplePABProtocolScriptDeleteParams :: PABProtocolScriptDeleteParams
examplePABProtocolScriptDeleteParams =
    PABProtocolScriptDeleteParams
        { ppsdpProtocolPABParams = exampleProtocolPABParams
        }

examplePABProtocolEmergencyParams :: PABProtocolEmergencyParams
examplePABProtocolEmergencyParams =
    PABProtocolEmergencyParams
        { ppepProtocolPABParams = exampleProtocolPABParams
        , ppepAdmins = [exampleWalletPaymentPKH]
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePABFundPrepareParams :: PABFundPrepareParams
examplePABFundPrepareParams =
    PABFundPrepareParams
        { pfppProtocolPABParams = exampleProtocolPABParams
        , pfppFundPABParams = exampleFundPABParams
        , pfppFundFT_TN = exampleTN
        , pfppAdmins = [exampleWalletPaymentPKH]
        , pfppTokenAdminPolicy_CS = exampleCS
        , pfppFundCategoryNumber = exampleInteger
        , pfppBeginAt = examplePOSIXTime
        , pfppDeadline = examplePOSIXTime
        , pfppClosedAt = Just examplePOSIXTime
        , pfppCommission_PerYear_InBPx1e3 = exampleInteger
        , pfppInvestUnit = exampleInvestUnit
        , pfppInvestUnitPriceADA = exampleInteger
        }

examplePABFundUpdateParams :: PABFundUpdateParams
examplePABFundUpdateParams =
    PABFundUpdateParams
        { pfupProtocolPABParams = exampleProtocolPABParams
        , pfupFundPABParams = exampleFundPABParams
        , pfupAdmins = [exampleWalletPaymentPKH]
        , pfupTokenAdminPolicy_CS = exampleCS
        }

examplePABFundScriptAddParams :: PABFundScriptAddParams
examplePABFundScriptAddParams =
    PABFundScriptAddParams
        { pfsapProtocolPABParams = exampleProtocolPABParams
        , pfsapFundPABParams = exampleFundPABParams
        }

examplePABFundScriptDeleteParams :: PABFundScriptDeleteParams
examplePABFundScriptDeleteParams =
    PABFundScriptDeleteParams
        { pfsdpProtocolPABParams = exampleProtocolPABParams
        , pfsdpFundPABParams = exampleFundPABParams
        }

examplePABFundEmergencyParams :: PABFundEmergencyParams
examplePABFundEmergencyParams =
    PABFundEmergencyParams
        { pfepProtocolPABParams = exampleProtocolPABParams
        , pfepFundPABParams = exampleFundPABParams
        , pfepAdmins = [exampleWalletPaymentPKH]
        }

--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2

examplePABFundHoldingAddParams :: PABFundHoldingAddParams
examplePABFundHoldingAddParams =
    PABFundHoldingAddParams
        { pfhapProtocolPABParams = exampleProtocolPABParams
        , pfhapFundPABParams = exampleFundPABParams
        }

examplePABFundHoldingDeleteParams :: PABFundHoldingDeleteParams
examplePABFundHoldingDeleteParams =
    PABFundHoldingDeleteParams
        { pfhdpProtocolPABParams = exampleProtocolPABParams
        , pfhdpFundPABParams = exampleFundPABParams
        , pfhdpFundHoldingTxOutRef = exampleTxOutRef
        }

examplePABFundDepositParams :: PABFundDepositParams
examplePABFundDepositParams =
    PABFundDepositParams
        { pfdpProtocolPABParams = exampleProtocolPABParams
        , pfdpFundPABParams = exampleFundPABParams
        , pfdpAmount = exampleInteger
        }

examplePABFundWithdrawParams :: PABFundWithdrawParams
examplePABFundWithdrawParams =
    PABFundWithdrawParams
        { pfwpProtocolPABParams = exampleProtocolPABParams
        , pfwpFundPABParams = exampleFundPABParams
        , pfwpAmount = exampleInteger
        }

examplePABFundReIndexingParams :: PABFundReIndexingParams
examplePABFundReIndexingParams =
    PABFundReIndexingParams
        { pfripProtocolPABParams = exampleProtocolPABParams
        , pfripFundPABParams = exampleFundPABParams
        , pfripTokensToAdd = exampleInvestUnit
        , pfripTokensToRemove = exampleInvestUnit
        , pfripFundHoldingTxOutRef = exampleTxOutRef
        }

examplePABFundUpdateOracleParams :: PABFundUpdateOracleParams
examplePABFundUpdateOracleParams =
    PABFundUpdateOracleParams
        { pfuopProtocolPABParams = exampleProtocolPABParams
        , pfuopFundPABParams = exampleFundPABParams
        , pfuopPriceADA = exampleInteger
        }

examplePABFundUpdateOracleReIdxParams :: PABFundUpdateOracleReIdxParams
examplePABFundUpdateOracleReIdxParams =
    PABFundUpdateOracleReIdxParams
        { pfuoripProtocolPABParams = exampleProtocolPABParams
        , pfuoripFundPABParams = exampleFundPABParams
        , pfuoripTokensWithPricesADA = exampleInvestUnit
        }

examplePABFundCollect_Protocol_CommissionParams :: PABFundCollect_Protocol_CommissionParams
examplePABFundCollect_Protocol_CommissionParams =
    PABFundCollect_Protocol_CommissionParams
        { pfwpcpProtocolPABParams = exampleProtocolPABParams
        , pfwpcpFundPABParams = exampleFundPABParams
        , pfwpcpFundHoldingTxOutRef = exampleTxOutRef
        , pfwpcpAmount = exampleInteger
        }

examplePABFundCollect_Managers_CommissionParams :: PABFundCollect_Managers_CommissionParams
examplePABFundCollect_Managers_CommissionParams =
    PABFundCollect_Managers_CommissionParams
        { pfwfcpProtocolPABParams = exampleProtocolPABParams
        , pfwfcpFundPABParams = exampleFundPABParams
        , pfwfcpFundHoldingTxOutRef = exampleTxOutRef
        , pfwfcpAmount = exampleInteger
        }


examplePABFundCollect_Delegators_CommissionParams :: PABFundCollect_Delegators_CommissionParams
examplePABFundCollect_Delegators_CommissionParams =
    PABFundCollect_Delegators_CommissionParams
        { pfwmcpProtocolPABParams = exampleProtocolPABParams
        , pfwmcpFundPABParams = exampleFundPABParams
        , pfwmcpFundHoldingTxOutRef = exampleTxOutRef
        , pfwmcpAmount = exampleInteger
        }


--------------------------------------------------------------------------------2
--------------------------------------------------------------------------------2
