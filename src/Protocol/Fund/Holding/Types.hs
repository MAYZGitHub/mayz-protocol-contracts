{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Fund.Holding.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson as DataAeson (FromJSON, ToJSON)
import qualified Data.OpenApi.Schema as DataOpenApiSchema (ToSchema)
import qualified GHC.Generics as GHCGenerics (Generic)
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Schema
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Constants as T
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- Any change in the logic, datum or redeemer must change the version of the fundVersion on Protocol.Fund.Types

ownVersion :: Integer
ownVersion = T.mkVersionWithDependency [ProtocolT.protocolVersion] FundT.fundVersion

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

newtype PolicyParams = PolicyParams {ppFundPolicy_CS :: LedgerApiV2.CurrencySymbol}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        ppFundPolicy_CS pp1 == ppFundPolicy_CS pp2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed
    ''PolicyParams
    [ ('PolicyParams, 0)
    ]

data ValidatorParams = ValidatorParams
    { vpProtocolPolicyID_CS :: LedgerApiV2.CurrencySymbol
    , vpFundPolicy_CS :: LedgerApiV2.CurrencySymbol
    , vpTokenEmergencyAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

-- instance Schema.ToSchema ValidatorParams where
--     toSchema = Schema.FormSchemaUnit

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    pp1 == pp2 =
        vpProtocolPolicyID_CS pp1 == vpProtocolPolicyID_CS pp2
            && vpFundPolicy_CS pp1 == vpFundPolicy_CS pp2
            && vpTokenEmergencyAdminPolicy_CS pp1 == vpTokenEmergencyAdminPolicy_CS pp2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed
    ''ValidatorParams
    [ ('ValidatorParams, 0)
    ]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2

data FundHoldingDatumType = FundHoldingDatumType
    { -- Version Control
      hdVersion :: Integer -- Contract version
    , -- Identification
      hdFundHolding_Index :: Integer -- Unique holding identifier
    , -- Fund Token Accounting
      hdSubTotal_FT_Minted_Accumulated :: Integer -- Total FTs ever minted
    , hdSubTotal_FT_Minted :: Integer -- Current FTs in circulation
    , -- Commission Tracking
      hdSubTotal_FT_Commissions :: Integer -- Available commissions
    , hdSubTotal_FT_Commissions_Total :: Integer -- Total acum commissions, for calcs
    , hdSubTotal_FT_Commissions_Release_PerMonth_1e6 :: Integer -- Monthly release
    , -- Commission Collections
      hdSubTotal_FT_Commissions_Collected_Protocol :: Integer -- Protocol total
    , hdSubTotal_FT_Commissions_Collected_Managers :: Integer -- Managers total
    , hdSubTotal_FT_Commissions_Collected_Delegators :: Integer -- Delegators total
    , --- Minimum ADA
      hdMinADA :: Integer -- Minimum ADA used in UTXO
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq FundHoldingDatumType where
    {-# INLINEABLE (==) #-}
    ps1 == ps2 =
        hdVersion ps1 == hdVersion ps2
            && hdFundHolding_Index ps1 == hdFundHolding_Index ps2
            && hdSubTotal_FT_Minted_Accumulated ps1 == hdSubTotal_FT_Minted_Accumulated ps2
            && hdSubTotal_FT_Minted ps1 == hdSubTotal_FT_Minted ps2
            && hdSubTotal_FT_Commissions ps1 == hdSubTotal_FT_Commissions ps2
            && hdSubTotal_FT_Commissions_Total ps1 == hdSubTotal_FT_Commissions_Total ps2
            && hdSubTotal_FT_Commissions_Release_PerMonth_1e6 ps1 == hdSubTotal_FT_Commissions_Release_PerMonth_1e6 ps2
            && hdSubTotal_FT_Commissions_Collected_Protocol ps1 == hdSubTotal_FT_Commissions_Collected_Protocol ps2
            && hdSubTotal_FT_Commissions_Collected_Managers ps1 == hdSubTotal_FT_Commissions_Collected_Managers ps2
            && hdSubTotal_FT_Commissions_Collected_Delegators ps1 == hdSubTotal_FT_Commissions_Collected_Delegators ps2
            && hdMinADA ps1 == hdMinADA ps2

instance Ord FundHoldingDatumType where
    {-# INLINEABLE compare #-}
    compare :: FundHoldingDatumType -> FundHoldingDatumType -> Ordering
    compare a b = compare (hdFundHolding_Index a) (hdFundHolding_Index b)

PlutusTx.makeIsDataIndexed
    ''FundHoldingDatumType
    [ ('FundHoldingDatumType, 0)
    ]

--------------------------------------------------------------------------------2

newtype ValidatorDatum
    = FundHoldingDatum FundHoldingDatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    FundHoldingDatum mps1 == FundHoldingDatum mps2 = mps1 == mps2

PlutusTx.makeIsDataIndexed
    ''ValidatorDatum
    [ ('FundHoldingDatum, 0)
    ]

{-# INLINEABLE getFundHolding_DatumType #-}
getFundHolding_DatumType :: ValidatorDatum -> FundHoldingDatumType
getFundHolding_DatumType (FundHoldingDatum sdType) = sdType

{-# INLINEABLE getFundHolding_DatumType_From_UTxO #-}
getFundHolding_DatumType_From_UTxO :: LedgerApiV2.TxOut -> FundHoldingDatumType
getFundHolding_DatumType_From_UTxO utxo = case OnChainHelpers.getInlineDatum_From_TxOut @ValidatorDatum utxo of
    Nothing -> P.error "No FundHolding Datum found"
    Just datum' -> getFundHolding_DatumType datum'

instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkFundHolding_DatumType #-}
mkFundHolding_DatumType :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> FundHoldingDatumType
mkFundHolding_DatumType = FundHoldingDatumType ownVersion

{-# INLINEABLE mkFundHolding_Datum #-}
mkFundHolding_Datum :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> ValidatorDatum
mkFundHolding_Datum
    holdingIndex
    subTotal_FT_AcumDeposits
    subTotal_FT_Minted
    subTotal_FT_Commissions
    subTotal_FT_Commissions_Accumulated
    subTotal_FT_Commissions_Release_PerMonth_1e6
    subTotal_FT_Commissions_Collected_Protocol
    subTotal_FT_Commissions_Collected_Managers
    subTotal_FT_Commissions_Collected_Delegators
    minADA =
        FundHoldingDatum $
            mkFundHolding_DatumType
                holdingIndex
                subTotal_FT_AcumDeposits
                subTotal_FT_Minted
                subTotal_FT_Commissions
                subTotal_FT_Commissions_Accumulated
                subTotal_FT_Commissions_Release_PerMonth_1e6
                subTotal_FT_Commissions_Collected_Protocol
                subTotal_FT_Commissions_Collected_Managers
                subTotal_FT_Commissions_Collected_Delegators
                minADA

mkDatum :: FundHoldingDatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . FundHoldingDatum

--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''PolicyRedeemerMintIDType [('PolicyRedeemerMintIDType, 0)]

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [ ('PolicyRedeemerBurnIDType, 0)
    ]

data PolicyRedeemer
    = PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemer where
    {-# INLINEABLE (==) #-}
    PolicyRedeemerMintID rmtx1 == PolicyRedeemerMintID rmtx2 = rmtx1 == rmtx2
    PolicyRedeemerBurnID rmtx1 == PolicyRedeemerBurnID rmtx2 = rmtx1 == rmtx2
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 0)
    , ('PolicyRedeemerBurnID, 1)
    ]

--------------------------------------------------------------------------------2

getPolicyRedeemerName :: Maybe PolicyRedeemer -> Maybe P.String
getPolicyRedeemerName (Just (PolicyRedeemerMintID PolicyRedeemerMintIDType)) = Just "MintID"
getPolicyRedeemerName (Just (PolicyRedeemerBurnID PolicyRedeemerBurnIDType)) = Just "BurnID"
getPolicyRedeemerName _ = Nothing

--------------------------------------------------------------------------------2
-- ValidatorRedeemer
--------------------------------------------------------------------------------2

data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerUpdateMinADAType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemerDepositType = ValidatorRedeemerDepositType
    { rdDate :: LedgerApiV2.POSIXTime
    , rdAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDepositType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rdDate r1 == rdDate r2
            && rdAmount r1 == rdAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDepositType
    [ ('ValidatorRedeemerDepositType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerWithdrawType = ValidatorRedeemerWithdrawType
    { rwDate :: LedgerApiV2.POSIXTime
    , rwAmount :: Integer
    , rwAmountPlusComissions :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerWithdrawType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rwDate r1 == rwDate r2
            && rwAmount r1 == rwAmount r2
            && rwAmountPlusComissions r1 == rwAmountPlusComissions r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerWithdrawType
    [ ('ValidatorRedeemerWithdrawType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerCollect_Protocol_CommissionType = ValidatorRedeemerCollect_Protocol_CommissionType
    { rcpcDate :: LedgerApiV2.POSIXTime
    , rcpcAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerCollect_Protocol_CommissionType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rcpcDate r1 == rcpcDate r2 && rcpcAmount r1 == rcpcAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_Protocol_CommissionType
    [ ('ValidatorRedeemerCollect_Protocol_CommissionType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerCollect_Managers_CommissionType = ValidatorRedeemerCollect_Managers_CommissionType
    { rcmcDate :: LedgerApiV2.POSIXTime
    , rcmcAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerCollect_Managers_CommissionType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rcmcDate r1 == rcmcDate r2 && rcmcAmount r1 == rcmcAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_Managers_CommissionType
    [ ('ValidatorRedeemerCollect_Managers_CommissionType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerCollect_Delegators_CommissionType = ValidatorRedeemerCollect_Delegators_CommissionType
    { rcdcDate :: LedgerApiV2.POSIXTime
    , rcdcAmount :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerCollect_Delegators_CommissionType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rcdcDate r1 == rcdcDate r2 && rcdcAmount r1 == rcdcAmount r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_Delegators_CommissionType
    [ ('ValidatorRedeemerCollect_Delegators_CommissionType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerReIndexingType = ValidatorRedeemerReIndexingType
    { rriTokensToAdd :: T.InvestUnit
    , rriTokensToRemove :: T.InvestUnit
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerReIndexingType where
    {-# INLINEABLE (==) #-}
    r1 == r2 =
        rriTokensToAdd r1 == rriTokensToAdd r2 && rriTokensToRemove r1 == rriTokensToRemove r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerReIndexingType
    [ ('ValidatorRedeemerReIndexingType, 0)
    ]

--------------------------------------------------------------------------------2

newtype ValidatorRedeemerBalanceAssetsType = ValidatorRedeemerBalanceAssetsType {rbMoveCommissionsFT :: [Integer]}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerBalanceAssetsType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = rbMoveCommissionsFT r1 == rbMoveCommissionsFT r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerBalanceAssetsType
    [ ('ValidatorRedeemerBalanceAssetsType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDeleteType
    [ ('ValidatorRedeemerDeleteType, 0)
    ]

--------------------------------------------------------------------------------2

data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerEmergencyType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]

--------------------------------------------------------------------------------2

data ValidatorRedeemer
    = ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerCollect_Protocol_Commission ValidatorRedeemerCollect_Protocol_CommissionType
    | ValidatorRedeemerCollect_Managers_Commission ValidatorRedeemerCollect_Managers_CommissionType
    | ValidatorRedeemerCollect_Delegators_Commission ValidatorRedeemerCollect_Delegators_CommissionType
    | ValidatorRedeemerReIndexing ValidatorRedeemerReIndexingType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    | ValidatorRedeemerBalanceAssets ValidatorRedeemerBalanceAssetsType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateMinADA rmf1 == ValidatorRedeemerUpdateMinADA rmf2 = rmf1 == rmf2
    ValidatorRedeemerDeposit rmf1 == ValidatorRedeemerDeposit rmf2 = rmf1 == rmf2
    ValidatorRedeemerWithdraw rmcp1 == ValidatorRedeemerWithdraw rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerCollect_Protocol_Commission rmcp1 == ValidatorRedeemerCollect_Protocol_Commission rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerCollect_Managers_Commission rmcp1 == ValidatorRedeemerCollect_Managers_Commission rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerCollect_Delegators_Commission rmcp1 == ValidatorRedeemerCollect_Delegators_Commission rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerReIndexing rmcp1 == ValidatorRedeemerReIndexing rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerBalanceAssets rmcp1 == ValidatorRedeemerBalanceAssets rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerEmergency rmcp1 == ValidatorRedeemerEmergency rmcp2 = rmcp1 == rmcp2
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateMinADA, 0)
    , ('ValidatorRedeemerDeposit, 1)
    , ('ValidatorRedeemerWithdraw, 2)
    , ('ValidatorRedeemerCollect_Protocol_Commission, 3)
    , ('ValidatorRedeemerCollect_Managers_Commission, 4)
    , ('ValidatorRedeemerCollect_Delegators_Commission, 5)
    , ('ValidatorRedeemerReIndexing, 6)
    , ('ValidatorRedeemerBalanceAssets, 7)
    , ('ValidatorRedeemerEmergency, 8)
    , ('ValidatorRedeemerDelete, 9)
    ]

--------------------------------------------------------------------------------2

getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType)) = Just "UpdateMinADA"
getValidatorRedeemerName (Just (ValidatorRedeemerDeposit ValidatorRedeemerDepositType {})) = Just "Deposit"
getValidatorRedeemerName (Just (ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType {})) = Just "Withdraw"
getValidatorRedeemerName (Just (ValidatorRedeemerCollect_Protocol_Commission ValidatorRedeemerCollect_Protocol_CommissionType {})) = Just "Collect_Protocol_Commission"
getValidatorRedeemerName (Just (ValidatorRedeemerCollect_Managers_Commission ValidatorRedeemerCollect_Managers_CommissionType {})) = Just "Collect_Managers_Commission"
getValidatorRedeemerName (Just (ValidatorRedeemerCollect_Delegators_Commission ValidatorRedeemerCollect_Delegators_CommissionType {})) = Just "Collect_Delegators_Commission"
getValidatorRedeemerName (Just (ValidatorRedeemerReIndexing ValidatorRedeemerReIndexingType {})) = Just "ReIndexing"
getValidatorRedeemerName (Just (ValidatorRedeemerBalanceAssets ValidatorRedeemerBalanceAssetsType {})) = Just "BalanceAssets"
getValidatorRedeemerName (Just (ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType)) = Just "Emergency"
getValidatorRedeemerName (Just (ValidatorRedeemerDelete ValidatorRedeemerDeleteType)) = Just "Delete"
getValidatorRedeemerName _ = Nothing

--------------------------------------------------------------------------------2

mkMintIDRedeemer :: LedgerApiV2.Redeemer
mkMintIDRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerMintID PolicyRedeemerMintIDType

mkBurnIDRedeemer :: LedgerApiV2.Redeemer
mkBurnIDRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            PolicyRedeemerBurnID PolicyRedeemerBurnIDType

--------------------------------------------------------------------------------2

mkUpdateMinADARedeemer :: LedgerApiV2.Redeemer
mkUpdateMinADARedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType

mkDepositRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkDepositRedeemer date' deposit' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDeposit $
                ValidatorRedeemerDepositType date' deposit'

mkWithdrawRedeemer :: LedgerApiV2.POSIXTime -> Integer -> Integer -> LedgerApiV2.Redeemer
mkWithdrawRedeemer date' withdraw' withdrawPlusComissions' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerWithdraw $
                ValidatorRedeemerWithdrawType date' withdraw' withdrawPlusComissions'

mkCollect_Protocol_CommissionRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkCollect_Protocol_CommissionRedeemer date' amount' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerCollect_Protocol_Commission $
                ValidatorRedeemerCollect_Protocol_CommissionType date' amount'

mkCollect_Managers_CommissionRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkCollect_Managers_CommissionRedeemer date' amount' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerCollect_Managers_Commission $
                ValidatorRedeemerCollect_Managers_CommissionType date' amount'

mkCollect_Delegators_CommissionRedeemer :: LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.Redeemer
mkCollect_Delegators_CommissionRedeemer date' amount' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerCollect_Delegators_Commission $
                ValidatorRedeemerCollect_Delegators_CommissionType date' amount'

mkReIndexingRedeemer :: T.InvestUnit -> T.InvestUnit -> LedgerApiV2.Redeemer
mkReIndexingRedeemer tokensToAdd' tokensToRemove' =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerReIndexing $
                ValidatorRedeemerReIndexingType tokensToAdd' tokensToRemove'

mkBalanceAssetsRedeemer :: [Integer] -> LedgerApiV2.Redeemer
mkBalanceAssetsRedeemer commissionsFT =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerBalanceAssets $
                ValidatorRedeemerBalanceAssetsType commissionsFT

mkEmergencyRedeemer :: LedgerApiV2.Redeemer
mkEmergencyRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

mkDeleteRedeemer :: LedgerApiV2.Redeemer
mkDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDelete ValidatorRedeemerDeleteType

--------------------------------------------------------------------------------2
