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
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Delegation.Types where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Data.Aeson as DataAeson
import qualified Data.OpenApi.Schema as DataOpenApiSchema
import qualified GHC.Generics as GHCGenerics
import qualified Ledger.Value as LedgerValue
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Schema
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Generic.Types as T
import qualified Protocol.Constants as T
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- Any change in the logic, datum or redeemer must change the version of the delegationVersion

delegationVersion :: Integer
delegationVersion = 1

ownVersion :: Integer
ownVersion = T.mkVersionWithDependency [ProtocolT.protocolVersion, FundT.fundVersion] delegationVersion

--------------------------------------------------------------------------------2
-- Params
--------------------------------------------------------------------------------2

data PolicyParams = PolicyParams
    { ppProtocolPolicyID_CS :: T.CS
    , ppDelegation_Validator_Hash :: LedgerApiV2.ValidatorHash
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq PolicyParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 =
        ppProtocolPolicyID_CS p1 == ppProtocolPolicyID_CS p2
            && ppDelegation_Validator_Hash p1 == ppDelegation_Validator_Hash p2

PlutusTx.makeLift ''PolicyParams
PlutusTx.makeIsDataIndexed ''PolicyParams [('PolicyParams, 0)]

data ValidatorParams = ValidatorParams
    { vpProtocolPolicyID_CS :: T.CS
    , vpTokenEmergencyAdminPolicy_CS :: LedgerApiV2.CurrencySymbol
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, DataOpenApiSchema.ToSchema, GHCGenerics.Generic, P.Eq, P.Ord, P.Show, Schema.ToSchema)

instance Eq ValidatorParams where
    {-# INLINEABLE (==) #-}
    p1 == p2 =
        vpProtocolPolicyID_CS p1 == vpProtocolPolicyID_CS p2
         && vpTokenEmergencyAdminPolicy_CS p1 == vpTokenEmergencyAdminPolicy_CS p2

PlutusTx.makeLift ''ValidatorParams
PlutusTx.makeIsDataIndexed ''ValidatorParams [('ValidatorParams, 0)]

--------------------------------------------------------------------------------2
-- Datums
--------------------------------------------------------------------------------2

data Delegation_DatumType = Delegation_DatumType
    { ddVersion :: Integer
    , ddDelegationPolicyID_CS :: T.CS
    , ddFundPolicy_CS :: T.CS
    , ddDelegatorPaymentPKH :: T.WalletPaymentPKH
    , ddDelegatorStakePKH :: Maybe T.WalletPaymentPKH
    , ddTokenMAYZ_AC :: LedgerValue.AssetClass
    , ddDelegated_MAYZ :: Integer
    , ddMinADA :: Integer
    }
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq Delegation_DatumType where
    {-# INLINEABLE (==) #-}
    sd1 == sd2 =
        ddVersion sd1 == ddVersion sd2
            && ddDelegationPolicyID_CS sd1 == ddDelegationPolicyID_CS sd2
            && ddFundPolicy_CS sd1 == ddFundPolicy_CS sd2
            && ddDelegatorPaymentPKH sd1 == ddDelegatorPaymentPKH sd2
            && ddDelegatorStakePKH sd1 == ddDelegatorStakePKH sd2
            && ddTokenMAYZ_AC sd1 == ddTokenMAYZ_AC sd2
            && ddDelegated_MAYZ sd1 == ddDelegated_MAYZ sd2
            && ddMinADA sd1 == ddMinADA sd2

PlutusTx.makeIsDataIndexed ''Delegation_DatumType [('Delegation_DatumType, 0)]

newtype ValidatorDatum
    = Delegation_Datum Delegation_DatumType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Eq, P.Ord, P.Show)

instance Eq ValidatorDatum where
    {-# INLINEABLE (==) #-}
    Delegation_Datum sd1 == Delegation_Datum sd2 = sd1 == sd2

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('Delegation_Datum, 0)]

{-# INLINEABLE getDelegation_DatumType #-}
getDelegation_DatumType :: ValidatorDatum -> Delegation_DatumType
getDelegation_DatumType (Delegation_Datum sdType) = sdType

{-# INLINEABLE getDelegation_DatumType_From_UTxO #-}
getDelegation_DatumType_From_UTxO :: LedgerApiV2.TxOut -> Delegation_DatumType
getDelegation_DatumType_From_UTxO utxo = case OnChainHelpers.getInlineDatum_From_TxOut @ValidatorDatum utxo of
    Nothing -> P.error "No Delegation Datum found"
    Just datum' -> getDelegation_DatumType datum'


instance T.ShowDatum ValidatorDatum where
    showCborAsDatumType cbor = case LedgerApiV2.fromBuiltinData @ValidatorDatum cbor of
        Nothing -> Nothing
        Just d -> Just $ P.show d

--------------------------------------------------------------------------------2

{-# INLINEABLE mkDelegation_Datum #-}
mkDelegation_Datum :: T.CS -> T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> LedgerValue.AssetClass -> Integer -> Integer -> ValidatorDatum
mkDelegation_Datum
    delegationPolicyID_CS
    fundPolicy_CS
    delegatorPaymentPKH
    delegatorStakePKH
    tokenMAYZ_AC
    delegated_MAYZ
    minADA =
        Delegation_Datum $
            mkDelegation_DatumType
                delegationPolicyID_CS
                fundPolicy_CS
                delegatorPaymentPKH
                delegatorStakePKH
                tokenMAYZ_AC
                delegated_MAYZ
                minADA

{-# INLINEABLE mkDelegation_DatumType #-}
mkDelegation_DatumType :: T.CS -> T.CS -> T.WalletPaymentPKH -> Maybe T.WalletPaymentPKH -> LedgerValue.AssetClass -> Integer -> Integer -> Delegation_DatumType
mkDelegation_DatumType = Delegation_DatumType ownVersion

mkDatum :: Delegation_DatumType -> LedgerApiV2.Datum
mkDatum = LedgerApiV2.Datum . LedgerApiV2.toBuiltinData . Delegation_Datum

--------------------------------------------------------------------------------2
-- PolicyRedeemer
--------------------------------------------------------------------------------2

data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerMintIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerMintIDType
    [('PolicyRedeemerMintIDType, 0)]

data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq PolicyRedeemerBurnIDType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [('PolicyRedeemerBurnIDType, 0)]

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
    [ ('PolicyRedeemerMintID, 1)
    , ('PolicyRedeemerBurnID, 2)
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

newtype ValidatorRedeemerDepositType = ValidatorRedeemerDepositType {vrdDelegated_MAYZ_Change :: Integer}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDepositType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = vrdDelegated_MAYZ_Change r1 == vrdDelegated_MAYZ_Change r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDepositType [('ValidatorRedeemerDepositType, 0)]

newtype ValidatorRedeemerWithdrawType = ValidatorRedeemerWithdrawType {vrdwDelegated_MAYZ_Change :: Integer}
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerWithdrawType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = vrdwDelegated_MAYZ_Change r1 == vrdwDelegated_MAYZ_Change r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerWithdrawType [('ValidatorRedeemerWithdrawType, 0)]

data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerDeleteType where
    {-# INLINEABLE (==) #-}
    (==) :: ValidatorRedeemerDeleteType -> ValidatorRedeemerDeleteType -> Bool
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDeleteType [('ValidatorRedeemerDeleteType, 0)]

data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemerEmergencyType where
    {-# INLINEABLE (==) #-}
    r1 == r2 = r1 == r2

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]

data ValidatorRedeemer
    = ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    deriving (DataAeson.FromJSON, DataAeson.ToJSON, GHCGenerics.Generic, P.Show)

instance Eq ValidatorRedeemer where
    {-# INLINEABLE (==) #-}
    ValidatorRedeemerUpdateMinADA rmcp1 == ValidatorRedeemerUpdateMinADA rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerDeposit rmcp1 == ValidatorRedeemerDeposit rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerWithdraw rmcp1 == ValidatorRedeemerWithdraw rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerDelete rmcp1 == ValidatorRedeemerDelete rmcp2 = rmcp1 == rmcp2
    ValidatorRedeemerEmergency rmcp1 == ValidatorRedeemerEmergency rmcp2 = rmcp1 == rmcp2
    _ == _ = False

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateMinADA, 0)
    , ('ValidatorRedeemerDeposit, 1)
    , ('ValidatorRedeemerWithdraw, 2)
    , ('ValidatorRedeemerDelete, 3)
    , ('ValidatorRedeemerEmergency, 4)
    ]

--------------------------------------------------------------------------------2

getValidatorRedeemerName :: Maybe ValidatorRedeemer -> Maybe P.String
getValidatorRedeemerName (Just (ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType)) = Just "UpdateMinADA"
getValidatorRedeemerName (Just (ValidatorRedeemerDeposit ValidatorRedeemerDepositType {})) = Just "Deposit"
getValidatorRedeemerName (Just (ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType {})) = Just "Withdraw"
getValidatorRedeemerName (Just (ValidatorRedeemerDelete ValidatorRedeemerDeleteType)) = Just "Delete"
getValidatorRedeemerName (Just (ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType)) = Just "Emergency"
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

mkDepositRedeemer :: Integer -> LedgerApiV2.Redeemer
mkDepositRedeemer delegated_MAYZ_Change =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDeposit $
                ValidatorRedeemerDepositType delegated_MAYZ_Change

mkWithdrawRedeemer :: Integer -> LedgerApiV2.Redeemer
mkWithdrawRedeemer delegated_MAYZ_Change =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerWithdraw $
                ValidatorRedeemerWithdrawType delegated_MAYZ_Change

mkDeleteRedeemer :: LedgerApiV2.Redeemer
mkDeleteRedeemer =
    LedgerApiV2.Redeemer $
        LedgerApiV2.toBuiltinData $
            ValidatorRedeemerDelete ValidatorRedeemerDeleteType

--------------------------------------------------------------------------------2
