{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

----------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
----------------------------------------------------------------------------2

module Protocol.Fund.Holding.OnChain where

----------------------------------------------------------------------------2
-- Import Externos
----------------------------------------------------------------------------2

import qualified Ledger
import qualified Ledger.Ada as LedgerAda
import qualified Ledger.Value as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.Prelude

----------------------------------------------------------------------------2
-- Import Internos
----------------------------------------------------------------------------2

import qualified Generic.Constants as T
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Generic.Types as T
import qualified PlutusTx.Ratio as TxRatio
import qualified Protocol.Constants as T
import qualified Protocol.Fund.Helpers as FundHelpers
import qualified Protocol.Fund.Holding.Types as T
import qualified Protocol.Fund.InvestUnit.Types as InvestUnitT
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Types as T

----------------------------------------------------------------------------2
-- Modulo
----------------------------------------------------------------------------2

-- Any change in the logic, datum or redeemer must change the version of the fundVersion on Protocol.Fund.Types

--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID (T.PolicyParams !fundPolicy_CS) !redRaw !ctxRaw =
    if traceIfFalse "" useThisToMakeScriptUnique
        && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
        && case redeemer of
            (T.PolicyRedeemerMintID _) ->
                ------------------
                -- it runs along with Fund Validator (ValidatorRedeemerFundHoldingAdd)
                ------------------
                -- Que se consuma FundDatum con redeemer correcto (AddHolding)
                -- Para identificar el correcto FundDatum necesita la póliza Fund ID que está en los parámetros de esta póliza.
                -- Que se genere salida con nuevo HoldingDatum en Holding Val (Holding Val está indicada en FundDatum)
                -- Que el HoldingDatum sea correcto
                -- Que se mintee Holding ID con own póliza
                -- Que el HoldingDatum tenga el Holding ID
                ------------------
                traceIfFalse "not isMintingFundHoldingID" isMintingFundHoldingID
                    && traceIfFalse "not isCorrect_Redeemer_Fund" (isCorrect_Redeemer_Fund isFundValidatorRedeemerFundHoldingAdd)
                    && traceIfFalse "not isCorrect_Output_FundHolding_Datum" isCorrect_Output_FundHolding_Datum
                    && traceIfFalse "not isCorrect_Output_FundHolding_Value" isCorrect_Output_FundHolding_Value
                where
                    ------------------
                    !outputs_txOuts =
                        [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)
                        ]
                    ------------------
                    -- 0 out is the FundDatum
                    -- 1 out is the FundHoldingDatum
                    ------------------
                    !outputs_txOuts_index1 =
                        if length outputs_txOuts < 2
                            then traceError "Expected at least two outputs to script addresses"
                            else outputs_txOuts !! 1
                    ------------------
                    !input_TxOut_And_FundDatum = (\(_, !txOut, !datum) -> (txOut, datum)) input_TxOutRef_TxOut_And_FundDatum
                    ------------------
                    !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundDatum
                    ------------------
                    !fundHolding_Index = FundT.fdHoldingsIndex fundDatum_In
                    !fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
                    !fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
                    ------------------
                    !fundHoldingValidator_Hash = FundT.fdFundHoldingValidator_Hash fundDatum_In
                    !fundHoldingValidator_Address = Ledger.scriptHashAddress fundHoldingValidator_Hash
                    ------------------
                    !output_Own_TxOut_And_FundHoldingDatum =
                        fromMaybe
                            (traceError "Expected FundHolding at output index 1")
                            ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                @T.ValidatorDatum
                                @T.FundHoldingDatumType
                                ctx
                                outputs_txOuts_index1
                                fundHoldingID_AC
                                (Just fundHoldingValidator_Address)
                                T.getFundHolding_DatumType
                            )
                    ------------------
                    !fundHoldingDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                    ------------------
                    !valueFor_Mint_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC 1
                    ------------------
                    !minADA_For_FundHoldingDatum_Out = T.hdMinADA fundHoldingDatum_Out
                    !value_MinADA_For_FundHoldingDatum_Out = LedgerAda.lovelaceValueOf minADA_For_FundHoldingDatum_Out
                    !valueFor_FundHoldingDatum_Out_Control = valueFor_Mint_FundHoldingID <> value_MinADA_For_FundHoldingDatum_Out
                    ------------------
                    !fundHoldingDatum_Out_Control =
                        T.FundHoldingDatumType
                            { T.hdVersion = T.ownVersion
                            , T.hdFundHolding_Index = fundHolding_Index
                            , T.hdSubTotal_FT_Minted_Accumulated = 0
                            , T.hdSubTotal_FT_Minted = 0
                            , T.hdSubTotal_FT_Commissions = 0
                            , T.hdSubTotal_FT_Commissions_Total = 0
                            , T.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 = 0
                            , T.hdSubTotal_FT_Commissions_Collected_Protocol = 0
                            , T.hdSubTotal_FT_Commissions_Collected_Managers = 0
                            , T.hdSubTotal_FT_Commissions_Collected_Delegators = 0
                            , T.hdMinADA = minADA_For_FundHoldingDatum_Out
                            }
                    ------------------
                    isFundValidatorRedeemerFundHoldingAdd :: FundT.ValidatorRedeemer -> Bool
                    isFundValidatorRedeemerFundHoldingAdd redemeerToCheck = case redemeerToCheck of
                        FundT.ValidatorRedeemerFundHoldingAdd _ -> True
                        _ -> False
                    ------------------
                    isMintingFundHoldingID :: Bool
                    !isMintingFundHoldingID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_FundHoldingID
                    -----------------
                    isCorrect_Output_FundHolding_Datum :: Bool
                    !isCorrect_Output_FundHolding_Datum =
                        fundHoldingDatum_Out `OnChainHelpers.isUnsafeEqDatums` fundHoldingDatum_Out_Control
                    ------------------
                    isCorrect_Output_FundHolding_Value :: Bool
                    !isCorrect_Output_FundHolding_Value =
                        let
                            !valueOf_FundHoldingDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                        in
                            valueOf_FundHoldingDatum_Out `OnChainHelpers.isEqValue` valueFor_FundHoldingDatum_Out_Control
            ------------------
            (T.PolicyRedeemerBurnID _) ->
                ------------------
                -- it runs along with Fund Validator (ValidatorRedeemerFundHoldingDelete)
                ------------------
                -- it runs along with FundHolding Validator (ValidatorRedeemerDelete)
                ------------------
                -- Que se consuma FundDatum con redeemer correcto (DeleteHolding)
                -- Para identificar el correcto FundDatum necesita la póliza Fund ID que está en los parámetros de esta póliza.
                -- Que se queme Holding ID con own póliza
                -- que no haya tokens en el FundHolding
                ------------------
                traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
                    && traceIfFalse "not isCorrect_Redeemer_Fund" (isCorrect_Redeemer_Fund isFundValidatorRedeemerFundHoldingDelete)
                    && traceIfFalse "not isCorrect_Redeemer_FundHolding" (isCorrect_Redeemer_FundHolding isFundHoldingValidatorRedeemerDelete)
                    && traceIfFalse "not isZeroAssets" isZeroAssets
                where
                    ------------------
                    !input_TxOutRef_TxOut_And_FundHoldingDatum =
                        case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_CS
                            @T.ValidatorDatum
                            @T.FundHoldingDatumType
                            ctx
                            inputs_TxOutRefs_TxOuts
                            fundHoldingPolicyID_CS
                            T.getFundHolding_DatumType of
                            [x] -> x
                            _ -> traceError "Expected exactly one FundHolding input"
                    ------------------
                    !input_TxOut_And_FundHoldingDatum = (\(_, !txOut, !datum) -> (txOut, datum)) input_TxOutRef_TxOut_And_FundHoldingDatum
                    ------------------
                    !fundHoldingDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum input_TxOut_And_FundHoldingDatum
                    ------------------
                    !fundHolding_Index = T.hdFundHolding_Index fundHoldingDatum_In
                    !fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
                    !fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
                    ------------------
                    !valueFor_Mint_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC 1
                    !valueFor_Burn_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC (negate 1)
                    ------------------
                    isBurningFundHoldingID :: Bool
                    !isBurningFundHoldingID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_FundHoldingID
                    -----------------
                    isFundValidatorRedeemerFundHoldingDelete :: FundT.ValidatorRedeemer -> Bool
                    isFundValidatorRedeemerFundHoldingDelete !redemeerToCheck = case redemeerToCheck of
                        FundT.ValidatorRedeemerFundHoldingDelete _ -> True
                        _ -> False
                    -----------------
                    isFundHoldingValidatorRedeemerDelete :: T.ValidatorRedeemer -> Bool
                    isFundHoldingValidatorRedeemerDelete !redemeerToCheck = case redemeerToCheck of
                        T.ValidatorRedeemerDelete _ -> True
                        _ -> False
                    ------------------
                    isZeroAssets :: Bool
                    !isZeroAssets =
                        let
                            ------------------
                            !minADA_For_FundHoldingDatum_In = T.hdMinADA fundHoldingDatum_In
                            !value_MinADA_For_FundHoldingDatum_In = LedgerAda.lovelaceValueOf minADA_For_FundHoldingDatum_In
                            !valueFor_FundHoldingDatum_In_Control = valueFor_Mint_FundHoldingID <> value_MinADA_For_FundHoldingDatum_In
                            ------------------
                            !valueOf_FundHoldingDatum_In = OnChainHelpers.getValue_In_TxOut_And_Datum input_TxOut_And_FundHoldingDatum
                        in
                            valueOf_FundHoldingDatum_In `OnChainHelpers.isEqValue` valueFor_FundHoldingDatum_In_Control
                    ------------------
                    isCorrect_Redeemer_FundHolding :: (T.ValidatorRedeemer -> Bool) -> Bool
                    isCorrect_Redeemer_FundHolding !isRedeemerType =
                        let
                            !redeemerFor_FundHoldingDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(txOutRef, _, _) -> txOutRef) input_TxOutRef_TxOut_And_FundHoldingDatum) info
                        in
                            case redeemerFor_FundHoldingDatum' of
                                Nothing -> False
                                Just redeemerFor_FundHoldingDatum -> case LedgerApiV2.fromBuiltinData @T.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_FundHoldingDatum of
                                    Just x -> isRedeemerType x
                                    _ -> False
        then ------------------
            ()
        else error ()
    where
        ------------------
        !useThisToMakeScriptUnique = fundPolicy_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        ------------------
        !fundHoldingPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        ------------------
        !inputs_TxOutRefs_TxOuts =
            [ (LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | !txInfoInput <- LedgerApiV2.txInfoInputs info, OnChainHelpers.isScriptAddress $ LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
            ]
        ------------------
        !input_TxOutRef_TxOut_And_FundDatum =
            case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC
                @FundT.ValidatorDatum
                @FundT.FundDatumType
                ctx
                inputs_TxOutRefs_TxOuts
                fundID_AC
                FundT.getFund_DatumType of
                [x] -> x
                _ -> traceError "Expected exactly one Fund input"
        ------------------
        isCorrect_Redeemer_Fund :: (FundT.ValidatorRedeemer -> Bool) -> Bool
        isCorrect_Redeemer_Fund !isRedeemerType =
            let
                !redeemerFor_FundDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(!txOutRef, _, _) -> txOutRef) input_TxOutRef_TxOut_And_FundDatum) info
            in
                case redeemerFor_FundDatum' of
                    Nothing -> False
                    Just !redeemerFor_FundDatum -> case LedgerApiV2.fromBuiltinData @FundT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_FundDatum of
                        Just x -> isRedeemerType x
                        _ -> False

------------------

----------------------------------------------------------------------------2

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator (T.ValidatorParams !protocolPolicyID_CS !fundPolicy_CS !tokenEmergencyAdminPolicy_CS) !datumRaw !redRaw !ctxRaw =
    let
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        -- Si esta el token de emergencia se saltea todos los controles
        isEmergencyRedeemer :: Bool
        !isEmergencyRedeemer =
            case redeemer of
                (T.ValidatorRedeemerEmergency _) -> True
                _ -> False
    in
        ------------------
        case isEmergencyRedeemer of
            True ->
                let
                    !tokenEmergencyAdmin_AC = LedgerValue.AssetClass (tokenEmergencyAdminPolicy_CS, T.protocolTokenEmergencyAdmin_TN)
                    -- search emergency admin token in output 0
                    !isEmergencyAdminTokenPresent = OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue $ head (LedgerApiV2.txInfoOutputs info)) tokenEmergencyAdmin_AC
                in
                    if traceIfFalse "not isEmergencyAdminTokenPresent" isEmergencyAdminTokenPresent
                        then ()
                        else error ()
            False ->
                if traceIfFalse "" useThisToMakeScriptUnique
                    && traceIfFalse "not isValidRange" (OnChainHelpers.isValidRange info T.validTxTimeRange)
                    && traceIfFalse "Expected at least one FundHolding input" (not (null inputs_Own_TxOuts))
                    && validateRedeemer getRedeemerType
                    then ()
                    else error ()
                where
                    ------------------
                    !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol && fundPolicy_CS /= LedgerApiV2.adaSymbol
                    ------------------
                    !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
                    ------------------
                    !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
                    !fundHolding_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                    ------------------
                    !inputs_Own_TxOuts =
                        [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info, let
                                                                                                                        !address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                                    in
                                                                                                                        OnChainHelpers.isScriptAddress address && address == fundHolding_Validator_Address
                        ]
                    ------------------
                    !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                    ------------------
                    !outputs_txOuts =
                        [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress txOut)
                        ]
                    ------------------
                    !redeemerDepositAndWithdraw = 1
                    !redeemerReIdx = 2
                    !redeemerDelete = 3
                    !redeemerCommissions = 4
                    !redeemerUpdateMinADA = 5
                    !redeemerBalance = 6
                    ------------------
                    getRedeemerType :: Integer
                    !getRedeemerType = case redeemer of
                        (T.ValidatorRedeemerDeposit _) -> redeemerDepositAndWithdraw
                        (T.ValidatorRedeemerWithdraw _) -> redeemerDepositAndWithdraw
                        (T.ValidatorRedeemerReIndexing _) -> redeemerReIdx
                        (T.ValidatorRedeemerCollect_Protocol_Commission _) -> redeemerCommissions
                        (T.ValidatorRedeemerCollect_Managers_Commission _) -> redeemerCommissions
                        (T.ValidatorRedeemerCollect_Delegators_Commission _) -> redeemerCommissions
                        (T.ValidatorRedeemerDelete _) -> redeemerDelete
                        (T.ValidatorRedeemerUpdateMinADA _) -> redeemerUpdateMinADA
                        (T.ValidatorRedeemerBalanceAssets _) -> redeemerBalance
                        _ -> traceError "Invalid redeemer"
                    ------------------
                    validateRedeemer :: Integer -> Bool
                    validateRedeemer !redeemerType
                        | redeemerType == redeemerDelete = validateDelete
                        | otherwise = validateAllButDelete
                        where
                            ------------------
                            validateDelete :: Bool
                            validateDelete =
                                ------------------
                                -- it runs along with Fund Validator (ValidatorRedeemerFundHoldingDelete)
                                ------------------
                                -- it runs along with FundHolding ID Policy (PolicyRedeemerBurnID)
                                ------------------
                                -- que sea fund admin, eso lo controla la validacion del Fund
                                -- Que se queme Holding ID con la correcta póliza indicada en FundDatum
                                -- Para identificar el correcto FundDatum necesita la póliza Fund ID que está en los parámetros de esta póliza.
                                -- que no haya tokens en el fundHOlding. Se verifica en la poliza de PolicyRedeemerBurnID
                                ------------------
                                traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                    && traceIfFalse "not isBurningFundHoldingID" isBurningFundHoldingID
                                where
                                    ------------------

                                    ------------------
                                    !outputs_txOuts_index0 =
                                        if null outputs_txOuts
                                            then traceError "Expected at least one output to script addresses"
                                            else head outputs_txOuts
                                    ------------------
                                    -- 0 out is the FundDatum
                                    ------------------
                                    !output_TxOut_And_FundDatum =
                                        fromMaybe
                                            (traceError "Expected Fund at output index 0")
                                            ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                                @FundT.ValidatorDatum
                                                @FundT.FundDatumType
                                                ctx
                                                outputs_txOuts_index0
                                                fundID_AC
                                                Nothing
                                                FundT.getFund_DatumType
                                            )
                                    ------------------
                                    !fundDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_TxOut_And_FundDatum
                                    ------------------
                                    !fundHoldingPolicyID_CS = FundT.fdFundHoldingPolicyID_CS fundDatum_Out
                                    ------------------
                                    isBurningFundHoldingID :: Bool
                                    !isBurningFundHoldingID = OnChainHelpers.isNFT_Burning_With_CS fundHoldingPolicyID_CS info
                            ------------------
                            validateAllButDelete :: Bool
                            validateAllButDelete
                                | redeemerType == redeemerBalance = validateBalance
                                | otherwise = validateAllButBalance
                                where
                                    ------------------
                                    !inputsRef_TxOuts =
                                        [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info, OnChainHelpers.isScriptAddress $ LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                        ]
                                    ------------------
                                    !inputRef_TxOut_And_FundDatum =
                                        case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                            @FundT.ValidatorDatum
                                            @FundT.FundDatumType
                                            ctx
                                            inputsRef_TxOuts
                                            fundID_AC
                                            FundT.getFund_DatumType of
                                            [x] -> x
                                            _ -> traceError "Expected exactly one Fund input ref"
                                    ------------------
                                    !outputs_txOuts_index0 =
                                        if null outputs_txOuts
                                            then traceError "Expected at least one output to script addresses"
                                            else head outputs_txOuts
                                    ------------------
                                    !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_FundDatum
                                    ------------------
                                    !fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
                                    !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
                                    ------------------
                                    !fundHoldingPolicyID_CS = FundT.fdFundHoldingPolicyID_CS fundDatum_In
                                    ------------------
                                    isCorrect_Output_FundHolding_Datum :: T.FundHoldingDatumType -> T.FundHoldingDatumType -> Bool
                                    isCorrect_Output_FundHolding_Datum !fundHoldingDatum_Out' !fundHoldingDatum_Control = fundHoldingDatum_Out' `OnChainHelpers.isUnsafeEqDatums` fundHoldingDatum_Control
                                    ------------------
                                    isCorrect_Output_FundHolding_Value :: LedgerValue.Value -> LedgerValue.Value -> Bool
                                    isCorrect_Output_FundHolding_Value !valueOf_FundHoldingDatum_Out' !valueFor_FundHoldingDatum_Control =
                                        valueOf_FundHoldingDatum_Out' `OnChainHelpers.isEqValue` valueFor_FundHoldingDatum_Control
                                    ------------------
                                    validateAdminAction :: [T.WalletPaymentPKH] -> LedgerValue.AssetClass -> Bool
                                    validateAdminAction !admins' !tokenAdmin_AC =
                                        -- Que este el token de admin presente en output 1 o Que sea Protocol Admin
                                        traceIfFalse "not isSignedByAny admins nor isAdminTokenPresent" (OnChainHelpers.isSignedByAny admins' info || isAdminTokenPresent)
                                        where
                                            ------------------
                                            isAdminTokenPresent :: Bool
                                            isAdminTokenPresent = case LedgerApiV2.txInfoOutputs info of
                                                [] -> False
                                                -- search admin token in output 0
                                                (output : _) -> OnChainHelpers.isToken_With_AC_InValue (LedgerApiV2.txOutValue output) tokenAdmin_AC
                                    ------------------
                                    validateFundAdminAction :: FundT.FundDatumType -> Bool
                                    validateFundAdminAction !fundDatum_In' =
                                        ------------------
                                        let
                                            !admins' = T.getAdmins fundDatum_In'
                                            !tokenAdminPolicy_CS = T.getAdminToken_CS fundDatum_In
                                            !tokenAdmin_AC = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.fundTokenAdmin_TN)
                                        in
                                            ------------------
                                            validateAdminAction admins' tokenAdmin_AC
                                    ------------------
                                    validateBalance :: Bool
                                    validateBalance = case redeemer of
                                        (T.ValidatorRedeemerBalanceAssets (T.ValidatorRedeemerBalanceAssetsType !alterCommissionsFT)) ->
                                            ---------------------
                                            -- it runs alone
                                            ---------------------
                                            -- Que sea Fund Admin
                                            -- Ya esta controlado que haya mas de un input, por lo tanto hay al menos misma cantidad de outputs
                                            -- por cuesitones de complejidad, se limita a dos entradas y dos salidas
                                            -- La cantidad de FT modificadas, sumas y restas en el array alterCommissionsFT, tiene que ser 0
                                            -- Que se modifique datum correctamente: si se alteraron FT Commissions, que se hayan movido proporcionalmente a los rates
                                            -- Que la suma total de los values de salida sea igual a la suma total de los values de entrada
                                            -- Que cada value de salida tiene lo que dice el datum de minADA, la cantidad de FT y el funHoldingID correspondiente
                                            ---------------------------------
                                            validateFundAdminAction fundDatum_In
                                                && traceIfFalse "wrong inputs, outputs, redeemers len" (cantInputs == 2 && cantInputs == cantOutputs && length alterCommissionsFT == cantOutputs)
                                                && traceIfFalse "wrong redeemers move FT total" (sum alterCommissionsFT == 0)
                                                && traceIfFalse "wrong outputs datums" isCorrect_Outputs_FundHoldingDatums_With_UpdatedCommissionsAndRate
                                                && traceIfFalse "wrong outputs values" (isCorrect_Output_FundHolding_Values_SameTotal && isCorrect_Output_FundHolding_Values)
                                            where
                                                ------------------
                                                !outputs_Own_TxOuts =
                                                    [ txOut | !txOut <- outputs_txOuts, LedgerApiV2.txOutAddress txOut == fundHolding_Validator_Address
                                                    ]
                                                ------------------
                                                !inputs_Own_TxOuts_And_FundHoldingDatums =
                                                    OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                                                        @T.ValidatorDatum
                                                        @T.FundHoldingDatumType
                                                        ctx
                                                        inputs_Own_TxOuts
                                                        fundHoldingPolicyID_CS
                                                        T.getFundHolding_DatumType
                                                ------------------
                                                !cantInputs = length inputs_Own_TxOuts_And_FundHoldingDatums
                                                ------------------
                                                !outputs_Own_TxOuts_And_FundHoldingDatums =
                                                    OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                                                        @T.ValidatorDatum
                                                        @T.FundHoldingDatumType
                                                        ctx
                                                        outputs_Own_TxOuts
                                                        fundHoldingPolicyID_CS
                                                        T.getFundHolding_DatumType
                                                ------------------
                                                !cantOutputs = length outputs_Own_TxOuts_And_FundHoldingDatums
                                                ------------------
                                                !inputs_Values_And_FundHoldingDatums = [(OnChainHelpers.getValue_In_TxOut_And_Datum txOut_And_Datum, OnChainHelpers.getDatum_In_TxOut_And_Datum txOut_And_Datum) | !txOut_And_Datum <- inputs_Own_TxOuts_And_FundHoldingDatums]
                                                !outputs_Values_And_FundHoldingDatums = [(OnChainHelpers.getValue_In_TxOut_And_Datum txOut_And_Datum, OnChainHelpers.getDatum_In_TxOut_And_Datum txOut_And_Datum) | !txOut_And_Datum <- outputs_Own_TxOuts_And_FundHoldingDatums]
                                                ------------------
                                                !fundHoldingDatums_In = snd <$> inputs_Values_And_FundHoldingDatums
                                                !fundHoldingDatums_Out = snd <$> outputs_Values_And_FundHoldingDatums
                                                ------------------
                                                !valueOf_fundHoldingDatums_In = fst <$> inputs_Values_And_FundHoldingDatums
                                                !valueOf_fundHoldingDatums_Out = fst <$> outputs_Values_And_FundHoldingDatums
                                                ------------------
                                                isCorrect_Outputs_FundHoldingDatums_With_UpdatedCommissionsAndRate :: Bool
                                                isCorrect_Outputs_FundHoldingDatums_With_UpdatedCommissionsAndRate =
                                                    let
                                                        --------------------
                                                        -- Ensure lists have the required elements
                                                        -- NOTE: ya se valida antes
                                                        -- isValidLength = length fundHoldingDatums_In == 2 && length fundHoldingDatums_Out == 2 && length alterCommissionsFT == 2
                                                    in
                                                        --------------------
                                                        -- if not isValidLength
                                                        --     then traceError "Insufficient datums for validation"
                                                        --     else
                                                                let
                                                                    -- De esta forma aseguramos el orden, y con esto ya sabemos que del primero  es el que resta y el segundo el que suma
                                                        --------------------
                                                        fnOrder :: ([a] -> a, [a] -> a)
                                                        fnOrder =
                                                            case alterCommissionsFT of
                                                                (x : y : _) -> if x <= y then (head, head . tail) else (head . tail, head)
                                                                _ -> traceError "wrong redeemers move FT error list"
                                                        !((alterCommissions1, _alterCommissions2), (datumIn1, datumIn2), (datumOut1, datumOut2)) =
                                                            ( (fst fnOrder alterCommissionsFT, snd fnOrder alterCommissionsFT)
                                                            , (fst fnOrder fundHoldingDatums_In, snd fnOrder fundHoldingDatums_In)
                                                            , (fst fnOrder fundHoldingDatums_Out, snd fnOrder fundHoldingDatums_Out)
                                                            )
                                                        --------------------
                                                        !(changeRelease, changeCommissionsTotal, chageCollectedProtocol, chageCollectedManagers, chageCollectedDelegators) =
                                                            if alterCommissions1 < 0
                                                                then
                                                                    if T.hdSubTotal_FT_Commissions datumIn1 > 0
                                                                        then
                                                                            let
                                                                                !changeRatio' = TxRatio.unsafeRatio alterCommissions1 (T.hdSubTotal_FT_Commissions datumIn1)
                                                                                applyRatio :: Integer -> TxRatio.Rational -> Integer
                                                                                applyRatio val changeRatio'' =
                                                                                    TxRatio.truncate (changeRatio'' * TxRatio.fromInteger val)
                                                                            in
                                                                                ( applyRatio (T.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 datumIn1) changeRatio'
                                                                                , applyRatio (T.hdSubTotal_FT_Commissions_Total datumIn1) changeRatio'
                                                                                , applyRatio (T.hdSubTotal_FT_Commissions_Collected_Protocol datumIn1) changeRatio'
                                                                                , applyRatio (T.hdSubTotal_FT_Commissions_Collected_Managers datumIn1) changeRatio'
                                                                                , applyRatio (T.hdSubTotal_FT_Commissions_Collected_Delegators datumIn1) changeRatio'
                                                                                )
                                                                        else traceError "Expected Commissions to take bigger than 0"
                                                                else (0, 0, 0, 0, 0)
                                                        --------------------
                                                        !newCommissions1 = T.hdSubTotal_FT_Commissions datumIn1 + alterCommissions1
                                                        !newCommissionsTotal1 = T.hdSubTotal_FT_Commissions_Total datumIn1 + changeCommissionsTotal
                                                        !newRelease1 = T.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 datumIn1 + changeRelease
                                                        !newCollectProtocol1 = T.hdSubTotal_FT_Commissions_Collected_Protocol datumIn1 + chageCollectedProtocol
                                                        !newCollectManagers1 = T.hdSubTotal_FT_Commissions_Collected_Managers datumIn1 + chageCollectedManagers
                                                        !newCollectDelegators1 = T.hdSubTotal_FT_Commissions_Collected_Delegators datumIn1 + chageCollectedDelegators
                                                        --------------------
                                                        !newCommissions2 = T.hdSubTotal_FT_Commissions datumIn2 - alterCommissions1
                                                        !newCommissionsTotal2 = T.hdSubTotal_FT_Commissions_Total datumIn2 - changeCommissionsTotal
                                                        !newRelease2 = T.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 datumIn2 - changeRelease
                                                        !newCollectProtocol2 = T.hdSubTotal_FT_Commissions_Collected_Protocol datumIn2 - chageCollectedProtocol
                                                        !newCollectManagers2 = T.hdSubTotal_FT_Commissions_Collected_Managers datumIn2 - chageCollectedManagers
                                                        !newCollectDelegators2 = T.hdSubTotal_FT_Commissions_Collected_Delegators datumIn2 - chageCollectedDelegators
                                                        --------------------
                                                        -- Crear los datums de control para validar la salida
                                                        !datumControl1 = FundHelpers.mkUpdated_FundHolding_Datum_AfterBalancing datumIn1 (T.hdSubTotal_FT_Minted datumOut1) (T.hdSubTotal_FT_Minted_Accumulated datumOut1) newCommissions1 newCommissionsTotal1 newRelease1 newCollectProtocol1 newCollectManagers1 newCollectDelegators1
                                                        !datumControl2 = FundHelpers.mkUpdated_FundHolding_Datum_AfterBalancing datumIn2 (T.hdSubTotal_FT_Minted datumOut2) (T.hdSubTotal_FT_Minted_Accumulated datumOut2) newCommissions2 newCommissionsTotal2 newRelease2 newCollectProtocol2 newCollectManagers2 newCollectDelegators2
                                                        ---------------------------------------------
                                                        isCorrect_Output_FundHolding_SameTotal :: (T.FundHoldingDatumType -> Integer) -> Bool
                                                        isCorrect_Output_FundHolding_SameTotal !f =
                                                            f datumIn1 + f datumIn2 == f datumOut1 + f datumOut2
                                                        ------------------
                                                        isCorrect_Output_FundHolding_NonNegative :: (T.FundHoldingDatumType -> Integer) -> Bool
                                                        isCorrect_Output_FundHolding_NonNegative !f = f datumOut1 >= 0 && f datumOut2 >= 0
                                                    in
                                                        ------------------
                                                        -- Verificamos que los datums de salida coincidan con los controlados
                                                        OnChainHelpers.isUnsafeEqDatums datumControl1 datumOut1
                                                        && OnChainHelpers.isUnsafeEqDatums datumControl2 datumOut2
                                                        && traceIfFalse
                                                            "wrong datums values"
                                                            ( isCorrect_Output_FundHolding_NonNegative T.hdSubTotal_FT_Minted
                                                                && isCorrect_Output_FundHolding_NonNegative T.hdSubTotal_FT_Minted_Accumulated
                                                                && isCorrect_Output_FundHolding_NonNegative T.hdSubTotal_FT_Commissions
                                                                && isCorrect_Output_FundHolding_NonNegative T.hdSubTotal_FT_Commissions_Total
                                                                && isCorrect_Output_FundHolding_NonNegative T.hdSubTotal_FT_Commissions_Release_PerMonth_1e6
                                                                && isCorrect_Output_FundHolding_NonNegative T.hdSubTotal_FT_Commissions_Collected_Protocol
                                                                && isCorrect_Output_FundHolding_NonNegative T.hdSubTotal_FT_Commissions_Collected_Managers
                                                                && isCorrect_Output_FundHolding_NonNegative T.hdSubTotal_FT_Commissions_Collected_Delegators
                                                            )
                                                        && traceIfFalse
                                                            "wrong datums totals"
                                                            ( isCorrect_Output_FundHolding_SameTotal T.hdSubTotal_FT_Minted
                                                                && isCorrect_Output_FundHolding_SameTotal T.hdSubTotal_FT_Minted_Accumulated
                                                                && isCorrect_Output_FundHolding_SameTotal T.hdSubTotal_FT_Commissions
                                                                && isCorrect_Output_FundHolding_SameTotal T.hdSubTotal_FT_Commissions_Total
                                                                && isCorrect_Output_FundHolding_SameTotal T.hdSubTotal_FT_Commissions_Release_PerMonth_1e6
                                                                && isCorrect_Output_FundHolding_SameTotal T.hdSubTotal_FT_Commissions_Collected_Protocol
                                                                && isCorrect_Output_FundHolding_SameTotal T.hdSubTotal_FT_Commissions_Collected_Managers
                                                                && isCorrect_Output_FundHolding_SameTotal T.hdSubTotal_FT_Commissions_Collected_Delegators
                                                            )
                                                ------------------
                                                isCorrect_Output_FundHolding_Values_SameTotal :: Bool
                                                !isCorrect_Output_FundHolding_Values_SameTotal =
                                                    let
                                                        -- Helper function to combine token values by summing input and subtracting output
                                                        sumAndSubtractTokens :: LedgerApiV2.Value -> (LedgerApiV2.Value, LedgerApiV2.Value) -> LedgerApiV2.Value
                                                        sumAndSubtractTokens !accValue (!inValue, !outValue) =
                                                            (accValue <> inValue <> negate outValue)
                                                        -- Perform the fold to sum and subtract token values pairwise
                                                        !finalValue = foldl sumAndSubtractTokens (LedgerAda.lovelaceValueOf 0) (zip valueOf_fundHoldingDatums_In valueOf_fundHoldingDatums_Out)
                                                    in
                                                        -- Check that the final accumulated value is zero for all tokens
                                                        finalValue `OnChainHelpers.isEqValue` LedgerAda.lovelaceValueOf 0
                                                ------------------
                                                isCorrect_Output_FundHolding_Values :: Bool
                                                !isCorrect_Output_FundHolding_Values =
                                                    all checkOutput outputs_Values_And_FundHoldingDatums
                                                    where
                                                        checkOutput :: (LedgerValue.Value, T.FundHoldingDatumType) -> Bool
                                                        checkOutput (!valueOf_FundHoldingDatum_Out', !fundHoldingDatum_Out') =
                                                            ------------------
                                                            let
                                                                !minADA = T.hdMinADA fundHoldingDatum_Out'
                                                                ------------------
                                                                !commissionsFT = T.hdSubTotal_FT_Commissions fundHoldingDatum_Out'
                                                                ------------------
                                                                !fundHolding_Index' = T.hdFundHolding_Index fundHoldingDatum_Out'
                                                                !fundHoldingID_TN' = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index'
                                                                !fundHoldingID_AC' = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN')
                                                            in
                                                                ------------------
                                                                OnChainHelpers.isNFT_With_AC_InValue valueOf_FundHoldingDatum_Out' fundHoldingID_AC'
                                                                    -- min ADA tiene que ser al menos lo que marca el datum, puede ser más, en el caso de que se haya usado ADA como token dentro de la InvestUnit
                                                                    && OnChainHelpers.getValueOfLovelace valueOf_FundHoldingDatum_Out' >= minADA
                                                                    && OnChainHelpers.getAmt_With_AC_InValue valueOf_FundHoldingDatum_Out' fundFT_AC == commissionsFT
                                        _ -> False
                                    ------------------
                                    validateAllButBalance :: Bool
                                    validateAllButBalance
                                        ------------------
                                        | redeemerType == redeemerDepositAndWithdraw = validateDepositAndWithdraw
                                        | redeemerType == redeemerReIdx = validateReIdx
                                        | redeemerType == redeemerCommissions || redeemerType == redeemerUpdateMinADA = validateCommissionsAndUpdateMinADA
                                        | otherwise = False
                                        where
                                            ------------------
                                            !output_Own_TxOut_And_FundHoldingDatum =
                                                fromMaybe
                                                    (traceError "Expected FundHolding at output index 0")
                                                    ( OnChainHelpers.getTxOut_And_DatumType_From_TxOut_And_AC_And_Address
                                                        @T.ValidatorDatum
                                                        @T.FundHoldingDatumType
                                                        ctx
                                                        outputs_txOuts_index0
                                                        fundHoldingID_AC
                                                        (Just fundHolding_Validator_Address)
                                                        T.getFundHolding_DatumType
                                                    )
                                            -------------------
                                            !fundHoldingDatum_In = T.getFundHolding_DatumType datum
                                            ------------------
                                            !fundHolding_Index = T.hdFundHolding_Index fundHoldingDatum_In
                                            !fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
                                            !fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
                                            ------------------
                                            !valueOf_FundHoldingDatum_In = LedgerApiV2.txOutValue input_TxOut_BeingValidated
                                            ------------------
                                            !fundHoldingDatum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                                            -------------------
                                            !valueOf_FundHoldingDatum_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_FundHoldingDatum
                                            -------------------
                                            validateDepositAndWithdraw :: Bool
                                            validateDepositAndWithdraw =
                                                case redeemer of
                                                    (T.ValidatorRedeemerDeposit (T.ValidatorRedeemerDepositType !date !deposit)) ->
                                                        ------------------
                                                        -- it runs along with Fund Policy (PolicyRedeemerMintFT)
                                                        ------------------
                                                        traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                            && traceIfFalse "wrong date" (OnChainHelpers.isDateInRange date info)
                                                            && traceIfFalse "not Correct Deposit Amount" (FundHelpers.isCorrectAmount deposit (FundT.fdMaxDepositAndWithdraw fundDatum_In) investUnit_Granularity)
                                                            && traceIfFalse "not isMintingFT" isMintingFT
                                                            && traceIfFalse "wrong output datum" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Deposit)
                                                            && traceIfFalse "wrong output value" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out valueFor_FundHoldingDatum_Control_With_Tokens_And_FT)
                                                        where
                                                            ------------------
                                                            !deadline = FundT.fdDeadline fundDatum_In
                                                            !commissions_Table_Numerator_1e6 = FundT.fdCommissions_Table_Numerator_1e6 fundDatum_In
                                                            ---------------------
                                                            !(userFT, commissionsFT, commissions_FT_Release_PerMonth_1e6) =
                                                                FundHelpers.calculateDepositCommissionsUsingMonths commissions_Table_Numerator_1e6 deadline date deposit
                                                            ------------------
                                                            !valueOf_TokensForDeposit_Plus_FundHolding_Value = FundHelpers.createValue_WithTokensFrom_InvestUnit_Plus_FundHolding_Value valueOf_FundHoldingDatum_In investUnitTokens deposit True
                                                            ------------------
                                                            !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT
                                                            ------------------
                                                            !valueFor_FundHoldingDatum_Control_With_Tokens_And_FT = valueOf_TokensForDeposit_Plus_FundHolding_Value <> valueFor_FT_Commissions
                                                            ------------------
                                                            !fundHoldingDatum_Control_With_Deposit = FundHelpers.mkUpdated_FundHolding_Datum_With_Deposit fundHoldingDatum_In deposit userFT commissionsFT commissions_FT_Release_PerMonth_1e6
                                                            ------------------
                                                            isMintingFT :: Bool
                                                            !isMintingFT = OnChainHelpers.isOnlyToken_Minting_With_AC_AndAmt fundFT_AC deposit info
                                                    ------------------
                                                    (T.ValidatorRedeemerWithdraw (T.ValidatorRedeemerWithdrawType !date !withdraw !withdrawPlusComissions)) ->
                                                        ------------------
                                                        -- it runs along with Fund Policy (PolicyRedeemerBurnFT)
                                                        ------------------
                                                        -- there are no temporal restritions for withdraw
                                                        ------------------
                                                        traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                            && traceIfFalse "wrong date" (OnChainHelpers.isDateInRange date info)
                                                            && traceIfFalse "not Correct Withdraw Amount" (FundHelpers.isCorrectAmount withdraw (FundT.fdMaxDepositAndWithdraw fundDatum_In) investUnit_Granularity)
                                                            && traceIfFalse "not Correct Comissions" (FundHelpers.isCorrectCommissionsAmount commissionsForUserFT commissionsForUserFT_calculated investUnit_Granularity)
                                                            && traceIfFalse "not isEnough_FT_ForComission" isEnough_FT_ForComission
                                                            && traceIfFalse "not isEnough_Commissions_Release_PerMonth" isEnough_Commissions_Release_PerMonth
                                                            && traceIfFalse "not isBurningFT" isBurningFT
                                                            && traceIfFalse "wrong output datum" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Withdraw)
                                                            && traceIfFalse "wrong output value" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out valueFor_FundHoldingDatum_ControlWithout_Tokens_And_FT_for_Commissions)
                                                        where
                                                            ---------------------
                                                            !commissionsForUserFT = withdrawPlusComissions - withdraw
                                                            ---------------------
                                                            !deadline = FundT.fdDeadline fundDatum_In
                                                            !monthsRemaining = FundHelpers.getRemainingMonths deadline date
                                                            !commissions_Table_Numerator_1e6 = FundT.fdCommissions_Table_Numerator_1e6 fundDatum_In
                                                            ---------------------
                                                            !commissionsForUserFT_calculated = FundHelpers.calculateWithdrawCommissionsAvailable commissions_Table_Numerator_1e6 monthsRemaining withdraw investUnit_Granularity
                                                            !commissions_FT_Release_PerMonth_1e6_calculated = FundHelpers.calculateRelease monthsRemaining commissionsForUserFT
                                                            ------------------
                                                            !valueOf_TokensForWithdraw_Plus_FundHolding_Value = FundHelpers.createValue_WithTokensFrom_InvestUnit_Plus_FundHolding_Value valueOf_FundHoldingDatum_In investUnitTokens (negate withdrawPlusComissions) False
                                                            !valueFor_FT_CommissionsToGetBack = LedgerValue.assetClassValue fundFT_AC commissionsForUserFT
                                                            !valueFor_FundHoldingDatum_ControlWithout_Tokens_And_FT_for_Commissions = valueOf_TokensForWithdraw_Plus_FundHolding_Value <> negate valueFor_FT_CommissionsToGetBack
                                                            ------------------
                                                            !fundHoldingDatum_Control_With_Withdraw = FundHelpers.mkUpdated_FundHolding_Datum_With_Withdraw fundHoldingDatum_In withdraw commissionsForUserFT commissions_FT_Release_PerMonth_1e6_calculated
                                                            ------------------
                                                            isEnough_FT_ForComission :: Bool
                                                            !isEnough_FT_ForComission = T.hdSubTotal_FT_Commissions fundHoldingDatum_In >= commissionsForUserFT
                                                            ------------------
                                                            isEnough_Commissions_Release_PerMonth :: Bool
                                                            !isEnough_Commissions_Release_PerMonth = T.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 fundHoldingDatum_In >= commissions_FT_Release_PerMonth_1e6_calculated
                                                            ------------------
                                                            isBurningFT :: Bool
                                                            !isBurningFT = OnChainHelpers.isOnlyToken_Burning_With_AC_AndAmt fundFT_AC (negate withdrawPlusComissions) info
                                                    ------------------
                                                    _ -> False
                                                where
                                                    ------------------
                                                    !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
                                                    ------------------
                                                    !inputRef_TxOut_And_InvestUnitDatum =
                                                        case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                                            @InvestUnitT.ValidatorDatum
                                                            @InvestUnitT.InvestUnitDatumType
                                                            ctx
                                                            inputsRef_TxOuts
                                                            investUnitID_AC
                                                            InvestUnitT.getInvestUnit_DatumType of
                                                            [x] -> x
                                                            _ -> traceError "Expected exactly one InvestUnit input ref"
                                                    ------------------
                                                    !investUnitDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_InvestUnitDatum
                                                    ------------------
                                                    !investUnit = InvestUnitT.iudInvestUnit investUnitDatum_In
                                                    !investUnitTokens = T.iuValues investUnit
                                                    ------------------
                                                    !investUnit_Granularity = FundHelpers.getDecimalsInInvestUnit investUnitTokens
                                            ------------------
                                            validateReIdx :: Bool
                                            validateReIdx = case redeemer of
                                                (T.ValidatorRedeemerReIndexing (T.ValidatorRedeemerReIndexingType (T.InvestUnit !tokensToAdd) (T.InvestUnit !tokensToRemove))) ->
                                                    ------------------
                                                    -- it runs along with Invest Unit Validator (ValidatorRedeemerReIndexing)
                                                    ------------------
                                                    traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                        && traceIfFalse "not isCorrect_ReIdx_Amounts_And_Valid_Divisibility " isCorrect_ReIdx_Amounts_And_Valid_Divisibility
                                                        && traceIfFalse "wrong output datum" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_NotChanged)
                                                        && traceIfFalse "wrong output value" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out valueFor_FundHoldingDatum_Control_WithTokensExchanged)
                                                        && traceIfFalse "not isCorrect_Redeemer_InvestUnit" isCorrect_Redeemer_InvestUnit
                                                    where
                                                        ------------------
                                                        !inputs_TxOutRefs_TxOuts =
                                                            [ (LedgerApiV2.txInInfoOutRef txInfoInput, LedgerApiV2.txInInfoResolved txInfoInput) | !txInfoInput <- LedgerApiV2.txInfoInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
                                                            ]
                                                        ------------------
                                                        !inputsRef_TxOuts_And_FundHoldingDatums' =
                                                            OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_CS
                                                                @T.ValidatorDatum
                                                                @T.FundHoldingDatumType
                                                                ctx
                                                                inputsRef_TxOuts
                                                                fundHoldingPolicyID_CS
                                                                T.getFundHolding_DatumType
                                                        ------------------
                                                        !fundHoldingsCount = FundT.fdHoldingsCount fundDatum_In
                                                        ------------------
                                                        !allFundHoldingsIndex = T.hdFundHolding_Index fundHoldingDatum_In : [T.hdFundHolding_Index dat | (_, !dat) <- inputsRef_TxOuts_And_FundHoldingDatums']
                                                        !countDistinctFundHoldingsIndex = OnChainHelpers.countDistinct allFundHoldingsIndex
                                                        ------------------
                                                        !inputsRef_TxOuts_And_FundHoldingDatums =
                                                            if countDistinctFundHoldingsIndex == fundHoldingsCount
                                                                then inputsRef_TxOuts_And_FundHoldingDatums'
                                                                else traceError "expected all but one FundHolding as input ref"
                                                        ------------------
                                                        !total_Deposits_IU = T.hdSubTotal_FT_Minted fundHoldingDatum_In + sum (T.hdSubTotal_FT_Minted . OnChainHelpers.getDatum_In_TxOut_And_Datum <$> inputsRef_TxOuts_And_FundHoldingDatums)
                                                        ------------------
                                                        -- amount is multiplied by 100 to have more accuracy, but must be divided by 100 to use it
                                                        !valueOf_TotalTokensToAdd = OnChainHelpers.flattenValueToValue [(cs, tn, (am * total_Deposits_IU) `divide` 100) | (!cs, !tn, !am) <- tokensToAdd]
                                                        !valueOf_TotalTokensToRemove = OnChainHelpers.flattenValueToValue [(cs, tn, (am * total_Deposits_IU) `divide` 100) | (!cs, !tn, !am) <- tokensToRemove]
                                                        ------------------
                                                        !valueFor_FundHoldingDatum_Control_WithTokensExchanged = valueOf_FundHoldingDatum_In <> valueOf_TotalTokensToAdd <> negate valueOf_TotalTokensToRemove
                                                        ------------------
                                                        !fundHoldingDatum_Control_NotChanged = fundHoldingDatum_In
                                                        ------------------
                                                        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
                                                        ------------------
                                                        !input_TxOutRef_TxOut_And_InvestUnitDatum =
                                                            case OnChainHelpers.getTxOutRefs_TxOuts_And_DatumTypes_From_TxOutRefs_TxOuts_By_AC
                                                                @InvestUnitT.ValidatorDatum
                                                                @InvestUnitT.InvestUnitDatumType
                                                                ctx
                                                                inputs_TxOutRefs_TxOuts
                                                                investUnitID_AC
                                                                InvestUnitT.getInvestUnit_DatumType of
                                                                [x] -> x
                                                                _ -> traceError "Expected exactly one InvestUnit input"
                                                        ------------------
                                                        isCorrect_ReIdx_Amounts_And_Valid_Divisibility :: Bool
                                                        isCorrect_ReIdx_Amounts_And_Valid_Divisibility =
                                                            all (\(_, _, !am) -> am > 0 && (am * total_Deposits_IU) `remainder` 100 == 0) tokensToAdd
                                                                && all (\(_, _, !am) -> am > 0 && (am * total_Deposits_IU) `remainder` 100 == 0) tokensToRemove
                                                        ------------------
                                                        isCorrect_Redeemer_InvestUnit :: Bool
                                                        !isCorrect_Redeemer_InvestUnit =
                                                            let
                                                                !redeemerFor_InvestUnitDatum' = OnChainHelpers.getRedeemerForConsumeInput ((\(!txOutRef, _, _) -> txOutRef) input_TxOutRef_TxOut_And_InvestUnitDatum) info
                                                            in
                                                                case redeemerFor_InvestUnitDatum' of
                                                                    Nothing -> False
                                                                    Just !redeemerFor_InvestUnitDatum ->
                                                                        case LedgerApiV2.fromBuiltinData @InvestUnitT.ValidatorRedeemer $ LedgerApiV2.getRedeemer redeemerFor_InvestUnitDatum of
                                                                            Just (InvestUnitT.ValidatorRedeemerReIndexing (InvestUnitT.ValidatorRedeemerReIndexingType !riuriTokensToAdd !riuriTokensToRemove _ _)) ->
                                                                                T.InvestUnit tokensToAdd == riuriTokensToAdd && T.InvestUnit tokensToRemove == riuriTokensToRemove
                                                                            _ -> False
                                                ------------------
                                                _ -> False
                                            ------------------
                                            validateCommissionsAndUpdateMinADA :: Bool
                                            validateCommissionsAndUpdateMinADA
                                                ------------------
                                                | redeemerType == redeemerCommissions = validateCommissions
                                                | otherwise = validateUpdateMinADA
                                                where
                                                    ------------------
                                                    validateUpdateMinADA :: Bool
                                                    validateUpdateMinADA =
                                                        ---------------------
                                                        -- it runs alone
                                                        ---------------------
                                                        -- Que sea Fund Admin
                                                        -- Que el FundHoldingDatum regrese a FundHolding Val (se hace automaticamente al buscar outputs en same address)
                                                        -- Que el FundHoldingDatum se actualiza correctamente
                                                        -- Que el FundHoldingDatum value cambie con el min ADA nuevo
                                                        -- no hay restricciones temporales
                                                        ------------------
                                                        validateFundAdminAction fundDatum_In
                                                            && traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                            && traceIfFalse "not min ADA > 0" (newMinADA > 0)
                                                            && traceIfFalse "wrong output datum" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_UpdatedMinADA)
                                                            && traceIfFalse "wrong output value" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out valueFor_FundHoldingDatum_Control)
                                                        where
                                                            ------------------
                                                            !newMinADA = T.hdMinADA fundHoldingDatum_Out
                                                            ------------------
                                                            !fundHoldingDatum_Control_With_UpdatedMinADA =
                                                                FundHelpers.mkUpdated_FundHolding_Datum_With_MinADAChanged
                                                                    fundHoldingDatum_In
                                                                    newMinADA
                                                            ------------------
                                                            !valueFor_FundHoldingDatum_Control = valueOf_FundHoldingDatum_In <> LedgerAda.lovelaceValueOf (newMinADA - T.hdMinADA fundHoldingDatum_In)
                                                    ------------------
                                                    validateCommissions :: Bool
                                                    validateCommissions =
                                                        case redeemer of
                                                            (T.ValidatorRedeemerCollect_Protocol_Commission (T.ValidatorRedeemerCollect_Protocol_CommissionType !date !withdraw)) ->
                                                                ---------------------
                                                                -- it runs alone
                                                                ---------------------
                                                                validateAdminAction admins tokenAdmin_AC
                                                                    && traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                                    && traceIfFalse "wrong date" (OnChainHelpers.isDateInRange date info)
                                                                    && traceIfFalse "not withdraw > 0" (withdraw > 0)
                                                                    && traceIfFalse "not isCommissionsAvailable" (isCommissionsAvailable (getAvailable getShare getTaken date) withdraw)
                                                                    && traceIfFalse "wrong output datum" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Collect_Protocol_Commission)
                                                                    && traceIfFalse "wrong output value" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out $ getValueFor_FundHoldingDatum_Control_Without_FT_for_Commissions withdraw)
                                                                where
                                                                    ------------------
                                                                    !getShare = ProtocolT.pdShare_InBPx1e2_Protocol
                                                                    !getTaken = T.hdSubTotal_FT_Commissions_Collected_Protocol
                                                                    ------------------
                                                                    !fundHoldingDatum_Control_With_Collect_Protocol_Commission = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission fundHoldingDatum_In withdraw
                                                                    ------------------
                                                                    !admins = T.getAdmins protocolDatum_In
                                                                    !tokenAdminPolicy_CS = T.getAdminToken_CS protocolDatum_In
                                                                    !tokenAdmin_AC = LedgerValue.AssetClass (tokenAdminPolicy_CS, T.protocolTokenAdmin_TN)
                                                            ------------------
                                                            (T.ValidatorRedeemerCollect_Managers_Commission (T.ValidatorRedeemerCollect_Managers_CommissionType !date !withdraw)) ->
                                                                ---------------------
                                                                -- it runs alone
                                                                ---------------------
                                                                validateFundAdminAction fundDatum_In
                                                                    && traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                                    && traceIfFalse "wrong date" (OnChainHelpers.isDateInRange date info)
                                                                    && traceIfFalse "not withdraw > 0" (withdraw > 0)
                                                                    && traceIfFalse "not isCommissionsAvailable" (isCommissionsAvailable (getAvailable getShare getTaken date) withdraw)
                                                                    && traceIfFalse "wrong output datum" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Collect_Managers_Commission)
                                                                    && traceIfFalse "wrong output value" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out $ getValueFor_FundHoldingDatum_Control_Without_FT_for_Commissions withdraw)
                                                                where
                                                                    ------------------
                                                                    !getShare = ProtocolT.pdShare_InBPx1e2_Managers
                                                                    !getTaken = T.hdSubTotal_FT_Commissions_Collected_Managers
                                                                    ------------------
                                                                    !fundHoldingDatum_Control_With_Collect_Managers_Commission = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission fundHoldingDatum_In withdraw
                                                            ------------------
                                                            (T.ValidatorRedeemerCollect_Delegators_Commission (T.ValidatorRedeemerCollect_Delegators_CommissionType !date !withdraw)) ->
                                                                ---------------------
                                                                -- it runs alone
                                                                ---------------------
                                                                traceIfFalse "not isSignedByAny delegatorsAdmins" (OnChainHelpers.isSignedByAny delegatorsAdmins info)
                                                                    && traceIfFalse "Expected exactly one FundHolding input" (length inputs_Own_TxOuts == 1)
                                                                    && traceIfFalse "wrong date" (OnChainHelpers.isDateInRange date info)
                                                                    && traceIfFalse "not withdraw > 0" (withdraw > 0)
                                                                    && traceIfFalse "not isCommissionsAvailable" (isCommissionsAvailable (getAvailable getShare getTaken date) withdraw)
                                                                    && traceIfFalse "wrong output datum" (isCorrect_Output_FundHolding_Datum fundHoldingDatum_Out fundHoldingDatum_Control_With_Collect_Delegators_Commission)
                                                                    && traceIfFalse "wrong output value" (isCorrect_Output_FundHolding_Value valueOf_FundHoldingDatum_Out $ getValueFor_FundHoldingDatum_Control_Without_FT_for_Commissions withdraw)
                                                                where
                                                                    ------------------
                                                                    !getShare = ProtocolT.pdShare_InBPx1e2_Delegators
                                                                    !getTaken = T.hdSubTotal_FT_Commissions_Collected_Delegators
                                                                    ------------------
                                                                    !fundHoldingDatum_Control_With_Collect_Delegators_Commission = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission fundHoldingDatum_In withdraw
                                                                    ------------------
                                                                    !delegatorsAdmins = ProtocolT.pdDelegatorsAdmins protocolDatum_In
                                                            ------------------
                                                            _ -> False
                                                        where
                                                            ------------------
                                                            !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                                                            ------------------
                                                            !inputRef_TxOut_And_ProtocolDatum =
                                                                case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                                                    @ProtocolT.ValidatorDatum
                                                                    @ProtocolT.ProtocolDatumType
                                                                    ctx
                                                                    inputsRef_TxOuts
                                                                    protocolID_AC
                                                                    ProtocolT.getProtocol_DatumType of
                                                                    [x] -> x
                                                                    _ -> traceError "Expected exactly one Protocol input ref"
                                                            ------------------
                                                            !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                                                            ------------------
                                                            -- si el fondo esta cerrado a la fuerza, las comisiones se calculan con la fecha de cierre
                                                            !deadline = case FundT.fdClosedAt fundDatum_In of
                                                                Just x -> x
                                                                Nothing -> FundT.fdDeadline fundDatum_In
                                                            ------------------
                                                            getAvailable !getShare !getTaken !date =
                                                                FundHelpers.getCommissionsAvailable deadline fundHoldingDatum_In share taken date
                                                                where
                                                                    !share = getShare protocolDatum_In
                                                                    !taken = getTaken fundHoldingDatum_In
                                                            ------------------
                                                            getValueFor_FT_Commissions ! withdraw = LedgerValue.assetClassValue fundFT_AC withdraw
                                                            getValueFor_FundHoldingDatum_Control_Without_FT_for_Commissions !withdraw = valueOf_FundHoldingDatum_In <> negate (getValueFor_FT_Commissions withdraw)
                                                            ------------------
                                                            isCommissionsAvailable :: Integer -> Integer -> Bool
                                                            isCommissionsAvailable !available !withdraw = available >= withdraw

----------------------------------------------------------------------------2

{-# INLINEABLE policyID #-}
policyID :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policyID params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyID||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID fundPolicy_CS = mkPolicyID params
    where
        params =
            T.PolicyParams
                { ppFundPolicy_CS = PlutusTx.unsafeFromBuiltinData fundPolicy_CS
                }

policyIDCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
policyIDCode = $$(PlutusTx.compile [||mkWrappedPolicyID||])

----------------------------------------------------------------------------2

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS fundPolicy_CS tokenEmergencyAdminPolicy_CS = mkValidator params
    where
        params =
            T.ValidatorParams
                { vpProtocolPolicyID_CS = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
                , vpFundPolicy_CS = PlutusTx.unsafeFromBuiltinData fundPolicy_CS
                , vpTokenEmergencyAdminPolicy_CS = PlutusTx.unsafeFromBuiltinData tokenEmergencyAdminPolicy_CS
                }

validatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(PlutusTx.compile [||mkWrappedValidator||])

----------------------------------------------------------------------------2
