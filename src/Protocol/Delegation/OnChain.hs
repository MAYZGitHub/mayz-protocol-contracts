{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.Delegation.OnChain where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Ledger
import qualified Ledger.Ada as LedgerAda
import qualified Ledger.Value as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts as LedgerContextsV2
import qualified PlutusTx
import PlutusTx.Prelude

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.Constants as T
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Constants as T
import qualified Protocol.Delegation.Types as T
import qualified Protocol.Protocol.Types as ProtocolT

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- Any change in the logic, datum or redeemer must change the version of the delegationVersion on the Protocol.Delegation.Types

--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID (T.PolicyParams !protocolPolicyID_CS !delegation_Validator_Hash) !redRaw !ctxRaw =
    let
        ------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !delegation_Validator_Address = Ledger.scriptHashAddress delegation_Validator_Hash
        ------------------
        !delegationPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !delegationID_AC = LedgerValue.AssetClass (delegationPolicyID_CS, T.delegationID_TN)
        ------------------
        !valueFor_Mint_Delegation_ID = LedgerValue.assetClassValue delegationID_AC 1
    in
        ------------------
        if traceIfFalse "" useThisToMakeScriptUnique
            && case redeemer of
                T.PolicyRedeemerMintID _ ->
                    ---------------------
                    -- it runs alone
                    ------------------
                    -- que se mintee ID de Delegation, con esta poliza, 1 unidad, con nombre de token que venga en datum del protocolo
                    -- que vaya a la direccion del contrato correcta. La direccion puede estar en el datum del protocolo
                    -- que tenga el value correcto, con ID, con MAYZ delegados y con min ADA, segun Datum. De esta forma tambien se valida el Datum un poco
                    ------------------
                    traceIfFalse "not isMintingDelegationID" isMintingDelegationID
                        && traceIfFalse "deposit amount must be greater than zero" (delegated_MAYZ > 0)
                        && traceIfFalse "not isCorrect_Output_Delegation_Datum" isCorrect_Output_Delegation_Datum
                        && traceIfFalse "not isCorrect_Output_Delegation_Value" isCorrect_Output_Delegation_Value
                        && traceIfFalse "expected zero Delegation inputs" (null inputs_Own_TxOuts)
                    where
                        ---------------------
                        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                        ------------------
                        !inputsRef_TxOuts =
                            [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
                            ]
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
                        !tokenMAYZ_AC = ProtocolT.pdTokenMAYZ_AC protocolDatum_In
                        ------------------
                        !inputs_Own_TxOuts =
                            [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info, let
                                                                                                                            address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                                        in
                                                                                                                            OnChainHelpers.isScriptAddress address && address == delegation_Validator_Address
                            ]
                        ------------------
                        !outputs_Own_TxOuts =
                            [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, let
                                                                                    address = LedgerApiV2.txOutAddress txOut
                                                                                in
                                                                                    OnChainHelpers.isScriptAddress address && address == delegation_Validator_Address
                            ]
                        ------------------
                        !output_Own_TxOut_And_Delegation_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                            @T.ValidatorDatum
                            @T.Delegation_DatumType
                            ctx
                            outputs_Own_TxOuts
                            delegationID_AC
                            T.getDelegation_DatumType of
                            [x] -> x
                            _ -> traceError "Expected exactly one Delegation output"
                        ------------------
                        !delegation_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_Delegation_Datum
                        ---------------------
                        !delegated_MAYZ = T.ddDelegated_MAYZ delegation_Datum_Out
                        ---------------------
                        isMintingDelegationID :: Bool
                        isMintingDelegationID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_Delegation_ID
                        ------------------
                        isCorrect_Output_Delegation_Datum :: Bool
                        isCorrect_Output_Delegation_Datum =
                            let
                                !delegation_Datum_Out_Control =
                                    T.mkDelegation_DatumType
                                        delegationPolicyID_CS
                                        (T.ddFundPolicy_CS delegation_Datum_Out)
                                        (T.ddDelegatorPaymentPKH delegation_Datum_Out)
                                        (T.ddDelegatorStakePKH delegation_Datum_Out)
                                        tokenMAYZ_AC
                                        delegated_MAYZ
                                        (T.ddMinADA delegation_Datum_Out)
                            in
                                delegation_Datum_Out `OnChainHelpers.isUnsafeEqDatums` delegation_Datum_Out_Control
                        ------------------
                        isCorrect_Output_Delegation_Value :: Bool
                        isCorrect_Output_Delegation_Value =
                            let
                                !valueFor_Delegation_Datum' = valueFor_Mint_Delegation_ID
                                ---------------------
                                !minADA_For_Delegation_Datum = T.ddMinADA delegation_Datum_Out
                                !value_MinADA_For_Delegation_Datum = LedgerAda.lovelaceValueOf minADA_For_Delegation_Datum
                                ---------------------
                                !valueOf_Delegated_MAYZ = LedgerValue.assetClassValue tokenMAYZ_AC delegated_MAYZ
                                ---------------------
                                !valueFor_Delegation_Datum_Out_Control = valueFor_Delegation_Datum' <> value_MinADA_For_Delegation_Datum <> valueOf_Delegated_MAYZ
                                ---------------------
                                !valueOf_Delegation_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_Delegation_Datum
                            in
                                valueOf_Delegation_Out `OnChainHelpers.isEqValue` valueFor_Delegation_Datum_Out_Control
                ------------------
                T.PolicyRedeemerBurnID _ ->
                    ---------------------
                    -- it runs along with Delegation Validator (ValidatorRedeemerDelete)
                    ---------------------
                    -- que se queme ID del Buy Order, 1 unidad. Creo que con esto es suficiente.
                    -- que se este ejecutando validador correcto. No seria necesario. Si se quema es por que sale de algun lado.
                    ---------------------
                    traceIfFalse "not isBurningDelegationID" isBurningDelegationID
                    where
                        ---------------------

                        ------------------
                        !valueFor_Burn_Delegation_ID = LedgerValue.assetClassValue delegationID_AC (negate 1)
                        ---------------------
                        isBurningDelegationID :: Bool
                        isBurningDelegationID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_Delegation_ID
            then -----------------
                ()
            else error ()

--------------------------------------------------------------------------------

{-# INLINEABLE mkValidator #-}
mkValidator :: T.ValidatorParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator (T.ValidatorParams !protocolPolicyID_CS !tokenEmergencyAdminPolicy_CS) !datumRaw !redRaw !ctxRaw =
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
                    && traceIfFalse "Expected exactly one Delegation input" (length inputs_Own_TxOuts == 1)
                    && validateRedeemerDeleteAndOthers
                    then ()
                    else error ()
                where
                    ------------------
                    !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
                    ------------------
                    !datum = LedgerApiV2.unsafeFromBuiltinData @T.ValidatorDatum datumRaw
                    ------------------
                    !input_TxOut_BeingValidated = OnChainHelpers.getUnsafe_Own_Input_TxOut ctx
                    !delegation_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                    ------------------
                    !inputs_Own_TxOuts =
                        [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info, let
                                                                                                                        address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                                    in
                                                                                                                        OnChainHelpers.isScriptAddress address && address == delegation_Validator_Address
                        ]
                    ------------------
                    !delegation_Datum_In = T.getDelegation_DatumType datum
                    ------------------
                    !delegationPolicyID_CS = T.ddDelegationPolicyID_CS delegation_Datum_In
                    !delegationID_AC = LedgerValue.AssetClass (delegationPolicyID_CS, T.delegationID_TN)
                    ------------------
                    !admin = T.ddDelegatorPaymentPKH delegation_Datum_In
                    ------------------
                    redeemerUpdateMinADA = 1
                    redeemerDeposit = 2
                    redeemerWithdraw = 3
                    redeemerDelete = 4
                    ------------------
                    redeemerType :: Integer
                    !redeemerType = case redeemer of
                        (T.ValidatorRedeemerUpdateMinADA _) -> redeemerUpdateMinADA
                        (T.ValidatorRedeemerDeposit _) -> redeemerDeposit
                        (T.ValidatorRedeemerWithdraw _) -> redeemerWithdraw
                        (T.ValidatorRedeemerDelete _) -> redeemerDelete
                        _ -> 0
                    ------------------
                    validateRedeemerDeleteAndOthers :: Bool
                    validateRedeemerDeleteAndOthers
                        | redeemerType == redeemerDelete = validateAdminAction && validateDelete
                        | otherwise = validateAllButDelete
                    ------------------
                    validateAdminAction :: Bool
                    validateAdminAction =
                        traceIfFalse "not txSignedBy admin" (LedgerContextsV2.txSignedBy info admin)
                    ------------------
                    validateDelete :: Bool
                    validateDelete = traceIfFalse "not isBurningDelegationID" isBurningDelegationID
                        where
                            ---------------------
                            -- it runs along with Delegation Policy ID (PolicyRedeemerBurnID)
                            ------------------
                            -- get back all MAYZ and delete datum utxo. Burn delegation ID. only delegator can do it
                            -- check that there is one input and zero output in this contract
                            -- check that ID is burning
                            ------------------

                            ------------------
                            isBurningDelegationID :: Bool
                            isBurningDelegationID = OnChainHelpers.isNFT_Burning_With_AC delegationID_AC info
                    ------------------
                    validateAllButDelete :: Bool
                    validateAllButDelete
                        ------------------
                        | redeemerType == redeemerDeposit = validateAdminAction && validateDeposit redeemer
                        | redeemerType == redeemerWithdraw = validateAdminAction && validateWithdraw redeemer
                        | redeemerType == redeemerUpdateMinADA = validateAdminAction && validateUpdateMinADA redeemer
                        | otherwise = False
                        where
                            ------------------
                            !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                            ------------------
                            !inputsRef_TxOuts =
                                [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
                                ]
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
                            !tokenMAYZ_AC = ProtocolT.pdTokenMAYZ_AC protocolDatum_In
                            ------------------
                            !outputs_Own_TxOuts =
                                [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, let
                                                                                        address = LedgerApiV2.txOutAddress txOut
                                                                                    in
                                                                                        OnChainHelpers.isScriptAddress address && address == delegation_Validator_Address
                                ]
                            ------------------
                            !output_Own_TxOut_And_Delegation_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                @T.ValidatorDatum
                                @T.Delegation_DatumType
                                ctx
                                outputs_Own_TxOuts
                                delegationID_AC
                                T.getDelegation_DatumType of
                                [x] -> x
                                _ -> traceError "Expected exactly one Delegation output"
                            ------------------
                            !delegation_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_Delegation_Datum
                            ------------------
                            !valueOf_Delegation_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_Delegation_Datum
                            ------------------
                            validateUpdateMinADA :: T.ValidatorRedeemer -> Bool
                            validateUpdateMinADA _ =
                                ---------------------
                                -- it runs alone
                                ------------------
                                -- change min ada value. Only admin can do it
                                -- check that there is one input and one output in this contract
                                -- check datum update with new minAda
                                -- check value changed ADA
                                ------------------
                                traceIfFalse "not min ADA > 0" (newMinADA > 0)
                                    && traceIfFalse "not isCorrect_Output_Delegation_Datum_With_MinADAChanged" isCorrect_Output_Delegation_Datum_With_MinADAChanged
                                    && traceIfFalse "not isCorrect_Output_Delegation_Value_With_MinADAChanged" isCorrect_Output_Delegation_Value_With_MinADAChanged
                                where
                                    ------------------
                                    !newMinADA = T.ddMinADA delegation_Datum_Out
                                    ------------------
                                    isCorrect_Output_Delegation_Datum_With_MinADAChanged :: Bool
                                    !isCorrect_Output_Delegation_Datum_With_MinADAChanged =
                                        let
                                            !delegation_Datum_Out_Control = mkUpdated_Delegation_Datum_With_MinADAChanged delegation_Datum_In newMinADA
                                        in
                                            delegation_Datum_Out `OnChainHelpers.isUnsafeEqDatums` delegation_Datum_Out_Control
                                    ------------------
                                    isCorrect_Output_Delegation_Value_With_MinADAChanged :: Bool
                                    !isCorrect_Output_Delegation_Value_With_MinADAChanged =
                                        let
                                            !valueFor_Delegation_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> LedgerAda.lovelaceValueOf (newMinADA - T.ddMinADA delegation_Datum_In)
                                        in
                                            valueOf_Delegation_Out `OnChainHelpers.isEqValue` valueFor_Delegation_Out_Control
                            ----------------
                            validateDeposit :: T.ValidatorRedeemer -> Bool
                            validateDeposit (T.ValidatorRedeemerDeposit (T.ValidatorRedeemerDepositType !vrdDelegated_MAYZ_Change)) =
                                ---------------------
                                -- it runs alone
                                ------------------
                                -- add some MAYZ. Only delegator can do it.
                                -- check that there is one input and one output in this contract: no estoy revisando si hay una sola entrada... hace falta ?
                                -- check datum update with deposit
                                -- check value changed with deposit
                                ------------------
                                traceIfFalse "deposit amount must be greater than zero" (vrdDelegated_MAYZ_Change > 0)
                                    && traceIfFalse "not isCorrect_Output_Delegation_Datum_With_DelegationChanged" (isCorrect_Output_Delegation_Datum_With_DelegationChanged vrdDelegated_MAYZ_Change)
                                    && traceIfFalse "not isCorrect_Output_Delegation_Value_With_DelegationChanged" (isCorrect_Output_Delegation_Value_With_DelegationChanged vrdDelegated_MAYZ_Change)
                            ------------------
                            validateDeposit _ = False
                            ------------------
                            validateWithdraw :: T.ValidatorRedeemer -> Bool
                            validateWithdraw (T.ValidatorRedeemerWithdraw (T.ValidatorRedeemerWithdrawType !vrdwDelegated_MAYZ_Change)) =
                                ---------------------
                                -- it runs alone
                                ------------------
                                -- get back some MAYZ. Only delegator can do it.
                                -- check that there is one input and one output in this contract
                                -- check datum update with withdraw ... me parece que no hay cambios que se hagan en el datum en esta tx
                                -- check value changed with withdraw
                                ------------------
                                traceIfFalse
                                    "withdraw amount must be greater than zero"
                                    (vrdwDelegated_MAYZ_Change > 0)
                                    && traceIfFalse
                                        "not withdraw <= Delegated MAYZ"
                                        (vrdwDelegated_MAYZ_Change <= T.ddDelegated_MAYZ delegation_Datum_In)
                                    && traceIfFalse "not isCorrect_Output_Delegation_Datum_With_DelegationChanged" (isCorrect_Output_Delegation_Datum_With_DelegationChanged (negate vrdwDelegated_MAYZ_Change))
                                    && traceIfFalse "not isCorrect_Output_Delegation_Value_With_DelegationChanged" (isCorrect_Output_Delegation_Value_With_DelegationChanged (negate vrdwDelegated_MAYZ_Change))
                            ------------------
                            validateWithdraw _ = False
                            ------------------
                            isCorrect_Output_Delegation_Datum_With_DelegationChanged :: Integer -> Bool
                            isCorrect_Output_Delegation_Datum_With_DelegationChanged delegated_MAYZ_Change =
                                let
                                    !delegation_Datum_Out_Control = mkUpdated_Delegation_Datum_With_Deposit delegation_Datum_In delegated_MAYZ_Change
                                in
                                    delegation_Datum_Out `OnChainHelpers.isUnsafeEqDatums` delegation_Datum_Out_Control
                            ------------------
                            isCorrect_Output_Delegation_Value_With_DelegationChanged :: Integer -> Bool
                            isCorrect_Output_Delegation_Value_With_DelegationChanged delegated_MAYZ_Change =
                                let
                                    !valueOf_Delegated_MAYZ = LedgerValue.assetClassValue tokenMAYZ_AC delegated_MAYZ_Change
                                    !valueFor_Delegation_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> valueOf_Delegated_MAYZ
                                in
                                    valueOf_Delegation_Out `OnChainHelpers.isEqValue` valueFor_Delegation_Out_Control

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Delegation_Datum_With_Deposit #-}
mkUpdated_Delegation_Datum_With_Deposit :: T.Delegation_DatumType -> Integer -> T.Delegation_DatumType
mkUpdated_Delegation_Datum_With_Deposit !delegation_Datum_In !delegated_MAYZ_Change =
    delegation_Datum_In
        { T.ddDelegated_MAYZ = T.ddDelegated_MAYZ delegation_Datum_In + delegated_MAYZ_Change
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Delegation_Datum_With_MinADAChanged #-}
mkUpdated_Delegation_Datum_With_MinADAChanged :: T.Delegation_DatumType -> Integer -> T.Delegation_DatumType
mkUpdated_Delegation_Datum_With_MinADAChanged !delegation_Datum_In !newMinADA =
    delegation_Datum_In {T.ddMinADA = newMinADA}

----------------------------------------------------------------------------

{-# INLINEABLE policyID #-}
policyID :: T.PolicyParams -> LedgerApiV2.MintingPolicy
policyID params =
    Plutonomy.optimizeUPLC $
        Plutonomy.mintingPolicyToPlutus $
            Plutonomy.mkMintingPolicyScript $
                $$(PlutusTx.compile [||mkPolicyID||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE mkWrappedPolicyID #-}
mkWrappedPolicyID :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedPolicyID protocolPolicyID_CS delegation_Validator_Hash = mkPolicyID params
    where
        params =
            T.PolicyParams
                { ppProtocolPolicyID_CS = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
                , ppDelegation_Validator_Hash = PlutusTx.unsafeFromBuiltinData delegation_Validator_Hash
                }

policyIDCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
policyIDCode = $$(PlutusTx.compile [||mkWrappedPolicyID||])

----------------------------------------------------------------------------

{-# INLINEABLE validator #-}
validator :: T.ValidatorParams -> LedgerApiV2.Validator
validator params =
    Plutonomy.optimizeUPLC $
        Plutonomy.validatorToPlutus $
            Plutonomy.mkValidatorScript $
                $$(PlutusTx.compile [||mkValidator||])
                    `PlutusTx.applyCode` PlutusTx.liftCode params

{-# INLINEABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator protocolPolicyID_CS tokenEmergencyAdminPolicy_CS = mkValidator params
    where
        params =
            T.ValidatorParams
                { vpProtocolPolicyID_CS = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
                 , vpTokenEmergencyAdminPolicy_CS = PlutusTx.unsafeFromBuiltinData tokenEmergencyAdminPolicy_CS
                }

validatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(PlutusTx.compile [||mkWrappedValidator||])

------------------------------------------------------------------------------
