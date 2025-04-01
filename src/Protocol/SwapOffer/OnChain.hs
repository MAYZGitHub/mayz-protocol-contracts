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

module Protocol.SwapOffer.OnChain where

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
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.OnChainHelpers as OnChainHelpers
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.SwapOffer.Types as T
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

-- Any change in the logic, datum or redeemer must change the version of the swapOfferVersion on Protocol.SwapOffer.Types

--------------------------------------------------------------------------------2

{-# INLINEABLE mkPolicyID #-}
mkPolicyID :: T.PolicyParams -> BuiltinData -> BuiltinData -> ()
mkPolicyID (T.PolicyParams !protocolPolicyID_CS !swapOffer_Validator_Hash) !redRaw !ctxRaw =
    let
        ------------------
        !useThisToMakeScriptUnique = protocolPolicyID_CS /= LedgerApiV2.adaSymbol
        ------------------
        !redeemer = LedgerApiV2.unsafeFromBuiltinData @T.PolicyRedeemer redRaw
        !ctx = LedgerApiV2.unsafeFromBuiltinData @LedgerContextsV2.ScriptContext ctxRaw
        !info = LedgerContextsV2.scriptContextTxInfo ctx
        ------------------
        !swapOffer_Validator_Address = Ledger.scriptHashAddress swapOffer_Validator_Hash
        ------------------
        !swapOfferPolicyID_CS = LedgerContextsV2.ownCurrencySymbol ctx
        !swapOfferID_AC = LedgerValue.AssetClass (swapOfferPolicyID_CS, T.swapOfferID_TN)
        ------------------
        !valueFor_Mint_SwapOffer_ID = LedgerValue.assetClassValue swapOfferID_AC 1
    in
        ------------------
        if traceIfFalse "" useThisToMakeScriptUnique
            && case redeemer of
                T.PolicyRedeemerMintID _ ->
                    ---------------------
                    -- it runs alone
                    ---------------------
                    traceIfFalse "not isMintingSwapOfferID" isMintingSwapOfferID
                        && traceIfFalse "not isCorrect_Output_SwapOffer_Datum" isCorrect_Output_SwapOffer_Datum
                        && traceIfFalse "not isCorrect_Output_SwapOffer_Value" isCorrect_Output_SwapOffer_Value
                        && traceIfFalse "expected zero SwapOffer inputs" (null inputs_Own_TxOuts)
                    where
                        ---------------------

                        ------------------
                        !inputsRef_TxOuts =
                            [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
                            ]
                        ------------------
                        -- !inputs_Own_TxOuts =
                        --     [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info, let
                        --                                                                                                     address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                        --                                                                                                 in
                        --                                                                                                     OnChainHelpers.isScriptAddress address && OnChainHelpers.getUnsafeScriptHash_In_Address address == swapOffer_Validator_Hash
                        --     ]
                        !inputs_Own_TxOuts =
                            [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info, let
                                                                                                                            address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                                        in
                                                                                                                            OnChainHelpers.isScriptAddress address && address == swapOffer_Validator_Address
                            ]
                        ------------------
                        -- !outputs_Own_TxOuts =
                        --     [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, let
                        --                                                             address = LedgerApiV2.txOutAddress txOut
                        --                                                         in
                        --                                                             OnChainHelpers.isScriptAddress address && OnChainHelpers.getUnsafeScriptHash_In_Address address == swapOffer_Validator_Hash
                        --     ]
                        !outputs_Own_TxOuts =
                            [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, let
                                                                                    address = LedgerApiV2.txOutAddress txOut
                                                                                in
                                                                                    OnChainHelpers.isScriptAddress address && address == swapOffer_Validator_Address
                            ]
                        ------------------
                        !output_Own_TxOut_And_SwapOffer_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                            @T.ValidatorDatum
                            @T.SwapOffer_DatumType
                            ctx
                            outputs_Own_TxOuts
                            swapOfferID_AC
                            T.getSwapOffer_DatumType of
                            [x] -> x
                            _ -> traceError "Expected exactly one SwapOffer output"
                        ------------------
                        !swapOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SwapOffer_Datum
                        ------------------
                        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
                        !fundID_AC = LedgerValue.AssetClass (T.sodFundPolicy_CS swapOffer_Datum_Out, T.fundID_TN)
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
                        !protocolDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_ProtocolDatum
                        ------------------
                        !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_FundDatum
                        ------------------
                        !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
                        !fundFT_TN_AC = LedgerValue.AssetClass (T.sodFundPolicy_CS swapOffer_Datum_Out, fundFT_TN)
                        ------------------
                        !commissionSwapOffer_InBPx1e3 = ProtocolT.pdCommissionSwapOffer_InBPx1e3 protocolDatum_In
                        ------------------
                        !tokenMAYZ_AC = ProtocolT.pdTokenMAYZ_AC protocolDatum_In
                        ------------------
                        !requiredMAYZ = ProtocolT.pdRequiredMAYZForSwapOffer protocolDatum_In
                        ---------------------
                        !valueOf_RequiredMAYZ = LedgerValue.assetClassValue tokenMAYZ_AC requiredMAYZ
                        ---------------------
                        isMintingSwapOfferID :: Bool
                        isMintingSwapOfferID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Mint_SwapOffer_ID
                        -----------------
                        isCorrect_Output_SwapOffer_Datum :: Bool
                        isCorrect_Output_SwapOffer_Datum =
                            let
                                isValidAllowOption option = option == T.swapOffer_AllowSell || option == T.swapOffer_NotAllowSell
                                isValidStatus status = status == T.swapOffer_Status_Open || status == T.swapOffer_Status_Closed
                                !swapOffer_Datum_Out_Control =
                                    T.mkSwapOffer_DatumType
                                        swapOfferPolicyID_CS
                                        (T.sodFundPolicy_CS swapOffer_Datum_Out)
                                        (T.sodSellerPaymentPKH swapOffer_Datum_Out)
                                        (T.sodSellerStakePKH swapOffer_Datum_Out)
                                        (T.sodAskedCommission_InBPx1e3 swapOffer_Datum_Out)
                                        (T.sodAmount_FT_Available swapOffer_Datum_Out)
                                        (T.sodAmount_ADA_Available swapOffer_Datum_Out)
                                        0
                                        0
                                        (T.sodOrder_AllowSellFT swapOffer_Datum_Out)
                                        (T.sodOrder_AllowSellADA swapOffer_Datum_Out)
                                        (T.sodOrder_Status swapOffer_Datum_Out)
                                        tokenMAYZ_AC
                                        requiredMAYZ
                                        (T.sodMinADA swapOffer_Datum_Out)
                            in
                                traceIfFalse "not isInRange commissionSwapOffer_InBPx1e3" (ProtocolT.isInRange commissionSwapOffer_InBPx1e3 (T.sodAskedCommission_InBPx1e3 swapOffer_Datum_Out))
                                    && traceIfFalse "not sodAmount_FT_Available >= 0" (T.sodAmount_FT_Available swapOffer_Datum_Out >= 0)
                                    && traceIfFalse "not sodAmount_ADA_Available >= 0" (T.sodAmount_ADA_Available swapOffer_Datum_Out >= 0)
                                    && traceIfFalse "not isValidAllowOption sodOrder_AllowSellFT" (isValidAllowOption $ T.sodOrder_AllowSellFT swapOffer_Datum_Out)
                                    && traceIfFalse "not isValidAllowOption sodOrder_AllowSellADA" (isValidAllowOption $ T.sodOrder_AllowSellADA swapOffer_Datum_Out)
                                    && traceIfFalse "not isValidStatus swapOffer_Status_Open" (isValidStatus $ T.sodOrder_Status swapOffer_Datum_Out)
                                    && swapOffer_Datum_Out
                                    `OnChainHelpers.isUnsafeEqDatums` swapOffer_Datum_Out_Control
                        ------------------
                        isCorrect_Output_SwapOffer_Value :: Bool
                        isCorrect_Output_SwapOffer_Value =
                            let
                                ---------------------
                                !amount_FT_Available_For_SwapOffer_Datum = T.sodAmount_FT_Available swapOffer_Datum_Out
                                !amount_ADA_Available_For_SwapOffer_Datum = T.sodAmount_ADA_Available swapOffer_Datum_Out
                                ---------------------
                                !value_Amount_FT_Available_For_SwapOffer_Datum = LedgerValue.assetClassValue fundFT_TN_AC amount_FT_Available_For_SwapOffer_Datum
                                !value_Amount_ADA_Available_For_SwapOffer_Datum = LedgerAda.lovelaceValueOf amount_ADA_Available_For_SwapOffer_Datum
                                ---------------------
                                !minADA_For_SwapOffer_Datum = T.sodMinADA swapOffer_Datum_Out
                                !value_MinADA_For_SwapOffer_Datum = LedgerAda.lovelaceValueOf minADA_For_SwapOffer_Datum
                                ---------------------
                                !valueFor_SwapOffer_Datum_Out_Control = valueFor_Mint_SwapOffer_ID <> value_MinADA_For_SwapOffer_Datum <> value_Amount_FT_Available_For_SwapOffer_Datum <> value_Amount_ADA_Available_For_SwapOffer_Datum <> valueOf_RequiredMAYZ
                                ---------------------
                                !valueOf_SwapOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SwapOffer_Datum
                                ---------------------
                                !currentMAYZ = OnChainHelpers.getAmt_With_AC_InValue valueOf_SwapOffer_Out tokenMAYZ_AC
                            in
                                ---------------------
                                traceIfFalse "not currentMAYZ == requiredMAYZ" (currentMAYZ == requiredMAYZ)
                                    && valueOf_SwapOffer_Out
                                    `OnChainHelpers.isEqValue` valueFor_SwapOffer_Datum_Out_Control
                ------------------
                T.PolicyRedeemerBurnID _ ->
                    ---------------------
                    -- it runs along with SwapOffer Validator (ValidatorRedeemerDelete)
                    ---------------------
                    traceIfFalse "not isBurningSwapOfferID" isBurningSwapOfferID
                    where
                        ---------------------

                        ------------------
                        !valueFor_Burn_SwapOffer_ID = LedgerValue.assetClassValue swapOfferID_AC (negate 1)
                        ---------------------
                        isBurningSwapOfferID :: Bool
                        isBurningSwapOfferID = OnChainHelpers.getUnsafeOwnMintingValue ctx `OnChainHelpers.isEqValue` valueFor_Burn_SwapOffer_ID
            then -----------------
                ()
            else error ()

--------------------------------------------------------------------------------2

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
                    && traceIfFalse "Expected exactly one SwapOffer input" (length inputs_Own_TxOuts == 1)
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
                    !swapOffer_Validator_Address = LedgerApiV2.txOutAddress input_TxOut_BeingValidated
                    -- !swapOffer_Validator_Hash = OnChainHelpers.getUnsafeScriptHash_In_Address swapOffer_Validator_Address
                    ------------------
                    -- !inputs_Own_TxOuts =
                    --     [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info, let
                    --                                                                                                     address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                    --                                                                                                 in
                    --                                                                                                     OnChainHelpers.isScriptAddress address && OnChainHelpers.getUnsafeScriptHash_In_Address address == swapOffer_Validator_Hash
                    --     ]
                    !inputs_Own_TxOuts =
                        [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoInputs info, let
                                                                                                                        address = LedgerApiV2.txOutAddress (LedgerApiV2.txInInfoResolved txInfoInput)
                                                                                                                    in
                                                                                                                        OnChainHelpers.isScriptAddress address && address == swapOffer_Validator_Address
                        ]
                    ------------------
                    !swapOffer_Datum_In = T.getSwapOffer_DatumType datum
                    ------------------
                    !fundPolicy_CS = T.sodFundPolicy_CS swapOffer_Datum_In
                    !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
                    ------------------
                    !swapOfferPolicyID_CS = T.sodSwapOfferPolicyID_CS swapOffer_Datum_In
                    !swapOfferID_AC = LedgerValue.AssetClass (swapOfferPolicyID_CS, T.swapOfferID_TN)
                    ------------------
                    !admin = T.sodSellerPaymentPKH swapOffer_Datum_In
                    ------------------
                    redeemerUpdateStatus = 1
                    redeemerUpdateAskedCommissionRate = 2
                    redeemerUpdateSellRestrictions = 3
                    redeemerUpdateMinADA = 4
                    redeemerDeposit = 5
                    redeemerWithdraw = 6
                    redeemerSwapFTxADA = 7
                    redeemerSwapADAxFT = 8
                    redeemerDelete = 9
                    ------------------
                    redeemerType :: Integer
                    !redeemerType = case redeemer of
                        (T.ValidatorRedeemerUpdateStatus _) -> redeemerUpdateStatus
                        (T.ValidatorRedeemerUpdateAskedCommissionRate _) -> redeemerUpdateAskedCommissionRate
                        (T.ValidatorRedeemerUpdateSellRestrictions _) -> redeemerUpdateSellRestrictions
                        (T.ValidatorRedeemerUpdateMinADA _) -> redeemerUpdateMinADA
                        (T.ValidatorRedeemerDeposit _) -> redeemerDeposit
                        (T.ValidatorRedeemerWithdraw _) -> redeemerWithdraw
                        (T.ValidatorRedeemerSwapFTxADA _) -> redeemerSwapFTxADA
                        (T.ValidatorRedeemerSwapADAxFT _) -> redeemerSwapADAxFT
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
                    validateDelete = traceIfFalse "not isBurningSwapOfferID" isBurningSwapOfferID
                        where
                            ---------------------
                            -- it runs along with SwapOffer Policy ID (PolicyRedeemerBurnID)
                            ---------------------

                            ------------------
                            isBurningSwapOfferID :: Bool
                            isBurningSwapOfferID = OnChainHelpers.isNFT_Burning_With_AC swapOfferID_AC info
                    ------------------
                    validateAllButDelete :: Bool
                    validateAllButDelete
                        ------------------
                        | redeemerType == redeemerUpdateStatus = validateAdminAction && validateUpdateStatus redeemer
                        | redeemerType == redeemerUpdateAskedCommissionRate = validateAdminAction && validateUpdateAskedCommissionRate redeemer
                        | redeemerType == redeemerUpdateSellRestrictions = validateAdminAction && validateUpdateSellRestrictions redeemer
                        | redeemerType == redeemerUpdateMinADA = validateAdminAction && validateUpdateMinADA redeemer
                        | redeemerType == redeemerDeposit = validateAdminAction && validateDeposit redeemer
                        | redeemerType == redeemerWithdraw = validateAdminAction && validateWithdraw redeemer
                        | redeemerType == redeemerSwapFTxADA || redeemerType == redeemerSwapADAxFT = validateSwaps
                        | otherwise = False
                        where
                            ------------------
                            !inputsRef_TxOuts =
                                [ LedgerApiV2.txInInfoResolved txInfoInput | !txInfoInput <- LedgerApiV2.txInfoReferenceInputs info, OnChainHelpers.isScriptAddress (LedgerApiV2.txOutAddress $ LedgerApiV2.txInInfoResolved txInfoInput)
                                ]
                            ------------------
                            -- !outputs_Own_TxOuts =
                            --     [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, let
                            --                                                             address = LedgerApiV2.txOutAddress txOut
                            --                                                         in
                            --                                                             OnChainHelpers.isScriptAddress address && OnChainHelpers.getUnsafeScriptHash_In_Address address == swapOffer_Validator_Hash
                            --     ]
                            !outputs_Own_TxOuts =
                                [ txOut | !txOut <- LedgerApiV2.txInfoOutputs info, let
                                                                                        address = LedgerApiV2.txOutAddress txOut
                                                                                    in
                                                                                        OnChainHelpers.isScriptAddress address && address == swapOffer_Validator_Address
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
                            !fundDatum_In = OnChainHelpers.getDatum_In_TxOut_And_Datum inputRef_TxOut_And_FundDatum
                            ------------------
                            !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
                            !fundFT_TN_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
                            ------------------
                            !output_Own_TxOut_And_SwapOffer_Datum = case OnChainHelpers.getTxOuts_And_DatumTypes_From_TxOuts_By_AC
                                @T.ValidatorDatum
                                @T.SwapOffer_DatumType
                                ctx
                                outputs_Own_TxOuts
                                swapOfferID_AC
                                T.getSwapOffer_DatumType of
                                [x] -> x
                                _ -> traceError "Expected exactly one SwapOffer output"
                            ------------------
                            !swapOffer_Datum_Out = OnChainHelpers.getDatum_In_TxOut_And_Datum output_Own_TxOut_And_SwapOffer_Datum
                            ------------------
                            !valueOf_SwapOffer_Out = OnChainHelpers.getValue_In_TxOut_And_Datum output_Own_TxOut_And_SwapOffer_Datum
                            ----------------
                            isOrderOpen :: Bool
                            !isOrderOpen = T.sodOrder_Status swapOffer_Datum_In == T.swapOffer_Status_Open
                            ------------------
                            isCorrect_Output_SwapOffer_Value_NotChanged :: Bool
                            !isCorrect_Output_SwapOffer_Value_NotChanged =
                                let
                                    !valueFor_SwapOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated
                                in
                                    valueOf_SwapOffer_Out `OnChainHelpers.isEqValue` valueFor_SwapOffer_Out_Control
                            ----------------
                            getLazyProtocolDatum_In :: ProtocolT.ProtocolDatumType
                            getLazyProtocolDatum_In =
                                let
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
                                in
                                    ------------------
                                    protocolDatum_In
                            ----------------
                            validateUpdateStatus :: T.ValidatorRedeemer -> Bool
                            validateUpdateStatus (T.ValidatorRedeemerUpdateStatus (T.ValidatorRedeemerUpdateStatusType !newStatus)) =
                                ------------------
                                ---- change status to open or close. Only admin can do it
                                -- check that there is one input and one output in this contract
                                -- check datum update with new status
                                -- check value not changed
                                ------------------
                                traceIfFalse "not isValidStatus swapOffer_Status_Open" (isValidStatus newStatus)
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Datum_With_StatusChanged" isCorrect_Output_SwapOffer_Datum_With_StatusChanged
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Value_NotChanged" isCorrect_Output_SwapOffer_Value_NotChanged
                                where
                                    ------------------
                                    isValidStatus status = status == T.swapOffer_Status_Open || status == T.swapOffer_Status_Closed
                                    ------------------
                                    isCorrect_Output_SwapOffer_Datum_With_StatusChanged :: Bool
                                    !isCorrect_Output_SwapOffer_Datum_With_StatusChanged =
                                        let
                                            !swapOffer_Datum_Out_Control = mkUpdated_SwapOffer_Datum_With_StatusChanged swapOffer_Datum_In newStatus
                                        in
                                            swapOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` swapOffer_Datum_Out_Control
                            ------------------
                            validateUpdateStatus _ = False
                            ------------------
                            validateUpdateAskedCommissionRate :: T.ValidatorRedeemer -> Bool
                            validateUpdateAskedCommissionRate (T.ValidatorRedeemerUpdateAskedCommissionRate (T.ValidatorRedeemerUpdateAskedCommissionRateType !newCommissionRate)) =
                                ---------------------
                                -- it runs alone
                                ---------------------
                                traceIfFalse "not isInRange commissionSwapOffer_InBPx1e3" (ProtocolT.isInRange commissionSwapOffer_InBPx1e3 newCommissionRate)
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Datum_With_CommissionChanged" isCorrect_Output_SwapOffer_Datum_With_CommissionChanged
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Value_NotChanged" isCorrect_Output_SwapOffer_Value_NotChanged
                                where
                                    ------------------
                                    !commissionSwapOffer_InBPx1e3 = ProtocolT.pdCommissionSwapOffer_InBPx1e3 getLazyProtocolDatum_In
                                    ---------------------
                                    isCorrect_Output_SwapOffer_Datum_With_CommissionChanged :: Bool
                                    !isCorrect_Output_SwapOffer_Datum_With_CommissionChanged =
                                        let
                                            !swapOffer_Datum_Out_Control = mkUpdated_SwapOffer_Datum_With_CommissionChanged swapOffer_Datum_In newCommissionRate
                                        in
                                            swapOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` swapOffer_Datum_Out_Control
                            ------------------
                            validateUpdateAskedCommissionRate _ = False
                            ------------------
                            validateUpdateSellRestrictions :: T.ValidatorRedeemer -> Bool
                            validateUpdateSellRestrictions (T.ValidatorRedeemerUpdateSellRestrictions (T.ValidatorRedeemerUpdateSellRestrictionsType !rusrAllowSellFT !rusrAllowSellADA)) =
                                ---------------------
                                -- it runs alone
                                ---------------------
                                traceIfFalse "not isValidAllowOption sodOrder_AllowSellFT" (isValidAllowOption rusrAllowSellFT)
                                    && traceIfFalse "not isValidAllowOption sodOrder_AllowSellADA" (isValidAllowOption rusrAllowSellADA)
                                    && traceIfFalse
                                        "not isCorrect_Output_SwapOffer_Datum_With_RestrictionsChanged"
                                        isCorrect_Output_SwapOffer_Datum_With_RestrictionsChanged
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Value_NotChanged" isCorrect_Output_SwapOffer_Value_NotChanged
                                where
                                    ------------------
                                    isValidAllowOption option = option == T.swapOffer_AllowSell || option == T.swapOffer_NotAllowSell
                                    ------------------
                                    isCorrect_Output_SwapOffer_Datum_With_RestrictionsChanged :: Bool
                                    !isCorrect_Output_SwapOffer_Datum_With_RestrictionsChanged =
                                        let
                                            !swapOffer_Datum_Out_Control = mkUpdated_SwapOffer_Datum_With_RestrictionsChanged swapOffer_Datum_In rusrAllowSellFT rusrAllowSellADA
                                        in
                                            swapOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` swapOffer_Datum_Out_Control
                            ------------------
                            validateUpdateSellRestrictions _ = False
                            ------------------
                            validateUpdateMinADA :: T.ValidatorRedeemer -> Bool
                            validateUpdateMinADA _ =
                                ---------------------
                                -- it runs alone
                                ---------------------
                                traceIfFalse "not min ADA > 0" (newMinADA > 0)
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Datum_With_MinADAChanged" isCorrect_Output_SwapOffer_Datum_With_MinADAChanged
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Value_With_MinADAChanged" isCorrect_Output_SwapOffer_Value_With_MinADAChanged
                                where
                                    ------------------
                                    !newMinADA = T.sodMinADA swapOffer_Datum_Out
                                    ------------------
                                    isCorrect_Output_SwapOffer_Datum_With_MinADAChanged :: Bool
                                    !isCorrect_Output_SwapOffer_Datum_With_MinADAChanged =
                                        let
                                            !swapOffer_Datum_Out_Control = mkUpdated_SwapOffer_Datum_With_MinADAChanged swapOffer_Datum_In newMinADA
                                        in
                                            swapOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` swapOffer_Datum_Out_Control
                                    ------------------
                                    isCorrect_Output_SwapOffer_Value_With_MinADAChanged :: Bool
                                    !isCorrect_Output_SwapOffer_Value_With_MinADAChanged =
                                        let
                                            !valueFor_SwapOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> LedgerAda.lovelaceValueOf (newMinADA - T.sodMinADA swapOffer_Datum_In)
                                        in
                                            valueOf_SwapOffer_Out `OnChainHelpers.isEqValue` valueFor_SwapOffer_Out_Control
                            ----------------
                            validateDeposit :: T.ValidatorRedeemer -> Bool
                            validateDeposit (T.ValidatorRedeemerDeposit (T.ValidatorRedeemerDepositType !newDeposit_FT !newDeposit_ADA)) =
                                ---------------------
                                -- it runs alone
                                ---------------------
                                traceIfFalse "deposits amounts must be greater than zero" (newDeposit_FT >= 0 && newDeposit_ADA >= 0)
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Datum_With_Deposit" isCorrect_Output_SwapOffer_Datum_With_Deposit
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Value_With_Deposit" isCorrect_Output_SwapOffer_Value_With_Deposit
                                where
                                    ------------------
                                    isCorrect_Output_SwapOffer_Datum_With_Deposit :: Bool
                                    !isCorrect_Output_SwapOffer_Datum_With_Deposit =
                                        let
                                            !swapOffer_Datum_Out_Control = mkUpdated_SwapOffer_Datum_With_Deposit swapOffer_Datum_In newDeposit_FT newDeposit_ADA
                                        in
                                            swapOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` swapOffer_Datum_Out_Control
                                    ------------------
                                    isCorrect_Output_SwapOffer_Value_With_Deposit :: Bool
                                    !isCorrect_Output_SwapOffer_Value_With_Deposit =
                                        let
                                            !value_Deposit_FT = LedgerValue.assetClassValue fundFT_TN_AC newDeposit_FT
                                            !value_Deposit_ADA = LedgerAda.lovelaceValueOf newDeposit_ADA
                                            ------------------
                                            !valueFor_SwapOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> value_Deposit_FT <> value_Deposit_ADA
                                        in
                                            valueOf_SwapOffer_Out `OnChainHelpers.isEqValue` valueFor_SwapOffer_Out_Control
                            ----------------
                            validateDeposit _ = False
                            ------------------
                            validateWithdraw :: T.ValidatorRedeemer -> Bool
                            validateWithdraw (T.ValidatorRedeemerWithdraw (T.ValidatorRedeemerWithdrawType !newWithdraw_FT !newWithdraw_ADA)) =
                                ---------------------
                                -- it runs alone
                                ---------------------
                                traceIfFalse "withdraws amounts must be greater than zero" (newWithdraw_FT >= 0 && newWithdraw_ADA >= 0)
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Datum_With_Withdraw" isCorrect_Output_SwapOffer_Datum_With_Withdraw
                                    && traceIfFalse "not isCorrect_Output_SwapOffer_Value_With_Withdraw" isCorrect_Output_SwapOffer_Value_With_Withdraw
                                where
                                    ------------------
                                    isCorrect_Output_SwapOffer_Datum_With_Withdraw :: Bool
                                    !isCorrect_Output_SwapOffer_Datum_With_Withdraw =
                                        let
                                            !swapOffer_Datum_Out_Control = mkUpdated_SwapOffer_Datum_With_Withdraw swapOffer_Datum_In newWithdraw_FT newWithdraw_ADA
                                        in
                                            swapOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` swapOffer_Datum_Out_Control
                                    ------------------
                                    isCorrect_Output_SwapOffer_Value_With_Withdraw :: Bool
                                    !isCorrect_Output_SwapOffer_Value_With_Withdraw =
                                        let
                                            !value_Withdraw_FT = LedgerValue.assetClassValue fundFT_TN_AC newWithdraw_FT
                                            !value_Withdraw_ADA = LedgerAda.lovelaceValueOf newWithdraw_ADA
                                            ------------------
                                            !valueFor_SwapOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> negate value_Withdraw_FT <> negate value_Withdraw_ADA
                                        in
                                            valueOf_SwapOffer_Out `OnChainHelpers.isEqValue` valueFor_SwapOffer_Out_Control
                            ----------------
                            validateWithdraw _ = False
                            ------------------
                            validateSwaps :: Bool
                            validateSwaps
                                ------------------
                                | redeemerType == redeemerSwapFTxADA = validateSwapFTxADA redeemer
                                | redeemerType == redeemerSwapADAxFT = validateSwapADAxFT redeemer
                                | otherwise = False
                                where
                                    ------------------
                                    validateSwapFTxADA :: T.ValidatorRedeemer -> Bool
                                    validateSwapFTxADA (T.ValidatorRedeemerSwapFTxADA (T.ValidatorRedeemerSwapFTxADAType !rsfxaAmount_FT !rsfxaAmount_ADA !rsfxaCommission_ADA !rsfxaOracle_Data !rsfxaOracle_Signature)) =
                                        ---------------------
                                        -- it runs alone
                                        ---------------------
                                        -- if order is open, user give FT and get ADA. Use a price for conversion provided by oracle. Must check signatura and validity time
                                        -- check that there is one input and one output in this contract
                                        -- check datum update with swap. Totals calculated
                                        -- check commissions
                                        -- check value changed FT and ADA
                                        -- check price, validity time and signature
                                        ------------------
                                        traceIfFalse "not isOrderOpen" isOrderOpen
                                            && traceIfFalse "isOrderRestrictedForSellingADA" (not isOrderRestrictedForSellingADA)
                                            && traceIfFalse "swap amounts must be greater than zero" (rsfxaAmount_FT > 0 && rsfxaAmount_ADA > 0 && rsfxaCommission_ADA >= 0)
                                            && traceIfFalse "not isCorrect_Oracle_Signature" (OnChainHelpers.isCorrect_Oracle_Signature priceData oraclePaymentPubKey rsfxaOracle_Signature)
                                            && traceIfFalse "not isCorrect_Oracle_InRangeTime" (OnChainHelpers.isCorrect_Oracle_InRangeTime info (T.odTime rsfxaOracle_Data) (ProtocolT.pdOracleData_Valid_Time protocolDatum_In))
                                            && traceIfFalse "not isCorrect_Conversion" (isCorrect_Conversion rsfxaOracle_Data rsfxaAmount_FT rsfxaAmount_ADA)
                                            && traceIfFalse "not isCorrect_Commission" (isCorrect_Commission rsfxaAmount_ADA rsfxaCommission_ADA)
                                            && traceIfFalse "not isAmount_ADA_Available" (isAmount_ADA_Available (rsfxaAmount_ADA - rsfxaCommission_ADA))
                                            && traceIfFalse "not isCorrect_Output_SwapOffer_Datum_With_SwapFTxADA" (isCorrect_Output_SwapOffer_Datum_With_SwapFTxADA rsfxaAmount_FT rsfxaAmount_ADA rsfxaCommission_ADA)
                                            && traceIfFalse "not isCorrect_Output_SwapOffer_Value_With_SwapFTxADA" (isCorrect_Output_SwapOffer_Value_With_SwapFTxADA rsfxaAmount_FT rsfxaAmount_ADA rsfxaCommission_ADA)
                                        where
                                            ------------------
                                            !priceData = OnChainHelpers.oracleDataToBBS rsfxaOracle_Data
                                            ------------------
                                            isOrderRestrictedForSellingADA :: Bool
                                            !isOrderRestrictedForSellingADA = T.sodOrder_AllowSellADA swapOffer_Datum_In == T.swapOffer_NotAllowSell
                                            ------------------
                                            isCorrect_Output_SwapOffer_Datum_With_SwapFTxADA :: Integer -> Integer -> Integer -> Bool
                                            isCorrect_Output_SwapOffer_Datum_With_SwapFTxADA !amount_FT !amount_ADA !commission_ADA =
                                                let
                                                    !swapOffer_Datum_Out_Control = mkUpdated_SwapOffer_Datum_With_SwapFTxADA swapOffer_Datum_In amount_FT amount_ADA commission_ADA
                                                in
                                                    swapOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` swapOffer_Datum_Out_Control
                                            ------------------
                                            isCorrect_Output_SwapOffer_Value_With_SwapFTxADA :: Integer -> Integer -> Integer -> Bool
                                            isCorrect_Output_SwapOffer_Value_With_SwapFTxADA !amount_FT !amount_ADA !commission_ADA =
                                                let
                                                    !value_Amount_FT = LedgerValue.assetClassValue fundFT_TN_AC amount_FT
                                                    !value_Amount_ADA = LedgerAda.lovelaceValueOf (amount_ADA - commission_ADA)
                                                    ------------------
                                                    !valueFor_SwapOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> value_Amount_FT <> negate value_Amount_ADA
                                                in
                                                    valueOf_SwapOffer_Out `OnChainHelpers.isEqValue` valueFor_SwapOffer_Out_Control
                                    ----------------
                                    validateSwapFTxADA _ = False
                                    ------------------
                                    validateSwapADAxFT :: T.ValidatorRedeemer -> Bool
                                    validateSwapADAxFT (T.ValidatorRedeemerSwapADAxFT (T.ValidatorRedeemerSwapADAxFTType !rsaxfAmount_ADA !rsaxfAmount_FT !rsaxfCommission_FT !rsaxfOracle_Data !rsaxfOracle_Signature)) =
                                        ---------------------
                                        -- it runs alone
                                        ---------------------
                                        -- if order is open, user give ADA and get FT. Use a price for conversion provided by oracle. Must check signatura and validity time
                                        -- check that there is one input and one output in this contract
                                        -- check datum update with swap. Totals calculated
                                        -- check commissions
                                        -- check value changed FT and ADA
                                        -- check price, validity time and signature
                                        ------------------
                                        traceIfFalse "not isOrderOpen" isOrderOpen
                                            && traceIfFalse "isOrderRestrictedForSellingFT" (not isOrderRestrictedForSellingFT)
                                            && traceIfFalse "swap amounts must be greater than zero" (rsaxfAmount_ADA > 0 && rsaxfAmount_FT > 0 && rsaxfCommission_FT >= 0)
                                            && traceIfFalse "not isCorrect_Oracle_Signature" (OnChainHelpers.isCorrect_Oracle_Signature priceData oraclePaymentPubKey rsaxfOracle_Signature)
                                            && traceIfFalse "not isCorrect_Oracle_InRangeTime" (OnChainHelpers.isCorrect_Oracle_InRangeTime info (T.odTime rsaxfOracle_Data) (ProtocolT.pdOracleData_Valid_Time protocolDatum_In))
                                            && traceIfFalse "not isCorrect_Conversion" (isCorrect_Conversion rsaxfOracle_Data rsaxfAmount_FT rsaxfAmount_ADA)
                                            && traceIfFalse "not isCorrect_Commission" (isCorrect_Commission rsaxfAmount_FT rsaxfCommission_FT)
                                            && traceIfFalse "not isAmount_FT_Available" (isAmount_FT_Available (rsaxfAmount_FT - rsaxfCommission_FT))
                                            && traceIfFalse "not isCorrect_Output_SwapOffer_Datum_With_SwapADAxFT" (isCorrect_Output_SwapOffer_Datum_With_SwapADAxFT rsaxfAmount_ADA rsaxfAmount_FT rsaxfCommission_FT)
                                            && traceIfFalse "not isCorrect_Output_SwapOffer_Value_With_SwapADAxFT" (isCorrect_Output_SwapOffer_Value_With_SwapADAxFT rsaxfAmount_ADA rsaxfAmount_FT rsaxfCommission_FT)
                                        where
                                            ------------------
                                            !priceData = OnChainHelpers.oracleDataToBBS rsaxfOracle_Data
                                            ------------------
                                            isOrderRestrictedForSellingFT :: Bool
                                            !isOrderRestrictedForSellingFT = T.sodOrder_AllowSellFT swapOffer_Datum_In == T.swapOffer_NotAllowSell
                                            ------------------
                                            isCorrect_Output_SwapOffer_Datum_With_SwapADAxFT :: Integer -> Integer -> Integer -> Bool
                                            isCorrect_Output_SwapOffer_Datum_With_SwapADAxFT !amount_ADA !amount_FT !commission_FT =
                                                let
                                                    !swapOffer_Datum_Out_Control = mkUpdated_SwapOffer_Datum_With_SwapADAxFT swapOffer_Datum_In amount_ADA amount_FT commission_FT
                                                in
                                                    swapOffer_Datum_Out `OnChainHelpers.isUnsafeEqDatums` swapOffer_Datum_Out_Control
                                            ------------------
                                            isCorrect_Output_SwapOffer_Value_With_SwapADAxFT :: Integer -> Integer -> Integer -> Bool
                                            isCorrect_Output_SwapOffer_Value_With_SwapADAxFT !amount_ADA !amount_FT !commission_FT =
                                                let
                                                    !value_Amount_ADA = LedgerAda.lovelaceValueOf amount_ADA
                                                    !value_Amount_FT = LedgerValue.assetClassValue fundFT_TN_AC (amount_FT - commission_FT)
                                                    ------------------
                                                    !valueFor_SwapOffer_Out_Control = LedgerApiV2.txOutValue input_TxOut_BeingValidated <> value_Amount_ADA <> negate value_Amount_FT
                                                in
                                                    valueOf_SwapOffer_Out `OnChainHelpers.isEqValue` valueFor_SwapOffer_Out_Control
                                    ----------------
                                    validateSwapADAxFT _ = False
                                    ----------------
                                    !protocolDatum_In = getLazyProtocolDatum_In
                                    !oraclePaymentPubKey = ProtocolT.pdOraclePaymentPubKey protocolDatum_In
                                    ------------------
                                    isCorrect_Conversion :: T.Oracle_Data -> Integer -> Integer -> Bool
                                    isCorrect_Conversion oracle_Data !amount_FT !amount_ADA =
                                        let
                                            !values = T.iuValues $ T.odFTPriceADA1xe6 oracle_Data
                                            !(cs, tn, priceADAx1e6) =
                                                if null values
                                                    then traceError "Oracle Data values are empty"
                                                    else head values
                                            !price_FT_in_ADA =
                                                if cs == fundPolicy_CS && tn == fundFT_TN
                                                    then priceADAx1e6
                                                    else traceError "FT Price ADA not found in Oracle Data"
                                            --------
                                            amount_ADA' = OnChainHelpers.multiply_By_Scaled_1e6_And_RoundUp amount_FT price_FT_in_ADA
                                        in
                                            --------
                                            amount_ADA == amount_ADA'
                                    ------------------
                                    isAmount_FT_Available :: Integer -> Bool
                                    isAmount_FT_Available !amount_FT =
                                        let
                                            !amount_FT_Available = T.sodAmount_FT_Available swapOffer_Datum_In
                                        in
                                            amount_FT_Available >= amount_FT
                                    ------------------
                                    isAmount_ADA_Available :: Integer -> Bool
                                    isAmount_ADA_Available !amount_ADA =
                                        let
                                            !amount_ADA_Available = T.sodAmount_ADA_Available swapOffer_Datum_In
                                        in
                                            amount_ADA_Available >= amount_ADA
                                    ------------------
                                    isCorrect_Commission :: Integer -> Integer -> Bool
                                    isCorrect_Commission !swap_Amount !commission_Payed =
                                        -- las comisiones son en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
                                        -- eso significa que al valor de Commission_InBPx1e3 tengo que dividirlo por
                                        -- 10e2 para pasarlo a bp
                                        -- 100 para pasarlo a porcentaje normal del 1 al 100
                                        -- 100 para pasarlo a porcentaje del 0 al 1
                                        -- den = 1e3 * 100 * 100 = 1000 * 100 * 100 = 10_000_000
                                        let
                                            !commissionRate = T.sodAskedCommission_InBPx1e3 swapOffer_Datum_In
                                            --------
                                            commission' = OnChainHelpers.multiply_By_Scaled_BPx1e3_And_RoundUp swap_Amount commissionRate
                                        in
                                            --------
                                            commission' == commission_Payed

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SwapOffer_Datum_With_StatusChanged #-}
mkUpdated_SwapOffer_Datum_With_StatusChanged :: T.SwapOffer_DatumType -> Integer -> T.SwapOffer_DatumType
mkUpdated_SwapOffer_Datum_With_StatusChanged !swapOffer_Datum_In !newStatus =
    swapOffer_Datum_In {T.sodOrder_Status = newStatus}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SwapOffer_Datum_With_CommissionChanged #-}
mkUpdated_SwapOffer_Datum_With_CommissionChanged :: T.SwapOffer_DatumType -> Integer -> T.SwapOffer_DatumType
mkUpdated_SwapOffer_Datum_With_CommissionChanged !swapOffer_Datum_In !newCommissionRate =
    swapOffer_Datum_In {T.sodAskedCommission_InBPx1e3 = newCommissionRate}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SwapOffer_Datum_With_RestrictionsChanged #-}
mkUpdated_SwapOffer_Datum_With_RestrictionsChanged :: T.SwapOffer_DatumType -> Integer -> Integer -> T.SwapOffer_DatumType
mkUpdated_SwapOffer_Datum_With_RestrictionsChanged !swapOffer_Datum_In !newAllowSellFT !newAllowSellADA =
    swapOffer_Datum_In {T.sodOrder_AllowSellADA = newAllowSellADA, T.sodOrder_AllowSellFT = newAllowSellFT}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SwapOffer_Datum_With_MinADAChanged #-}
mkUpdated_SwapOffer_Datum_With_MinADAChanged :: T.SwapOffer_DatumType -> Integer -> T.SwapOffer_DatumType
mkUpdated_SwapOffer_Datum_With_MinADAChanged !swapOffer_Datum_In !newMinADA =
    swapOffer_Datum_In {T.sodMinADA = newMinADA}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SwapOffer_Datum_With_Deposit #-}
mkUpdated_SwapOffer_Datum_With_Deposit :: T.SwapOffer_DatumType -> Integer -> Integer -> T.SwapOffer_DatumType
mkUpdated_SwapOffer_Datum_With_Deposit !swapOffer_Datum_In !newDeposit_FT !newDeposit_ADA =
    swapOffer_Datum_In
        { T.sodAmount_FT_Available = T.sodAmount_FT_Available swapOffer_Datum_In + newDeposit_FT
        , T.sodAmount_ADA_Available = T.sodAmount_ADA_Available swapOffer_Datum_In + newDeposit_ADA
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SwapOffer_Datum_With_Withdraw #-}
mkUpdated_SwapOffer_Datum_With_Withdraw :: T.SwapOffer_DatumType -> Integer -> Integer -> T.SwapOffer_DatumType
mkUpdated_SwapOffer_Datum_With_Withdraw !swapOffer_Datum_In !newWithdraw_FT !newWithdraw_ADA =
    swapOffer_Datum_In
        { T.sodAmount_FT_Available = T.sodAmount_FT_Available swapOffer_Datum_In - newWithdraw_FT
        , T.sodAmount_ADA_Available = T.sodAmount_ADA_Available swapOffer_Datum_In - newWithdraw_ADA
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SwapOffer_Datum_With_SwapFTxADA #-}
mkUpdated_SwapOffer_Datum_With_SwapFTxADA :: T.SwapOffer_DatumType -> Integer -> Integer -> Integer -> T.SwapOffer_DatumType
mkUpdated_SwapOffer_Datum_With_SwapFTxADA !swapOffer_Datum_In !amount_FT !amount_ADA !commission_ADA =
    swapOffer_Datum_In
        { T.sodAmount_FT_Available = T.sodAmount_FT_Available swapOffer_Datum_In + amount_FT
        , T.sodAmount_ADA_Available = T.sodAmount_ADA_Available swapOffer_Datum_In - (amount_ADA - commission_ADA)
        , T.sodTotal_ADA_Earned = T.sodTotal_ADA_Earned swapOffer_Datum_In + commission_ADA
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_SwapOffer_Datum_With_SwapADAxFT #-}
mkUpdated_SwapOffer_Datum_With_SwapADAxFT :: T.SwapOffer_DatumType -> Integer -> Integer -> Integer -> T.SwapOffer_DatumType
mkUpdated_SwapOffer_Datum_With_SwapADAxFT !swapOffer_Datum_In !amount_ADA !amount_FT !commission_FT =
    swapOffer_Datum_In
        { T.sodAmount_FT_Available = T.sodAmount_FT_Available swapOffer_Datum_In - (amount_FT - commission_FT)
        , T.sodAmount_ADA_Available = T.sodAmount_ADA_Available swapOffer_Datum_In + amount_ADA
        , T.sodTotal_FT_Earned = T.sodTotal_FT_Earned swapOffer_Datum_In + commission_FT
        }

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
mkWrappedPolicyID protocolPolicyID_CS swapOffer_Validator_Hash = mkPolicyID params
    where
        params =
            T.PolicyParams
                { ppProtocolPolicyID_CS = PlutusTx.unsafeFromBuiltinData protocolPolicyID_CS
                , ppSwapOffer_Validator_Hash = PlutusTx.unsafeFromBuiltinData swapOffer_Validator_Hash
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
