{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Protocol.Fund.OffChain where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Control.Monad as Monad
import qualified Data.Map as DataMap
import qualified Data.Set as Set
import qualified Data.Text as DataText (Text)
import qualified Ledger
import qualified Ledger.Ada as LedgerAda
import qualified Ledger.Constraints as LedgerConstraints
import qualified Ledger.Constraints.ValidityInterval as LedgerValidityInterval
import qualified Ledger.Crypto as Crypto
import qualified Ledger.Value as LedgerValue
import qualified Plutus.Contract as PlutusContract
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import qualified PlutusTx.Ratio as TxRatio
import qualified Text.Printf as TextPrintf (printf)
import qualified Prelude as P

--------------------------------------------------------------------------------2
-- Internal Imports
--------------------------------------------------------------------------------2

import qualified Generic.Constants as T
import qualified Generic.OffChainHelpers as OffChainHelpers
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Generic.Types as T
import qualified Protocol.Constants as T
import qualified Protocol.Fund.Helpers as FundHelpers
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.Fund.InvestUnit.Types as InvestUnitT
import qualified Protocol.OffChainHelpers as OffChainHelpers
import qualified Protocol.OnChainHelpers as OnChainHelpers
import qualified Protocol.PABTypes as T
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Types as T
import Protocol.Constants (maxDepositAndWithdraw_aux)

--------------------------------------------------------------------------------2
-- Module
--------------------------------------------------------------------------------2

endPointFundPrepare :: T.PABFundPrepareParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundPrepare T.PABFundPrepareParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "Fund Prepare"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let
        !protocolPABParams = pfppProtocolPABParams
    let
        !fundPABParams = pfppFundPABParams
    ---------------------
    let
        !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let
        !fundValidator_Hash = T.fppFundValidator_Hash fundPABParams
    ---------------------
    let
        !investUnitValidator_Hash = T.pppInvestUnitValidator_Hash protocolPABParams
    ---------------------
    let
        !fundPolicy_Params = T.fppFundPolicy_Params fundPABParams
        !fundPolicy_TxOutRef = FundT.ppFundPolicy_TxOutRef fundPolicy_Params
    ---------------------
    let
        !fundPolicy = T.fppFundPolicy fundPABParams
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
        !fundPolicy_ScriptHash = OffChainHelpers.hashScriptMinting fundPolicy
    ---------------------
    let
        !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
    ---------------------
    let
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address

    let
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
    ---------------------
    case find (\(txOutRef, _) -> txOutRef == fundPolicy_TxOutRef) (DataMap.toList uTxOsAtUser) of
        Nothing ->
            PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "%s : Can't find uTxO for mint FundPolicy" nameEndPoint
        Just _ -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "%s : uTxO for mint FundPolicy found!" nameEndPoint
    ---------------------
    let
        fundPolicy_TxOut = head [(t, ci) | (t, ci) <- DataMap.toList uTxOsAtUser, t == fundPolicy_TxOutRef]
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !scriptRef_With_fundPolicy' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundPolicy" fundPolicy_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    ---------------------
    let
        !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
        ---------------------
        !fundCategories = ProtocolT.pdFundCategories protocolDatum_In
        !selectedFundCategory' = find (\fundCategory -> ProtocolT.fcCategoryNumber fundCategory == pfppFundCategoryNumber) fundCategories
    ---------------------
    !requiredMAYZ <- case selectedFundCategory' of
        Nothing ->
            PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "%s : Can't find Fund Category: %s" nameEndPoint (P.show selectedFundCategory')
        Just selectedFundCategory -> do
            PlutusContract.logInfo @P.String $ TextPrintf.printf "%s : Fund Category: %s" nameEndPoint (P.show selectedFundCategory)
            PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
            return $ ProtocolT.fcRequiredMAYZ selectedFundCategory
    ---------------------
    let
        !tokenMAYZ_AC = T.tokenMAYZ_AC_aux
        !valueOf_RequiredMAYZ = LedgerValue.assetClassValue tokenMAYZ_AC requiredMAYZ
    ---------------------
    let
        !valueFor_Mint_FundID = LedgerValue.assetClassValue fundID_AC 1
        !valueFor_Mint_InvestUnitID = LedgerValue.assetClassValue investUnitID_AC 1
        ---------------------
        !valueFor_Mint_FundID_And_OtherIDs = valueFor_Mint_FundID <> valueFor_Mint_InvestUnitID
        ---------------------
        !valueFor_FundDatum' = valueFor_Mint_FundID
        !minADA_For_FundDatum = OnChainHelpers.calculateMinADAOfValue valueFor_FundDatum' True + 5_000_000 -- min ada para datum grande
        !value_MinADA_For_FundDatum = LedgerAda.lovelaceValueOf minADA_For_FundDatum
        !valueFor_FundDatum = valueFor_FundDatum' <> value_MinADA_For_FundDatum <> valueOf_RequiredMAYZ
        ---------------------
        !valueFor_InvestUnitDatum' = valueFor_Mint_InvestUnitID
        !minADA_For_InvestUnitDatum = OnChainHelpers.calculateMinADAOfValue valueFor_InvestUnitDatum' True + 5_000_000 -- min ada para datum grande
        !value_MinADA_For_InvestUnitDatum = LedgerAda.lovelaceValueOf minADA_For_InvestUnitDatum
        !valueFor_InvestUnitDatum = valueFor_InvestUnitDatum' <> value_MinADA_For_InvestUnitDatum
        ---------------------
        !fundFT_TN = pfppFundFT_TN
        !admins = pfppAdmins
        !tokenAdminPolicy_CS = pfppTokenAdminPolicy_CS
        !fundCategoryNumber = pfppFundCategoryNumber
        !beginAt = pfppBeginAt
        !deadline = pfppDeadline
        !closedAt = pfppClosedAt
        --
        !commission_PerYear_InBPx1e3 = pfppCommission_PerYear_InBPx1e3
        !monthsRemainingPlusOne = FundHelpers.getRemainingMonths deadline beginAt + 1
        -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120 000 000
        !den = 120_000_000
        !commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne]]
        ---
        !holdingsCount = 0
        !holdingsIndex = 0
        ---------------------
        !fundDatum_Out =
            FundT.mkFund_Datum
                fundPolicy_CS
                fundFT_TN
                fundValidator_Hash
                fundHoldingPolicyID_CS
                fundHoldingValidator_Hash
                investUnitValidator_Hash
                admins
                tokenAdminPolicy_CS
                fundCategoryNumber
                beginAt
                deadline
                closedAt
                commission_PerYear_InBPx1e3
                commissions_Table_Numerator_1e6
                holdingsCount
                holdingsIndex
                T.maxDepositAndWithdraw_aux
                T.tokenMAYZ_AC_aux
                requiredMAYZ
                minADA_For_FundDatum
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_Out: %s" (P.show fundDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundDatum: %s" (P.show valueFor_FundDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !investUnit = pfppInvestUnit
        !investUnitDatum_Out = InvestUnitT.mkInvestUnit_Datum fundPolicy_CS investUnit minADA_For_InvestUnitDatum
    ---------------------
    let
        !redeemer_For_Mint_FundID_And_OtherIDs = FundT.PolicyRedeemerMintID FundT.PolicyRedeemerMintIDType -- user Nothing
        ---------------------
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTxTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    do
        let
            (lookupsTx_Mint_FundAndOthersID, tx_Mint_FundAndOthersID) = OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Mint_FundID_And_OtherIDs (Just fundPolicy_TxOut) (Just redeemer_For_Mint_FundID_And_OtherIDs) scriptRef_With_fundPolicy' (Just fundPolicy)
        let
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> lookupsTx_Mint_FundAndOthersID
            tx =
                tx_Mint_FundAndOthersID
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) valueFor_FundDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum investUnitValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData investUnitDatum_Out) valueFor_InvestUnitDatum
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ------------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundUpdate :: T.PABFundUpdateParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundUpdate T.PABFundUpdateParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "Fund Update"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let
        !protocolPABParams = pfupProtocolPABParams
    let
        !fundPABParams = pfupFundPABParams
    ---------------------
    let
        !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let
        !fundValidator = T.fppFundValidator fundPABParams
        !fundValidator_Hash = T.fppFundValidator_Hash fundPABParams
        !fundValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundValidator
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    ---------------------
    !scriptRef_With_FundValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundValidator" fundValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let
        !valueOf_FundDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundDatum
        !valueFor_FundDatum_Out = valueOf_FundDatum_In
        ---------------------
        !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
    ---------------------
    let
        !admins = pfupAdmins
        !tokenAdminPolicy_CS = pfupTokenAdminPolicy_CS
        !maxDepositAndWithdraw = maxDepositAndWithdraw_aux
        ---------------------
        !fundDatum_Out =
            FundT.FundDatum $
                FundHelpers.mkUpdated_Fund_Datum_With_NormalChanges
                    fundDatum_In
                    admins
                    tokenAdminPolicy_CS
                    maxDepositAndWithdraw
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_In: %s" (P.show fundDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_Out: %s" (P.show fundDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !redeemer_For_Consuming_FundDatum = FundT.ValidatorRedeemerDatumUpdate FundT.ValidatorRedeemerDatumUpdateType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundDatum: %s" (P.show redeemer_For_Consuming_FundDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTxTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let
            (lookupsTx_Consume_FundDatum, tx_Consume_FundDatum) =
                OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundDatum redeemer_For_Consuming_FundDatum scriptRef_With_FundValidator' (Just fundValidator)
        let
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> lookupsTx_Consume_FundDatum
            tx =
                LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> tx_Consume_FundDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) valueFor_FundDatum_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

-- endPointFundEmergency :: T.PABFundEmergencyParams -> PlutusContract.Contract w s DataText.Text ()
-- endPointFundEmergency T.PABFundEmergencyParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
--     ---------------------
--     let nameEndPoint = "Fund Emergency"
--     OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
--     ---------------------
--     (now, _) <- PlutusContract.currentNodeClientTimeRange
--     PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
--     PlutusContract.logInfo @P.String "--------------------------------"
--     ---------------------
--     !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
--     !userAddsCardano <- PlutusContract.ownAddress
--     !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
--     ---------------------
--     OffChainHelpers.checkCollateral uTxOsAtUser
--     ---------------------
--     let !protocolPABParams = pfepProtocolPABParams
--     let !fundPABParams = pfepFundPABParams
--     ---------------------
--     let !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
--         !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
--         !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
--     ---------------------
--     let !fundValidator = T.fppFundValidator fundPABParams
--         !fundValidator_Hash = T.fppFundValidator_Hash fundPABParams
--         !fundValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundValidator
--         !fundValidator_Address = T.fppFundValidator_Address fundPABParams
--         !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
--     ---------------------
--     let !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
--         !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
--         !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
--     ---------------------
--     let !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
--     ---------------------
--     let !adminsWhotSigns = pfepAdmins
--     ---------------------
--     let !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
--         !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
--     ---------------------
--     !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
--     !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
--     !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
--     ---------------------
--     !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
--     !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
--     ---------------------
--     !scriptRef_With_FundValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundValidator" fundValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
--     ---------------------
--     let !valueOf_FundDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundDatum
--         !valueFor_FundDatum_Out = valueOf_FundDatum_In
--         ---------------------
--         !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
--         ---------------------
--         -- !isEmergency = negate $ FundT.fdInEmergency fundDatum_In
--         !fundDatum_Out = FundT.FundDatum $ FundHelpers.mkUpdated_Fund_Datum_With_Emergency fundDatum_In isEmergency
--         ---------------------
--         !redeemer_For_Consuming_FundDatum = FundT.ValidatorRedeemerEmergency $ FundT.ValidatorRedeemerEmergencyType adminsWhotSigns
--         ---------------------
--         !intervalOffset1 = 1000
--         !intervalOffset2 = T.validTxTimeRange - 1000
--         !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
--         ---------------------
--     do
--         let (lookupsTx_Consume_FundDatum, tx_Consume_FundDatum) =
--                     OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundDatum redeemer_For_Consuming_FundDatum scriptRef_With_FundValidator' (Just fundValidator)
--         let lookupsTx =
--                     LedgerConstraints.unspentOutputs uTxOsAtUser
--                     P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
--                     P.<> lookupsTx_Consume_FundDatum
--             tx =
--                     LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
--                     P.<> tx_Consume_FundDatum
--                     P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) valueFor_FundDatum_Out
--                     P.<> LedgerConstraints.mustValidateInTimeRange validityRange
--                     P.<> LedgerConstraints.mustBeSignedBy userPPKH

--         ---------------------
--         OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundHoldingAdd :: T.PABFundHoldingAddParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundHoldingAdd T.PABFundHoldingAddParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "Holding Add"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let
        !protocolPABParams = pfhapProtocolPABParams
    let
        !fundPABParams = pfhapFundPABParams
    ---------------------
    let
        !fundValidator = T.fppFundValidator fundPABParams
        !fundValidator_Hash = T.fppFundValidator_Hash fundPABParams
        !fundValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundValidator
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let
        !fundHoldingPolicyID = T.fppFundHoldingPolicyID fundPABParams
        !fundHoldingPolicyID_ScriptHash = OffChainHelpers.hashScriptMinting fundHoldingPolicyID
        !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
    ---------------------
    let
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    ---------------------
    !scriptRef_With_FundValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundValidator" fundValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    !scriptRef_With_FundHoldingPolicyID' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingPolicyID" fundHoldingPolicyID_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let
        !valueOf_FundDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundDatum
        !valueFor_FundDatum_Out = valueOf_FundDatum_In
    ---------------------
    let
        !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
    ---------------------
    let
        fundHolding_Index = FundT.fdHoldingsIndex fundDatum_In
        fundHoldingID_TN = LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS fundHolding_Index
        fundHoldingID_AC = LedgerValue.AssetClass (fundHoldingPolicyID_CS, fundHoldingID_TN)
    ---------------------
    let
        !valueFor_Mint_FundHoldingID = LedgerValue.assetClassValue fundHoldingID_AC 1
        ---------------------
        !valueFor_FundHoldingDatum_Out' = valueFor_Mint_FundHoldingID
        !minADA_For_FundHoldingDatum_Out = OnChainHelpers.calculateMinADAOfValue valueFor_FundHoldingDatum_Out' True + 5_000_000 -- min ada para datum grande
        !value_MinADA_For_FundHoldingDatum_Out = LedgerAda.lovelaceValueOf minADA_For_FundHoldingDatum_Out
        !valueFor_FundHoldingDatum_Out = valueFor_FundHoldingDatum_Out' <> value_MinADA_For_FundHoldingDatum_Out
        ---------------------
        !fundDatum_Out =
            FundT.FundDatum $ FundHelpers.mkUpdated_Fund_Datum_With_HoldingAdded fundDatum_In
        ---------------------
        !fundHoldingDatum_Out =
            FundHoldingT.FundHoldingDatum $
                FundHoldingT.FundHoldingDatumType
                    { FundHoldingT.hdVersion = FundHoldingT.ownVersion
                    , FundHoldingT.hdFundHolding_Index = FundT.fdHoldingsIndex fundDatum_In
                    , FundHoldingT.hdSubTotal_FT_Minted_Accumulated = 0
                    , FundHoldingT.hdSubTotal_FT_Minted = 0
                    , FundHoldingT.hdSubTotal_FT_Commissions = 0
                    , FundHoldingT.hdSubTotal_FT_Commissions_Total = 0
                    , FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 = 0
                    , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol = 0
                    , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers = 0
                    , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators = 0
                    , FundHoldingT.hdMinADA = minADA_For_FundHoldingDatum_Out
                    }
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_In: %s" (P.show fundDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundDatum_Out: %s" (P.show fundDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "holdingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !redeemer_For_Consuming_FundDatum = FundT.ValidatorRedeemerFundHoldingAdd FundT.ValidatorRedeemerFundHoldingAddType
        !redeemer_For_Mint_FundHoldingID = FundHoldingT.PolicyRedeemerMintID FundHoldingT.PolicyRedeemerMintIDType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundDatum: %s" (P.show redeemer_For_Consuming_FundDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Mint_FundHoldingID: %s" (P.show redeemer_For_Mint_FundHoldingID)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTxTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let
            (lookupsTx_Mint_FundHoldingID, tx_Mint_FundHoldingID) =
                OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Mint_FundHoldingID Nothing (Just redeemer_For_Mint_FundHoldingID) scriptRef_With_FundHoldingPolicyID' (Just fundHoldingPolicyID)
        let
            (lookupsTx_Consume_FundDatum, tx_Consume_FundDatum) =
                OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundDatum redeemer_For_Consuming_FundDatum scriptRef_With_FundValidator' (Just fundValidator)
        let
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> lookupsTx_Mint_FundHoldingID
                    P.<> lookupsTx_Consume_FundDatum
            tx =
                tx_Mint_FundHoldingID
                    P.<> tx_Consume_FundDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundDatum_Out) valueFor_FundDatum_Out
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundHoldingDelete :: T.PABFundHoldingDeleteParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundHoldingDelete _ = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "Holding Delete"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"

--------------------------------------------------------------------------------2

endPointFundDeposit :: T.PABFundDepositParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundDeposit T.PABFundDepositParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "Deposit"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !user = Ledger.unPaymentPubKeyHash userPPKH
    -- !userAdds = Ledger.pubKeyHashAddress userPPKH Nothing
    -- !userAddressStakingCredential = case OffChainHelpers.getStakePubKeyHash userAdds of
    --     Nothing              -> Nothing
    --     Just stakePubKeyHash -> Just $ Ledger.unStakePubKeyHash stakePubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let
        !protocolPABParams = pfdpProtocolPABParams
    let
        !fundPABParams = pfdpFundPABParams
    ---------------------
    let
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let
        !investUnitValidator_Address = T.pppInvestUnitValidator_Address protocolPABParams
        !investUnitValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId investUnitValidator_Address
    ---------------------
    let
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let
        !fundPolicy = T.fppFundPolicy fundPABParams
        !fundPolicy_ScriptHash = OffChainHelpers.hashScriptMinting fundPolicy
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let
        !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
    ---------------------
    let
        !deposit = pfdpAmount
    ---------------------
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_InvestUnitValidator <- PlutusContract.utxosAt investUnitValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_InvestUnitDatum <- OffChainHelpers.getFullUTxO_With_InvestUnitDatum_By_AC investUnitID_AC uTxOsAt_InvestUnitValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_By_CS fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    !scriptRef_With_FundPolicy' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundPolicy" fundPolicy_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let
        !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
    let
        !investUnitDatum_In = (\(_, _, dat) -> dat) uTxO_With_InvestUnitDatum
    ---------------------
    let
        !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
    ---------------------
    let
        !investUnit = InvestUnitT.iudInvestUnit investUnitDatum_In
        ---------------------
        !valueOf_TokensForDeposit = OffChainHelpers.mkValue_From_InvestUnit_And_Amount investUnit deposit
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_TokensForDeposit: %s" (P.show valueOf_TokensForDeposit)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    !valueOf_User <- OffChainHelpers.getBalanceOfUTXOs uTxOsAtUser
    ---------------------
    Monad.unless (OnChainHelpers.isIncludeValue valueOf_User valueOf_TokensForDeposit) P.$
        PlutusContract.throwError @DataText.Text $
            OffChainHelpers.stringToStrictText $
                TextPrintf.printf "You dont have enough tokens to deposit"
    ---------------------
    let
        !deadline = FundT.fdDeadline fundDatum_In
        -- !commission_PerYear_InBPx1e3 = FundT.fdCommission_PerYear_InBPx1e3 fundDatum_In
        !commissions_Table_Numerator_1e6 = FundT.fdCommissions_Table_Numerator_1e6 fundDatum_In
        ---------------------
        !monthsRemaining = FundHelpers.getRemainingMonths deadline now
        ---------------------
        !(userFT, commissionsFT, commissions_FT_Release_PerMonth_1e6) = FundHelpers.calculateDepositCommissionsUsingMonths commissions_Table_Numerator_1e6 deadline now deposit
    ------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "deposit: %s" (P.show deposit)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "monthsRemaining: %s" (P.show monthsRemaining)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commission_PerYear_InBPx1e3: %s" (P.show commission_PerYear_InBPx1e3)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionesPerDayPct: %s" (OffChainHelpers.displayRational 8 commissionesPerDayPct)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionesPerDayPct': %s" (OffChainHelpers.displayRational 8 commissionesPerDayPct')
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsAcumulated: %s" (OffChainHelpers.displayRational 8 commissionsAcumulated)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsAcumulated': %s" (OffChainHelpers.displayRational 8 commissionsAcumulated')
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "userFT: %s" (P.show userFT)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsFT: %s" (P.show commissionsFT)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissions_FT_Release_PerMonth_1e6: %s" (P.show commissions_FT_Release_PerMonth_1e6)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
        ---------------------
        !valueFor_Mint_FundFT = LedgerValue.assetClassValue fundFT_AC deposit
        ---------------------
        !valueFor_FT_User = LedgerValue.assetClassValue fundFT_AC userFT
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT
        ---------------------
        !valueFor_User_Out' = valueFor_FT_User
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    let
        !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> valueOf_TokensForDeposit P.<> valueFor_FT_Commissions
        ---------------------
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
    let
        !fundHoldingDatum_Out = FundHoldingT.FundHoldingDatum $ FundHelpers.mkUpdated_FundHolding_Datum_With_Deposit fundHoldingDatum_In deposit userFT commissionsFT commissions_FT_Release_PerMonth_1e6
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerDeposit $ FundHoldingT.ValidatorRedeemerDepositType now deposit
        !redeemer_For_Mint_FundFT = FundT.PolicyRedeemerMintFT FundT.PolicyRedeemerMintFTType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Mint_FundFT: %s" (P.show redeemer_For_Mint_FundFT)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTxTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let
            (lookupsTx_Mint_FundFT, tx_Mint_FundFT) =
                OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Mint_FundFT Nothing (Just redeemer_For_Mint_FundFT) scriptRef_With_FundPolicy' (Just fundPolicy)
        let
            (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_InvestUnitDatum])
                    P.<> lookupsTx_Mint_FundFT
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_InvestUnitDatum)
                    P.<> tx_Mint_FundFT
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundWithdraw :: T.PABFundWithdrawParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundWithdraw T.PABFundWithdrawParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "Withdraw"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !user = Ledger.unPaymentPubKeyHash userPPKH
    -- !userAdds = Ledger.pubKeyHashAddress userPPKH Nothing
    -- !userAddressStakingCredential = case OffChainHelpers.getStakePubKeyHash userAdds of
    --     Nothing              -> Nothing
    --     Just stakePubKeyHash -> Just $ Ledger.unStakePubKeyHash stakePubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let
        !protocolPABParams = pfwpProtocolPABParams
    let
        !fundPABParams = pfwpFundPABParams
    ---------------------
    let
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let
        !investUnitValidator_Address = T.pppInvestUnitValidator_Address protocolPABParams
        !investUnitValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId investUnitValidator_Address
    ---------------------
    let
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let
        !fundPolicy = T.fppFundPolicy fundPABParams
        !fundPolicy_ScriptHash = OffChainHelpers.hashScriptMinting fundPolicy
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let
        !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
    ---------------------
    let
        !withdraw = pfwpAmount
    ---------------------
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_InvestUnitValidator <- PlutusContract.utxosAt investUnitValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_InvestUnitDatum <- OffChainHelpers.getFullUTxO_With_InvestUnitDatum_By_AC investUnitID_AC uTxOsAt_InvestUnitValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    !scriptRef_With_FundPolicy' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundPolicy" fundPolicy_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let
        !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
    let
        !investUnitDatum_In = (\(_, _, dat) -> dat) uTxO_With_InvestUnitDatum
    ---------------------
    let
        !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
    ---------------------
    let
        !investUnit = InvestUnitT.iudInvestUnit investUnitDatum_In
        !investUnitTokens = T.iuValues investUnit
    ---------------------
    let
        !deadline = FundT.fdDeadline fundDatum_In
        -- !commission_PerYear_InBPx1e3 = FundT.fdCommission_PerYear_InBPx1e3 fundDatum_In
        !commissions_Table_Numerator_1e6 = FundT.fdCommissions_Table_Numerator_1e6 fundDatum_In
        ---------------------
        !monthsRemaining = FundHelpers.getRemainingMonths deadline now
        ---------------------
        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit investUnitTokens
        ---------------------
        !(commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6) = FundHelpers.calculateWithdrawCommissionsUsingMonths commissions_Table_Numerator_1e6 deadline now withdraw investUnit_Granularity
    ------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdraw: %s" (P.show withdraw)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "monthsRemaining: %s" (P.show monthsRemaining)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commission_PerYear_InBPx1e3: %s" (P.show commission_PerYear_InBPx1e3)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionesPerDayPct: %s" (OffChainHelpers.displayRational 8 commissionesPerDayPct)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionesPerDayPct': %s" (OffChainHelpers.displayRational 8 commissionesPerDayPct')
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsAcumulatedNotIncludingThisPeriod: %s" (OffChainHelpers.displayRational 8 commissionsAcumulatedNotIncludingThisPeriod)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsAcumulatedNotIncludingThisPeriod': %s" (OffChainHelpers.displayRational 8 commissionsAcumulatedNotIncludingThisPeriod')
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "userFT'forCalculationsOfCommissionsToGetBack: %s" (P.show userFT'forCalculationsOfCommissionsToGetBack)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissionsForUserFTToGetBack: %s" (P.show commissionsForUserFTToGetBack)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdrawPlusCommissionsGetBack: %s" (P.show withdrawPlusCommissionsGetBack)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "commissions_FT_Release_PerMonth_1e6: %s" (P.show commissions_FT_Release_PerMonth_1e6)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ------------------
    let
        !valueOf_TokensForWithdraw =
            foldl
                (P.<>)
                (LedgerAda.lovelaceValueOf 0)
                -- [(LedgerValue.assetClassValue (LedgerValue.AssetClass (LedgerApiV2.CurrencySymbol cs, LedgerApiV2.CurrencySymbol tn)) amt) | (cs, tn, amt) <- investUnitTokens]
                [ LedgerValue.assetClassValue
                    ( LedgerValue.AssetClass
                        (cs, tn)
                    )
                    (amt * withdrawPlusCommissionsGetBack)
                | (cs, tn, amt) <- investUnitTokens
                ]
    ---------------------
    let
        fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
        ---------------------
        !valueFor_Burn_FundFT = LedgerValue.assetClassValue fundFT_AC (negate withdrawPlusCommissionsGetBack)
        ---------------------
        !valueFor_FT_CommissionsToGetBack = LedgerValue.assetClassValue fundFT_AC commissionsForUserFTToGetBack
        ---------------------
        !valueFor_User_Out' = valueOf_TokensForWithdraw
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Enough_SubTotal_By_CS withdrawPlusCommissionsGetBack fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    let
        !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> negate valueOf_TokensForWithdraw P.<> negate valueFor_FT_CommissionsToGetBack
        ---------------------
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
    let
        !fundHoldingDatum_Out = FundHoldingT.FundHoldingDatum $ FundHelpers.mkUpdated_FundHolding_Datum_With_Withdraw fundHoldingDatum_In withdraw commissionsForUserFTToGetBack commissions_FT_Release_PerMonth_1e6
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_User_Out: %s" (P.show valueFor_User_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerWithdraw $ FundHoldingT.ValidatorRedeemerWithdrawType now withdraw withdrawPlusCommissionsGetBack
        !redeemer_For_Burn_FundFT = FundT.PolicyRedeemerBurnFT FundT.PolicyRedeemerBurnFTType
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Burn_FundFT: %s" (P.show redeemer_For_Burn_FundFT)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTxTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let
            (lookupsTx_Burn_FundFT, tx_Burn_FundFT) =
                OffChainHelpers.mintToken_With_RefPolicyOrAttachedPolicy valueFor_Burn_FundFT Nothing (Just redeemer_For_Burn_FundFT) scriptRef_With_FundPolicy' (Just fundPolicy)
        let
            (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_InvestUnitDatum])
                    P.<> lookupsTx_Burn_FundFT
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_InvestUnitDatum)
                    P.<> tx_Burn_FundFT
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundReIndexing :: T.PABFundReIndexingParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundReIndexing T.PABFundReIndexingParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "ReIndexing"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let
        !protocolPABParams = pfripProtocolPABParams
        !fundPABParams = pfripFundPABParams
    ---------------------
    let
        !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let
        !investUnitValidator = T.pppInvestUnitValidator protocolPABParams
        !investUnitValidator_Hash = T.pppInvestUnitValidator_Hash protocolPABParams
        !investUnitValidator_ScriptHash = OffChainHelpers.hashScriptValidator investUnitValidator
        !investUnitValidator_Address = T.pppInvestUnitValidator_Address protocolPABParams
        !investUnitValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId investUnitValidator_Address
    ---------------------
    let
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let
        !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
        !investUnitID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.investUnitID_TN)
    ---------------------
    let
        !investUnitTokensToAdd = pfripTokensToAdd
        !investUnitTokensToRemove = pfripTokensToRemove
        !holdingTxOutRef = pfripFundHoldingTxOutRef
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_InvestUnitValidator <- PlutusContract.utxosAt investUnitValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_InvestUnitDatum <- OffChainHelpers.getFullUTxO_With_InvestUnitDatum_By_AC investUnitID_AC uTxOsAt_InvestUnitValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS holdingTxOutRef fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !uTxOs_With_FundHoldingDatums <- OffChainHelpers.getUnsafe_TxOutRefs_DecoratedTxOuts_And_DatumsTypes_By_CS @FundHoldingT.ValidatorDatum @FundHoldingT.FundHoldingDatumType fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator FundHoldingT.getFundHolding_DatumType
    let
        !uTxOs_With_FundHoldingDatums_WithoutSelected = filter (\(ref, _, _) -> ref /= holdingTxOutRef) uTxOs_With_FundHoldingDatums
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    !scriptRef_With_InvestUnitValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "InvestUnitValidator" investUnitValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let
        !investUnitDatum_In = (\(_, _, dat) -> dat) uTxO_With_InvestUnitDatum
        !investUnit_In = InvestUnitT.iudInvestUnit investUnitDatum_In
        !investUnitTokens_In = T.iuValues investUnit_In
        ---------------------
        !tokensToAdd = T.iuValues investUnitTokensToAdd
        !tokensToRemove = T.iuValues investUnitTokensToRemove
    ---------------------
    let
        !investUnit_Out' = OnChainHelpers.flattenValueAdd investUnitTokens_In tokensToAdd
        !investUnit_Out = OnChainHelpers.flattenValueSub investUnit_Out' tokensToRemove
        ---------------------
        !valueOf_InvestUnitDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_InvestUnitDatum
        !valueFor_InvestUnitDatum_Out = valueOf_InvestUnitDatum_In
        ---------------------
        !investUnitDatum_Out = InvestUnitT.mkInvestUnit_Datum (InvestUnitT.iudFundPolicy_CS investUnitDatum_In) (T.InvestUnit investUnit_Out) (InvestUnitT.iudMinADA investUnitDatum_In)
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "investUnitDatum_In: %s" (P.show investUnitDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "investUnitDatum_Out: %s" (P.show investUnitDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        tokensPricesToAddADA = [(cs, tn, 100) | (cs, tn, _) <- tokensToAdd]
        tokensPricesToRemoveADA = [(cs, tn, 100) | (cs, tn, _) <- tokensToRemove]
        ---------------------
        uniquePairs :: P.Ord cs => P.Ord tn => [(cs, tn, Integer)] -> [(cs, tn, Integer)]
        uniquePairs tokens =
            Set.toList $ Set.fromList [(cs, tn, amt) | (cs, tn, amt) <- tokens]
        ---------------------
        tokensPricesADA = T.InvestUnit {T.iuValues = uniquePairs (tokensPricesToAddADA ++ tokensPricesToRemoveADA)}
        ---------------------
        oracleReIdx_Data =
            T.OracleReIdx_Data
                { T.oridTokensPriceADA = tokensPricesADA
                , T.oridTime = now - LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.oracleData_Valid_Time_aux `divide` 2)
                }
    ---------------------
    let
        oracleWallet_XPriv = Crypto.generateFromSeed' T.oracleWallet_Seed_aux
        oracleReIdx_DataBBS = OnChainHelpers.oracleReIdxDataToBBS oracleReIdx_Data
    let
        oracleSignature = Crypto.sign' oracleReIdx_DataBBS oracleWallet_XPriv
    ---------------------
    let
        findPriceADA :: T.CS -> T.TN -> PlutusContract.Contract w s DataText.Text P.Integer
        findPriceADA cs tn =
            case find (\(cs', tn', _) -> cs' == cs && tn' == tn) (T.iuValues tokensPricesADA) of
                Nothing -> PlutusContract.throwError @DataText.Text $ OffChainHelpers.stringToStrictText $ TextPrintf.printf "No price found for %s.%s" (P.show cs) (P.show tn)
                Just (_, _, price) -> return price
    -------------------
    !priceADAOf_TokensForTokensToAdd <-
        Monad.foldM
            ( \acc (cs, tn, amt) -> do
                price <- findPriceADA cs tn
                return (acc + amt * price)
            )
            0
            tokensToAdd
    -------------------
    !priceADAOf_TokensForTokensToRemove <-
        Monad.foldM
            ( \acc (cs, tn, amt) -> do
                price <- findPriceADA cs tn
                return (acc + amt * price)
            )
            0
            tokensToRemove
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "priceADAOf_TokensForTokensToAdd: %s" (P.show priceADAOf_TokensForTokensToAdd)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "priceADAOf_TokensForTokensToRemove: %s" (P.show priceADAOf_TokensForTokensToRemove)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    Monad.when (priceADAOf_TokensForTokensToAdd /= priceADAOf_TokensForTokensToRemove) P.$
        PlutusContract.throwError @DataText.Text $
            OffChainHelpers.stringToStrictText $
                TextPrintf.printf "Prices dont match"
    ---------------------
    let
        getSubTotalUI :: (LedgerApiV2.TxOutRef, Ledger.DecoratedTxOut, FundHoldingT.FundHoldingDatumType) -> P.Integer
        getSubTotalUI (_, _, !dat) = FundHoldingT.hdSubTotal_FT_Minted dat
        !total_Deposits_IU = P.sum (getSubTotalUI <$> uTxOs_With_FundHoldingDatums)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "total_Deposits_IU: %s" (P.show total_Deposits_IU)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !valueOf_TotalTokensToAdd = OnChainHelpers.flattenValueToValue [(cs, tn, am * total_Deposits_IU) | (cs, tn, am) <- tokensToAdd]
        !valueOf_TotalTokensToRemove = OnChainHelpers.flattenValueToValue [(cs, tn, am * total_Deposits_IU) | (cs, tn, am) <- tokensToRemove]
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_TotalTokensToAdd: %s" (P.show valueOf_TotalTokensToAdd)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_TotalTokensToRemove: %s" (P.show valueOf_TotalTokensToRemove)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
    ---------------------
    Monad.unless (OnChainHelpers.isIncludeValue valueOf_FundHoldingDatum_In valueOf_TotalTokensToRemove) P.$
        PlutusContract.throwError @DataText.Text $
            OffChainHelpers.stringToStrictText $
                TextPrintf.printf "FundHolding selected does not have enough tokens to delete"
    ---------------------
    let
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> valueOf_TotalTokensToAdd P.<> negate valueOf_TotalTokensToRemove
        ---------------------
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
        ---------------------
        !fundHoldingDatum_Out = FundHoldingT.FundHoldingDatum fundHoldingDatum_In
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !redeemer_For_Consuming_InvestUnitDatum =
            InvestUnitT.ValidatorRedeemerReIndexing $
                InvestUnitT.ValidatorRedeemerReIndexingType
                    (T.InvestUnit tokensToAdd)
                    (T.InvestUnit tokensToRemove)
                    oracleReIdx_Data
                    oracleSignature
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_InvestUnitDatum: %s" (P.show redeemer_For_Consuming_InvestUnitDatum)
    ---------------------
    let
        !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerReIndexing $ FundHoldingT.ValidatorRedeemerReIndexingType (T.InvestUnit tokensToAdd) (T.InvestUnit tokensToRemove)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTxTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let
            (lookupsTx_Consume_InvestUnitDatum, tx_Consume_InvestUnitDatum) =
                OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_InvestUnitDatum redeemer_For_Consuming_InvestUnitDatum scriptRef_With_InvestUnitValidator' (Just investUnitValidator)
        let
            (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList ((\(ref, dec, _) -> (ref, dec)) <$> uTxOs_With_FundHoldingDatums_WithoutSelected))
                    P.<> lookupsTx_Consume_InvestUnitDatum
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> mconcat [LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundHoldingDatum') | uTxO_With_FundHoldingDatum' <- uTxOs_With_FundHoldingDatums_WithoutSelected]
                    P.<> tx_Consume_InvestUnitDatum
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum investUnitValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData investUnitDatum_Out) valueFor_InvestUnitDatum_Out
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundCollect_Protocol_Commission :: T.PABFundCollect_Protocol_CommissionParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundCollect_Protocol_Commission T.PABFundCollect_Protocol_CommissionParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "Withdraw Protocol Commissions"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !user = Ledger.unPaymentPubKeyHash userPPKH
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let
        !protocolPABParams = pfwpcpProtocolPABParams
    let
        !fundPABParams = pfwpcpFundPABParams
    ---------------------
    let
        !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let
        !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    let
        !withdraw = pfwpcpAmount
        !holdingTxOutRef = pfwpcpFundHoldingTxOutRef
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS holdingTxOutRef fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let
        !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
    ---------------------
    let
        !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
    let
        !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
        ---------------------
        !deadline = FundT.fdDeadline fundDatum_In
        ------------------
        !shareBPx1e2 = ProtocolT.pdShare_InBPx1e2_Protocol protocolDatum_In
        !sharePct = TxRatio.unsafeRatio shareBPx1e2 10_000
        !taken = FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol fundHoldingDatum_In
        !available = FundHelpers.getCommissionsAvailable deadline fundHoldingDatum_In shareBPx1e2 taken now
    ------------------
    -- !msPerDay = 1000 * 60 * 60 * 24
    -- !msPerMonth = msPerDay * 30
    -- !msRemaining =  LedgerApiV2.getPOSIXTime  $ FundT.fdDeadline fundDatum_In - now
    -- !monthsRemainingRational = TxRatio.unsafeRatio msRemaining msPerMonth
    -- -- !rate = TxRatio.unsafeRatio (FundHoldingT.hdSubTotal_Commissions_RateNumerator fundHoldingDatum_In) (FundHoldingT.hdSubTotal_Commissions_RateDenominator fundHoldingDatum_In)
    -- !rate = TxRatio.unsafeRatio (FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 fundHoldingDatum_In) 1_000_000
    -- !commisionsReady = TxRatio.fromInteger totalCommisionsAcum - (monthsRemainingRational * rate)
    -- !sharePct = TxRatio.unsafeRatio (ProtocolT.pdShare_InBPx1e2_Protocol protocolDatum_In) 10_000
    -- !commisionsReady_For_Protocol = commisionsReady * sharePct
    -- !alreadyWithdraw_Protocol = FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol fundHoldingDatum_In
    -- !available = commisionsReady_For_Protocol - TxRatio.fromInteger alreadyWithdraw_Protocol
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdrawing: %s" (P.show withdraw)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "msRemainin: %s" (P.show msRemaining)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "monthsRemainingRational: %s" (OffChainHelpers.displayRational 4 monthsRemainingRational)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "totalCommisionsAcum: %s" (P.show totalCommisionsAcum)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "rate: %s" (OffChainHelpers.displayRational 4 rate)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commisionsReady: %s" (OffChainHelpers.displayRational 4 commisionsReady)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "sharePct: %s" (OffChainHelpers.displayRational 4 sharePct)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commisionsReady_For_Protocol: %s" (OffChainHelpers.displayRational 4 commisionsReady_For_Protocol)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "taken: %s" (P.show taken)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "available: %s" (P.show available)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    Monad.when (available < withdraw) P.$ PlutusContract.throwError "Not enough commissions to withdraw"
    ---------------------
    let
        fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
        ---------------------
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC withdraw
        ---------------------
        !valueFor_User_Out' = valueFor_FT_Commissions
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    let
        !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> negate valueFor_FT_Commissions
    ---------------------
    let
        !fundHoldingDatum_Out =
            FundHoldingT.FundHoldingDatum $ FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission fundHoldingDatum_In withdraw
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_User_Out: %s" (P.show valueFor_User_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerCollect_Protocol_Commission $ FundHoldingT.ValidatorRedeemerCollect_Protocol_CommissionType now withdraw
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTxTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let
            (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundCollect_Delegators_Commission :: T.PABFundCollect_Delegators_CommissionParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundCollect_Delegators_Commission T.PABFundCollect_Delegators_CommissionParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "Withdraw MAYZ Commissions"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !user = Ledger.unPaymentPubKeyHash userPPKH
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let
        !protocolPABParams = pfwmcpProtocolPABParams
    let
        !fundPABParams = pfwmcpFundPABParams
    ---------------------
    let
        !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let
        !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    let
        !withdraw = pfwmcpAmount
        !holdingTxOutRef = pfwmcpFundHoldingTxOutRef
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS holdingTxOutRef fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let
        !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
    ---------------------
    let
        !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
    let
        !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
        ---------------------
        !deadline = FundT.fdDeadline fundDatum_In
        ------------------
        !shareBPx1e2 = ProtocolT.pdShare_InBPx1e2_Delegators protocolDatum_In
        !sharePct = TxRatio.unsafeRatio shareBPx1e2 10_000
        !taken = FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators fundHoldingDatum_In
        !available = FundHelpers.getCommissionsAvailable deadline fundHoldingDatum_In shareBPx1e2 taken now
    ------------------
    -- !msPerDay = 1000 * 60 * 60 * 24
    -- !msPerMonth = msPerDay * 30
    -- !msRemaining =  LedgerApiV2.getPOSIXTime  $ FundT.fdDeadline fundDatum_In - now
    -- !monthsRemainingRational =TxRatio.unsafeRatio msRemaining msPerMonth
    -- -- !rate = TxRatio.unsafeRatio (FundHoldingT.hdSubTotal_Commissions_RateNumerator fundHoldingDatum_In) (FundHoldingT.hdSubTotal_Commissions_RateDenominator fundHoldingDatum_In)
    -- !rate = TxRatio.unsafeRatio (FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 fundHoldingDatum_In) 1_000_000
    -- !commisionsReady = TxRatio.fromInteger totalCommisionsAcum - (monthsRemainingRational * rate)
    -- !sharePct = TxRatio.unsafeRatio (ProtocolT.pdShare_InBPx1e2_Delegators protocolDatum_In) 10_000
    -- !commisionsReady_For_MAYZ = commisionsReady * sharePct
    -- !alreadyWithdraw_MAYZ  = FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators fundHoldingDatum_In
    -- !available = commisionsReady_For_MAYZ - TxRatio.fromInteger alreadyWithdraw_MAYZ
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdrawing: %s" (P.show withdraw)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "msRemainin: %s" (P.show msRemaining)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "monthsRemainingRational: %s" (OffChainHelpers.displayRational 4 monthsRemainingRational)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "totalCommisionsAcum: %s" (P.show totalCommisionsAcum)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "rate: %s" (OffChainHelpers.displayRational 4 rate)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commisionsReady: %s" (OffChainHelpers.displayRational 4 commisionsReady)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "sharePct: %s" (OffChainHelpers.displayRational 4 sharePct)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commisionsReady_For_MAYZ: %s" (OffChainHelpers.displayRational 4 commisionsReady_For_MAYZ)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "taken : %s" (P.show taken)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "available: %s" (P.show available)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    Monad.when (available < withdraw) P.$ PlutusContract.throwError "Not enough commissions to withdraw"
    ---------------------
    let
        fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
        ---------------------
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC withdraw
        ---------------------
        !valueFor_User_Out' = valueFor_FT_Commissions
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    let
        !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> negate valueFor_FT_Commissions
    ---------------------
    let
        !fundHoldingDatum_Out =
            FundHoldingT.FundHoldingDatum $ FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission fundHoldingDatum_In withdraw
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_User_Out: %s" (P.show valueFor_User_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerCollect_Delegators_Commission $ FundHoldingT.ValidatorRedeemerCollect_Delegators_CommissionType now withdraw
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTxTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let
            (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2

endPointFundCollect_Managers_Commission :: T.PABFundCollect_Managers_CommissionParams -> PlutusContract.Contract w s DataText.Text ()
endPointFundCollect_Managers_Commission T.PABFundCollect_Managers_CommissionParams {..} = PlutusContract.handleError OffChainHelpers.handleContractError $ do
    ---------------------
    let
        nameEndPoint = "Withdraw Fund Admins Commissions"
    OffChainHelpers.printTitle (nameEndPoint ++ " : Init")
    ---------------------
    (now, _) <- PlutusContract.currentNodeClientTimeRange
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Time: %s" (P.show now)
    PlutusContract.logInfo @P.String "--------------------------------"
    ---------------------
    !userPPKH <- PlutusContract.ownFirstPaymentPubKeyHash
    let
        !user = Ledger.unPaymentPubKeyHash userPPKH
    !userAddsCardano <- PlutusContract.ownAddress
    !uTxOsAtUser <- PlutusContract.utxosAt userAddsCardano
    ---------------------
    OffChainHelpers.checkCollateral uTxOsAtUser
    ---------------------
    let
        !protocolPABParams = pfwfcpProtocolPABParams
    let
        !fundPABParams = pfwfcpFundPABParams
    ---------------------
    let
        !protocolPolicyID_CS = T.pppProtocolPolicyID_CS protocolPABParams
        !protocolValidator_Address = T.pppProtocolValidator_Address protocolPABParams
        !protocolValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId protocolValidator_Address
    ---------------------
    let
        !fundValidator_Address = T.fppFundValidator_Address fundPABParams
        !fundValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundValidator_Address
    ---------------------
    let
        !scriptPolicyID_CS = T.pppScriptPolicyID_CS protocolPABParams
        !scriptValidator_Address = T.pppScriptValidator_Address protocolPABParams
        !scriptValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId scriptValidator_Address
    ---------------------
    let
        !fundPolicy_CS = T.fppFundPolicy_CS fundPABParams
    ---------------------
    let
        !fundHoldingPolicyID_CS = T.fppFundHoldingPolicyID_CS fundPABParams
        !fundHoldingValidator = T.fppFundHoldingValidator fundPABParams
        !fundHoldingValidator_Hash = T.fppFundHoldingValidator_Hash fundPABParams
        !fundHoldingValidator_ScriptHash = OffChainHelpers.hashScriptValidator fundHoldingValidator
        !fundHoldingValidator_Address = T.fppFundHoldingValidator_Address fundPABParams
        !fundHoldingValidator_AddressCardano = OffChainHelpers.addressToCardanoAddress T.networkId fundHoldingValidator_Address
    ---------------------
    let
        !protocolID_AC = LedgerValue.AssetClass (protocolPolicyID_CS, T.protocolID_TN)
        !fundID_AC = LedgerValue.AssetClass (fundPolicy_CS, T.fundID_TN)
    ---------------------
    let
        !withdraw = pfwfcpAmount
        !holdingTxOutRef = pfwfcpFundHoldingTxOutRef
    ---------------------
    !uTxOsAt_ProtocolValidator <- PlutusContract.utxosAt protocolValidator_AddressCardano
    !uTxOsAt_FundValidator <- PlutusContract.utxosAt fundValidator_AddressCardano
    !uTxOsAt_FundHoldingValidator <- PlutusContract.utxosAt fundHoldingValidator_AddressCardano
    !uTxOsAt_ScriptValidator <- PlutusContract.utxosAt scriptValidator_AddressCardano
    ---------------------
    !uTxO_With_ProtocolDatum <- OffChainHelpers.getFullUTxO_With_ProtocolDatum_By_AC protocolID_AC uTxOsAt_ProtocolValidator
    !uTxO_With_FundDatum <- OffChainHelpers.getFullUTxO_With_FundDatum_By_AC fundID_AC uTxOsAt_FundValidator
    !uTxO_With_FundHoldingDatum <- OffChainHelpers.getFullUTxO_With_FundHoldingDatum_And_Selected_By_CS holdingTxOutRef fundHoldingPolicyID_CS uTxOsAt_FundHoldingValidator
    ---------------------
    !scriptRef_With_FundHoldingValidator' <- OffChainHelpers.getMaybeUTxO_With_ScriptRef "FundHoldingValidator" fundHoldingValidator_ScriptHash scriptPolicyID_CS uTxOsAt_ScriptValidator
    ---------------------
    let
        !protocolDatum_In = (\(_, _, dat) -> dat) uTxO_With_ProtocolDatum
    ---------------------
    let
        !fundDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundDatum
        !fundHoldingDatum_In = (\(_, _, dat) -> dat) uTxO_With_FundHoldingDatum
    ---------------------
    let
        !fundFT_TN = FundT.fdFundFT_TN fundDatum_In
        ---------------------
        !deadline = FundT.fdDeadline fundDatum_In
        ------------------
        !shareBPx1e2 = ProtocolT.pdShare_InBPx1e2_Managers protocolDatum_In
        !sharePct = TxRatio.unsafeRatio shareBPx1e2 10_000
        !taken = FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers fundHoldingDatum_In
        !available = FundHelpers.getCommissionsAvailable deadline fundHoldingDatum_In shareBPx1e2 taken now
    ------------------
    -- !msPerDay = 1000 * 60 * 60 * 24
    -- !msPerMonth = msPerDay * 30
    -- !msRemaining =  LedgerApiV2.getPOSIXTime  $ FundT.fdDeadline fundDatum_In - now
    -- !monthsRemainingRational =TxRatio.unsafeRatio   msRemaining  msPerMonth
    -- !rate = TxRatio.unsafeRatio (FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 fundHoldingDatum_In) 1_000_000
    -- -- !rate = TxRatio.unsafeRatio (FundHoldingT.hdSubTotal_Commissions_RateNumerator fundHoldingDatum_In) (FundHoldingT.hdSubTotal_Commissions_RateDenominator fundHoldingDatum_In)
    -- !commisionsReady = TxRatio.fromInteger totalCommisionsAcum - (monthsRemainingRational * rate)
    -- !sharePct = TxRatio.unsafeRatio (ProtocolT.pdShare_InBPx1e2_Managers protocolDatum_In) 10_000
    -- !commisionsReady_For_Admins = commisionsReady * sharePct
    -- !alreadyWithdraw_Admins = FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers fundHoldingDatum_In
    -- !available = commisionsReady_For_Admins - TxRatio.fromInteger alreadyWithdraw_Admins
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "withdrawing: %s" (P.show withdraw)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "msRemainin: %s" (P.show msRemaining)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "monthsRemainingRational: %s" (OffChainHelpers.displayRational 4 monthsRemainingRational)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "totalCommisionsAcum: %s" (P.show totalCommisionsAcum)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "rate: %s" (OffChainHelpers.displayRational 4 rate)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commisionsReady: %s" (OffChainHelpers.displayRational 4 commisionsReady)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "sharePct: %s" (OffChainHelpers.displayRational 4 sharePct)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "commisionsReady_For_Admins: %s" (OffChainHelpers.displayRational 4 commisionsReady_For_Admins)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "taken: %s" (P.show taken)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "available: %s" (P.show available)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    Monad.when (available < withdraw) P.$ PlutusContract.throwError "Not enough commissions to withdraw"
    ---------------------
    let
        fundFT_AC = LedgerValue.AssetClass (fundPolicy_CS, fundFT_TN)
        ---------------------
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC withdraw
        ---------------------
        !valueFor_User_Out' = valueFor_FT_Commissions
        !minADA_For_User_Out = OnChainHelpers.calculateMinADAOfValue valueFor_User_Out' True
        !value_MinADA_For_User_Out = LedgerAda.lovelaceValueOf minADA_For_User_Out
        !valueFor_User_Out = valueFor_User_Out' <> value_MinADA_For_User_Out
    ---------------------
    let
        !valueOf_FundHoldingDatum_In = OffChainHelpers.getValueFromDecoratedTxOut $ (\(_, dec, _) -> dec) uTxO_With_FundHoldingDatum
        !valueFor_FundHoldingDatum_Out = valueOf_FundHoldingDatum_In P.<> negate valueFor_FT_Commissions
    ---------------------
    let
        !fundHoldingDatum_Out =
            FundHoldingT.FundHoldingDatum $ FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission fundHoldingDatum_In withdraw
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_In: %s" (P.show fundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "fundHoldingDatum_Out: %s" (P.show fundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueOf_FundHoldingDatum_In: %s" (P.show valueOf_FundHoldingDatum_In)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_FundHoldingDatum_Out: %s" (P.show valueFor_FundHoldingDatum_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    PlutusContract.logInfo @P.String $ TextPrintf.printf "valueFor_User_Out: %s" (P.show valueFor_User_Out)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !redeemer_For_Consuming_FundHoldingDatum = FundHoldingT.ValidatorRedeemerCollect_Managers_Commission $ FundHoldingT.ValidatorRedeemerCollect_Managers_CommissionType now withdraw
    PlutusContract.logInfo @P.String $ TextPrintf.printf "redeemer_For_Consuming_FundHoldingDatum: %s" (P.show redeemer_For_Consuming_FundHoldingDatum)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "-------------------"
    ---------------------
    let
        !intervalOffset1 = 1000
        !intervalOffset2 = T.validTxTimeRange - 1000
        !validityRange = LedgerValidityInterval.interval (now - intervalOffset1) (now + intervalOffset2)
    ---------------------
    do
        let
            (lookupsTx_Consume_FundHoldingDatum, tx_Consume_FundHoldingDatum) =
                OffChainHelpers.mustSpendScriptOutput_With_RefPolicyOrAttachedPolicy' uTxO_With_FundHoldingDatum redeemer_For_Consuming_FundHoldingDatum scriptRef_With_FundHoldingValidator' (Just fundHoldingValidator)
        let
            lookupsTx =
                LedgerConstraints.unspentOutputs uTxOsAtUser
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_ProtocolDatum])
                    P.<> LedgerConstraints.unspentOutputs (DataMap.fromList [(\(ref, dec, _) -> (ref, dec)) uTxO_With_FundDatum])
                    P.<> lookupsTx_Consume_FundHoldingDatum
            tx =
                LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_ProtocolDatum)
                    P.<> LedgerConstraints.mustReferenceOutput ((\(ref, _, _) -> ref) uTxO_With_FundDatum)
                    P.<> tx_Consume_FundHoldingDatum
                    P.<> LedgerConstraints.mustPayToOtherScriptWithInlineDatum fundHoldingValidator_Hash (LedgerApiV2.Datum $ PlutusTx.toBuiltinData fundHoldingDatum_Out) valueFor_FundHoldingDatum_Out
                    P.<> LedgerConstraints.mustPayToPubKey (Ledger.PaymentPubKeyHash user) valueFor_User_Out
                    P.<> LedgerConstraints.mustValidateInTimeRange validityRange
                    P.<> LedgerConstraints.mustBeSignedBy userPPKH
        ---------------------
        OffChainHelpers.evalAndSubmitTx' nameEndPoint protocolPABParams (Just fundPABParams) lookupsTx tx

--------------------------------------------------------------------------------2
