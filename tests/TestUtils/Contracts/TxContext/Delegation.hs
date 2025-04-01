--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : TestUtils.Contracts.TxContext.Delegation
Description : Mock Data and Auxiliary Functions for testing the Delegation.
-}
module TestUtils.Contracts.TxContext.Delegation where

--------------------------------------------------------------------------------3

-- Non-IOG imports

-- IOG imports
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import PlutusTx.Prelude

-- Project imports

import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Ledger.Value as LedgerValue
import qualified Protocol.Constants as T
import qualified Protocol.Delegation.OnChain as Delegation
import qualified Protocol.Delegation.Types as DelegationT
import TestUtils.Contracts.InitialData
import TestUtils.Helpers
import TestUtils.TestContext.Helpers
import TestUtils.Types
import TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- Delegation Contract
--------------------------------------------------------------------------------

delegation_Create_TxContext :: TestParams -> LedgerApiV2.ScriptContext
delegation_Create_TxContext tp =
    mkContext
        |> setInputsRef [protocol_UTxO_MockData tp, fund_UTxO_MockData tp]
        |> setOutputs [delegation_UTxO_MockData tp]
        |> setMintAndAddRedeemers
            [
                ( LedgerApiV2.singleton
                    (tpDelegationPolicyID_CS tp)
                    T.delegationID_TN
                    1
                , DelegationT.mkMintIDRedeemer
                )
            ]
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

delegation_Delete_TxContext :: TestParams -> LedgerApiV2.ScriptContext
delegation_Delete_TxContext tp =
    mkContext
        |> setInputsRef
            [ protocol_UTxO_MockData tp
            , uTxOForValidatorAsReference tp (tpDelegationValidator tp)
            , uTxOForMintingAsReference tp (tpDelegationPolicyID tp)
            ]
        |> setInputsAndAddRedeemers
            [(delegation_UTxO_MockData tp, DelegationT.mkDeleteRedeemer)]
        |> setMintAndAddRedeemers
            [
                ( LedgerApiV2.singleton (tpDelegationPolicyID_CS tp) T.delegationID_TN $
                    negate 1
                , DelegationT.mkBurnIDRedeemer
                )
            ]
        |> setSignatories [tpDelegationAdmin tp]
        |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

delegation_UpdateMinADA_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
delegation_UpdateMinADA_TxContext tp newMinADA =
    let
        input_Delegation_UTxO = delegation_UTxO_MockData tp
        -----------------
        input_Delegation_Datum = DelegationT.getDelegation_DatumType_From_UTxO input_Delegation_UTxO
        -----------------
        input_Delegation_Value = LedgerApiV2.txOutValue input_Delegation_UTxO
        -----------------
        output_Delegation_Datum =
            Delegation.mkUpdated_Delegation_Datum_With_MinADAChanged
                input_Delegation_Datum
                newMinADA
        -----------------
        output_Delegation_UTxO =
            input_Delegation_UTxO
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        DelegationT.mkDatum output_Delegation_Datum
                , LedgerApiV2.txOutValue =
                    changeValue_Amount
                        input_Delegation_Value
                        OnChainHelpers.adaAssetClass
                        newMinADA
                }
    in
        -----------------

        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_Delegation_UTxO, DelegationT.mkUpdateMinADARedeemer)]
            |> setOutputs [output_Delegation_UTxO]
            |> setSignatories [tpDelegationAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

delegation_Deposit_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
delegation_Deposit_TxContext tp !newDeposit_MAYZ =
    let
        input_Delegation_UTxO = delegation_UTxO_MockData tp
        -----------------
        input_Delegation_Datum = DelegationT.getDelegation_DatumType_From_UTxO input_Delegation_UTxO
        input_Delegation_Value = LedgerApiV2.txOutValue input_Delegation_UTxO
        -----------------
        output_Delegation_Datum =
            Delegation.mkUpdated_Delegation_Datum_With_Deposit
                input_Delegation_Datum
                newDeposit_MAYZ
        -----------------
        newAmount_MAYZ = DelegationT.ddDelegated_MAYZ output_Delegation_Datum
        -----------------
        output_Delegation_UTxO =
            input_Delegation_UTxO
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        DelegationT.mkDatum output_Delegation_Datum
                , LedgerApiV2.txOutValue =
                    changeValue_Amount
                        input_Delegation_Value
                        (LedgerValue.assetClass (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp))
                        newAmount_MAYZ
                }
    in
        -----------------

        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_Delegation_UTxO, DelegationT.mkDepositRedeemer newDeposit_MAYZ)]
            |> setOutputs [output_Delegation_UTxO]
            |> setSignatories [tpDelegationAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------

delegation_Withdraw_TxContext :: TestParams -> Integer -> LedgerApiV2.ScriptContext
delegation_Withdraw_TxContext tp !newWithdraw_MAYZ =
    let
        input_Delegation_UTxO = delegation_UTxO_MockData tp
        -----------------
        input_Delegation_Datum = DelegationT.getDelegation_DatumType_From_UTxO input_Delegation_UTxO
        input_Delegation_Value = LedgerApiV2.txOutValue input_Delegation_UTxO
        -----------------
        output_Delegation_Datum =
            Delegation.mkUpdated_Delegation_Datum_With_Deposit
                input_Delegation_Datum
                (negate newWithdraw_MAYZ)
        -----------------
        newAmount_MAYZ = DelegationT.ddDelegated_MAYZ output_Delegation_Datum
        -----------------
        output_Delegation_UTxO =
            input_Delegation_UTxO
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        DelegationT.mkDatum output_Delegation_Datum
                , LedgerApiV2.txOutValue =
                    changeValue_Amount
                        input_Delegation_Value
                        (LedgerValue.assetClass (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp))
                        newAmount_MAYZ
                }
    in
        -----------------

        mkContext
            |> setInputsRef [protocol_UTxO_MockData tp]
            |> setInputsAndAddRedeemers [(input_Delegation_UTxO, DelegationT.mkWithdrawRedeemer newWithdraw_MAYZ)]
            |> setOutputs [output_Delegation_UTxO]
            |> setSignatories [tpDelegationAdmin tp]
            |> setValidyRange (createValidRange (tpTransactionDate tp))

--------------------------------------------------------------------------------
