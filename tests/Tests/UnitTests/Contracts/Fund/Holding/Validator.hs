--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Fund.Holding.Validator
Description : Validation logic and tests related to the FundHolding module.

This module defines the validation logic for the FundHolding contract.

It includes multiple test cases to ensure the integrity and correctness of the
validation script.
-}
module Contracts.Fund.Holding.Validator where

--------------------------------------------------------------------------------4

-- Non-IOG imports
import qualified GHC.Stack as GHC
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import Prelude as P

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada as LedgerAda
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import PlutusTx.Prelude (divide)

-- Project imports

import qualified Generic.Constants as T
import qualified Protocol.Fund.Helpers as FundHelpers
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.InvestUnit.Types as InvestUnitT
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.OnChainHelpers as OnChainHelpers
import qualified Protocol.PABTypes as T
import qualified Protocol.Types as T
import TestUtils.Contracts.InitialData
import TestUtils.Contracts.TxContext.FundHolding
import TestUtils.HelpersMAYZ
import TestUtils.TestContext.Asserts
import TestUtils.TestContext.Helpers
import TestUtils.Types
import TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

fundHolding_Validator_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Tests tp =
    Tasty.testGroup
        "FundHolding Validator Tests"
        [ fundHolding_Validator_Redeemer_UpdateMinADA_Tests tp
        , fundHolding_Validator_Redeemer_Deposit_Tests tp
        , fundHolding_Validator_Redeemer_Withdraw_Tests tp
        , fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests tp
        , fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests tp
        , fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests tp
        , fundHolding_Validator_Redeemer_ReIndexing_Tests tp
        , fundHolding_Validator_Redeemer_BalanceAssets_Tests tp
        , fundHolding_Validator_Redeemer_Delete_Tests tp
        ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_UpdateMinADA_Tests tp =
    let
        ------------------------
        txName = show FundHolding_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_UpdateMinADA_TxContext tp toAlter_minAda
            in
                [ Tasty.testCase "Changing min ADA correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Deposit_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Deposit_Tests tp =
    let
        ------------------------
        txName = show Fund_Deposit_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Deposit_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Deposit_TxContext tp (tpDepositDate tp) deposit_MockData
            in
                [ Tasty.testCase "Depositing correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Depositing zero must fail" $ do
                    let
                        ctx' = fundHolding_Deposit_TxContext tp (tpDepositDate tp) 0
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not Correct Deposit Amount"]
                , Tasty.testCase "Depositing negative must fail" $ do
                    let
                        ctx' = fundHolding_Deposit_TxContext tp (tpDepositDate tp) (-10)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Deposit cannot be negative"]
                , Tasty.testCase "Depositing without minting FT must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers mempty
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingFT"]
                , Tasty.testCase "Depositing without increasing FT minted subTotal must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubTotal_FT_Minted =
                                    FundHoldingT.hdSubTotal_FT_Minted (fundHolding_DatumType_With_NoDeposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Deposits_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output datum"]
                , Tasty.testCase "Depositing without paying invest units must fail" $ do
                    let
                        (_, commissionsFT_MockData, _) = calculateDepositCommissionsUsingMonths_ tp (tpDepositDate tp) deposit_MockData
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Deposits_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaFundHoldingDatum
                                        <> LedgerApiV2.singleton (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN 0) 1
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionsFT_MockData
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output value"]
                , Tasty.testCase "Depositing date outside valid range must fail" $ do
                    let
                        -- valid range for tx is created with tpDepositDate as date, and then subs and adds hald of valid time
                        -- (date'  - LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTxTimeRange `divide` 2) + 1) (date' + LedgerApiV2.POSIXTime (LedgerApiV2.getPOSIXTime T.validTxTimeRange `divide` 2) -1)
                        -- so if we set in redeemer the date (tpDepositDate tp+T.validTxTimeRange ) it must fail
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_NoDeposits_MockData tp, FundHoldingT.mkDepositRedeemer (tpDepositDate tp + T.validTxTimeRange) deposit_MockData)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong date"]
                , Tasty.testCase "Depositing with invalid range must fail" $ do
                    let
                        ctx' = ctx |> setValidyRange (createInValidRange (tpDepositDate tp))
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isValidRange"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Withdraw_Tests :: GHC.HasCallStack => TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Withdraw_Tests tp =
    let
        ------------------------
        txName = show Fund_Withdraw_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Withdraw_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Withdraw_TxContext tp (tpDepositDate tp) deposit_MockData (tpWithdrawDate tp) withdraw_MockData
            in
                [ Tasty.testCase "Withdrawing correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Withdrawing zero must fail" $ do
                    let
                        ctx' = fundHolding_Withdraw_TxContext tp (tpDepositDate tp) deposit_MockData (tpWithdrawDate tp) 0
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not Correct Withdraw Amount"]
                , Tasty.testCase "Withdrawing negative must fail" $ do
                    let
                        ctx' = fundHolding_Withdraw_TxContext tp (tpDepositDate tp) deposit_MockData (tpWithdrawDate tp) (-10)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Withdraw cannot be negative"]
                , Tasty.testCase "Withdrawing without burning FT must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers mempty
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningFT"]
                , Tasty.testCase "Withdrawing without updating FT minted subTotal must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Withdraw_MockData tp)
                                { FundHoldingT.hdSubTotal_FT_Minted =
                                    FundHoldingT.hdSubTotal_FT_Minted (fundHolding_DatumType_With_Deposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Withdraw_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output datum"]
                , Tasty.testCase "Withdrawing without user recovering commissions must fail" $ do
                    let
                        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)
                        (_, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (tpWithdrawDate tp) withdraw_MockData investUnit_Granularity
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Withdraw_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                                        -- <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-commissionsFTToGetBack_MockData)
                                        <> LedgerApiV2.singleton investUnit_Initial_Token_CS investUnit_Initial_Token_TN (-((withdrawPlusCommissionsGetBack_MockData * investUnit_Initial_Token_Amount) `divide` 100))
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output value"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Collect_Protocol_Commission_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Protocol_Commission_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Collect_Protocol_Commission_TxContext tp
            in
                [ Tasty.testCase "Collecting commissions correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Collecting commissions without updating remaining commissions must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubTotal_FT_Commissions =
                                    FundHoldingT.hdSubTotal_FT_Commissions (fundHolding_DatumType_With_Deposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output datum"]
                , Tasty.testCase "Collecting commissions without updating FundHolding value must fail" $ do
                    let
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutValue =
                                    LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output value"]
                , Tasty.testCase "Trying to collect more commissions than the available must fail" $ do
                    let
                        available = FundHelpers.getCommissionsAvailable (tpDeadline tp) (fundHolding_DatumType_With_Deposits_MockData tp) (tpShare_InBPx1e2_Protocol tp) (FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol (fundHolding_DatumType_With_Deposits_MockData tp)) (tpCollectCommissionsDate tp)
                        withdraw_Commissions_MockData' = available + 1
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkCollect_Protocol_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData')]
                                |> setOutputs [fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCommissionsAvailable"]
                , Tasty.testCase "Trying to collect commissions without adding admin signatory must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setSignatories []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                , Tasty.testCase "Collecting commissions outside valid range must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setValidyRange (Ledger.interval 0 300_000)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong date"]
                ]

fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Collect_Managers_Commission_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Managers_Commission_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Collect_Managers_Commission_TxContext tp
            in
                [ Tasty.testCase "Collecting commissions correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Collecting commissions without updating remaining commissions must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubTotal_FT_Commissions =
                                    FundHoldingT.hdSubTotal_FT_Commissions (fundHolding_DatumType_With_Deposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output datum"]
                , Tasty.testCase "Collecting commissions without updating FundHolding value must fail" $ do
                    let
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaFundHoldingDatum
                                        <> LedgerApiV2.singleton T.exampleCS T.exampleTN 300
                                        <> LedgerApiV2.singleton (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN 0) 1
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) 200
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output value"]
                , Tasty.testCase "Trying to collect more commissions than the available must fail" $ do
                    let
                        available = FundHelpers.getCommissionsAvailable (tpDeadline tp) (fundHolding_DatumType_With_Deposits_MockData tp) (tpShare_InBPx1e2_Managers tp) (FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers (fundHolding_DatumType_With_Deposits_MockData tp)) (tpCollectCommissionsDate tp)
                        withdraw_Commissions_MockData' = available + 1
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkCollect_Managers_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData')]
                                |> setOutputs [fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCommissionsAvailable"]
                , Tasty.testCase "Trying to collect commissions without adding admin signatory must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setSignatories []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                , Tasty.testCase "Collecting commissions outside valid range must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setValidyRange (Ledger.interval 0 300_000)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong date"]
                ]

fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Collect_Delegators_Commission_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Delegators_Commission_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Collect_Delegators_Commission_TxContext tp
            in
                [ Tasty.testCase "Collecting commissions correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Collecting commissions without updating remaining commissions must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubTotal_FT_Commissions =
                                    FundHoldingT.hdSubTotal_FT_Commissions (fundHolding_DatumType_With_Deposits_MockData tp)
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output datum"]
                , Tasty.testCase "Collecting commissions without updating FundHolding value must fail" $ do
                    let
                        modifiedUTxO =
                            (fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData)
                                { LedgerApiV2.txOutValue =
                                    LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output value"]
                , Tasty.testCase "Trying to collect more commissions than the available must fail" $ do
                    let
                        available = FundHelpers.getCommissionsAvailable (tpDeadline tp) (fundHolding_DatumType_With_Deposits_MockData tp) (tpShare_InBPx1e2_Delegators tp) (FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators (fundHolding_DatumType_With_Deposits_MockData tp)) (tpCollectCommissionsDate tp)
                        withdraw_Commissions_MockData' = available + 1
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkCollect_Delegators_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw_Commissions_MockData')]
                                |> setOutputs [fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCommissionsAvailable"]
                , Tasty.testCase "Trying to collect commissions without adding admin signatory must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setSignatories []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isSignedByAny delegatorsAdmins"]
                , Tasty.testCase "Collecting commissions outside valid range must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setValidyRange (Ledger.interval 0 300_000)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong date"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_ReIndexing_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_ReIndexing_Tests tp =
    let
        ------------------------
        txName = show Fund_ReIndexing_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_ReIndexing_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_ReIndexing_TxContext tp
            in
                [ Tasty.testCase "Re-index correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Not including protocol input ref must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsRef []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected exactly one Fund input ref"]
                , Tasty.testCase "Not including some FundHolding as ref must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsRef [fund_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["expected all but one FundHolding as input ref"]
                , Tasty.testCase "Not including invest unit input must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers
                                    [ (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial)
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected exactly one InvestUnit input"]
                , Tasty.testCase "Not having a FundHolding output must fail" $ do
                    let
                        -- NOTE: pongo a la fuerza cualquier output, para que no falle por no tener outputs
                        ctx' =
                            ctx
                                |> setOutputs [protocol_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected FundHolding at output index 0"]
                , Tasty.testCase "Updating FundHolding must fail" $ do
                    let
                        modifiedDatumType =
                            (fundHolding_DatumType_With_Deposits_MockData tp)
                                { FundHoldingT.hdSubTotal_FT_Commissions =
                                    FundHoldingT.hdSubTotal_FT_Commissions (fundHolding_DatumType_With_Deposits_MockData tp) + 1
                                }
                        modifiedUTxO =
                            (fundHolding_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum modifiedDatumType
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output datum"]
                , Tasty.testCase "Not updating the FundHolding value must fail" $ do
                    let
                        modifiedUTxO =
                            (fundHolding_UTxO_After_Reidx_MockData tp investUnit_Initial investUnit_AfterReIdx)
                                { LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [modifiedUTxO]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong output value"]
                , Tasty.testCase "Having an incorrect redeemer for the Invest Unit validator must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers
                                    [ (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkReIndexingRedeemer investUnit_AfterReIdx investUnit_Initial)
                                    , (investUnit_UTxO_MockData tp, InvestUnitT.mkReIndexingRedeemer (T.InvestUnit [("cc", "name", 0)]) (T.InvestUnit [("cc", "name", 0)]) (oracleReIdxData tp) (oracleReIdxSignature tp))
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Redeemer_InvestUnit"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_BalanceAssets_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_BalanceAssets_Tests tp =
    let
        ------------------------
        txName = show FundHolding_BalanceAssets_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_BalanceAssets_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        ------------------------
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [0, 0] False []
            in
                [ Tasty.testCase "Balancing assets correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Balancing assets incorrect total must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -99] [0, 0] False []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong outputs values"]
                , -- , Tasty.testCase "Balancing assets from fst input with zero deposits to other correctly must fail" $ do
                  --     -- para el calculo y control del movimiento de comisiones, se necesita un deposito previo en la primer input
                  --     -- es una restriccion de simplificacion y siempre se puede armar la misma tx usando la primer input como segunda y viceversa
                  --     let
                  --         ctx' = fundHolding_BalanceAssets_TxContext tp [0, deposit_MockData] [100, -100] [0, 0] False []
                  --     results <- testContextWrapper tp ctx'
                  --     (Just selectedRedeemer, results)
                  --         `assertResultsContainAnyOf` ["Expected oldCommissions firts input to be greater than 0"]
                  Tasty.testCase "Balancing assets from fst input with deposits to other with zero correctly must succeed" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, 0] [-100, 100] [0, 0] False []
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Balancing commissions FT correctly must succeed" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [0, 0] [1000000, -1000000] False []
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Balancing deposits and commissions FT neg snd correctly release must succeed" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000000, -1000000] False []
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Balancing deposits and commissions FT neg fst correctly release must succeed" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [-1000000, 1000000] False []
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Balancing deposits and commissions FT using correct custom release per month must succeed" $ do
                    let
                        -- con deposit_MockData se crea un datum con
                        -- comission FT = 1991200
                        -- rate = 221244444444
                        -- con un cambio de [1000000, -1000000]
                        -- deberia generarse un cambio [332355555554, 110133333334]
                        -- eso lo saco del caso de testeo anterior, usando testContextWrapperTrace y leyendo el contexto. me fijo en los datums in y out, y aca pongo la dif
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000000, -1000000] True [332355555554, 110133333334]
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Balancing ALL deposits and ALL commissions FT correctly release must succeed" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, 0] [-deposit_MockData, deposit_MockData] [-1991200, 1991200] False []
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , -- TODO: deberia fallar pero no lo hace por que mi test context no verifica que ningun valor en value sea menor a cero.
                  -- deberia agregar ese control en el test context
                  -- , Tasty.testCase "Balancing a LITLE MORE deposits and ALL commissions FT correctly release must fail" $ do
                  --     let
                  --         ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, 0] [-deposit_MockData - 100, deposit_MockData + 100] [-1991200, 1991200] False []
                  --     results <- testContextWrapperTrace tp ctx'
                  --     (Nothing, results)
                  --         `assertResultsContainAnyOf` []
                  Tasty.testCase "Balancing a ALL deposits and LITLE MORE commissions FT release must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, 0] [-deposit_MockData, deposit_MockData] [-1991200 - 1, 1991200 + 1] False []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong datums values"]
                ,  Tasty.testCase "Balancing a ALL deposits and commissions FT from non deposits utxo release must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, 0] [-deposit_MockData, deposit_MockData] [100, -100] False []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected Commissions to take bigger than 0"]
                ,  Tasty.testCase "Balancing a ALL deposits and commissions FT from non deposits utxo release must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, 1] [-deposit_MockData, deposit_MockData] [100, -100] False []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong datums values"]
                 ,  Tasty.testCase "Balancing a ALL deposits and commissions FT from non deposits utxo release must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, 100] [-deposit_MockData, deposit_MockData] [100, -100] False []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong datums values"]
                 ,  Tasty.testCase "Balancing a ALL deposits and commissions FT from deposits utxo release must succeed" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [0, deposit_MockData] [deposit_MockData, -deposit_MockData] [100, -100] False []
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Balancing deposits and commissions FT using incorrect custom release 1 per month must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000000, -1000000] True [332355555553, 110133333334]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong outputs datums"]
                , Tasty.testCase "Balancing deposits and commissions FT using incorrect custom release 2 per month must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000000, -1000000] True [332355555554, 110133333333]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong outputs datums"]
                , Tasty.testCase "Balancing deposits and commissions FT using incorrect custom release 3 per month must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [100, -100] [1000001, -1000001] True [332355555554, 110133333334]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong outputs datums"]
                , Tasty.testCase "Incorrect redeemer with more items in commissions list must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [0, 0, 0] False []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong inputs, outputs, redeemers len"]
                , Tasty.testCase "Incorrect redeemer with less items in commissions list invalid must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [0] False []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong inputs, outputs, redeemers len"]
                , Tasty.testCase "Incorrect redeemer with sum total of commision not zero (plus zero) must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [1, 0] False []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong redeemers move FT total"]
                , Tasty.testCase "Incorrect redeemer with sum total of commision not zero (less zero) must fail" $ do
                    let
                        ctx' = fundHolding_BalanceAssets_TxContext tp [deposit_MockData, deposit_MockData] [deposit_MockData, deposit_MockData] [-1, 0] False []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["wrong redeemers move FT total"]
                ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Delete_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fundHolding_Delete_TxContext tp
            in
                [ Tasty.testCase "Delete FundHolding correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Not empty FundHolding value must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers
                                    [ (fund_UTxO_With_Added_FundHolding_MockData tp, FundT.mkFundHoldingDeleteRedeemer)
                                    , (fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkDeleteRedeemer)
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just (RedeemerLogPolicy (Just FundHolding_BurnID_TestRedeemer)), results)
                        `assertResultsContainAnyOf` ["not isZeroAssets"]
                , Tasty.testCase "Not including fund admin sign must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setSignatories []
                    results <- testContextWrapper tp ctx'
                    (Just (RedeemerLogValidator (Just Fund_FundHoldingDelete_TestRedeemer)), results)
                        `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]
                , Tasty.testCase "Not including Fund UTXO as input must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_NoDeposits_MockData tp, FundHoldingT.mkDeleteRedeemer)]
                    results <- testContextWrapper tp ctx'
                    (Just (RedeemerLogPolicy (Just FundHolding_BurnID_TestRedeemer)), results)
                        `assertResultsContainAnyOf` ["Expected exactly one Fund input"]
                , Tasty.testCase "Not including Fund UTXO as output must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setOutputs [protocol_UTxO_MockData tp]
                    -- NOTE: necesito agregar alguna output para que no falle por no tener outputs
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected Fund at output index 0"]
                , Tasty.testCase "Not burning FundHolding ID must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers mempty
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningFundHoldingID"]
                , Tasty.testCase "Too big range must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setValidyRange (createInValidRange (tpTransactionDate tp))
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isValidRange"]
                ]

--------------------------------------------------------------------------------
