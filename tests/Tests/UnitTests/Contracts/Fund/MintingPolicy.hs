--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Fund.MintingPolicy
Description : Validation logic and tests related to the Fund minting policy.

This module defines the validation logic for the Fund's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Fund.MintingPolicy where

--------------------------------------------------------------------------------4

-- Non-IOG imports

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import Prelude (show)

-- IOG imports
import qualified Ledger.Ada as LedgerAda
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import PlutusTx.Prelude as PTx

-- Project imports
import qualified Generic.OffChainHelpers as OffChainHelpers
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Constants as T
import qualified Protocol.Fund.Helpers as FundHelpers
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.OnChainHelpers as OnChainHelpers
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.Types as T
import TestUtils.Constants
import TestUtils.Contracts.InitialData
import TestUtils.Contracts.TxContext.Fund
import TestUtils.HelpersMAYZ
import TestUtils.TestContext.Asserts
import TestUtils.TestContext.Helpers
import TestUtils.Types
import TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

fund_Policy_Tests :: TestParams -> Tasty.TestTree
fund_Policy_Tests tp =
    Tasty.testGroup
        "Fund Policy Tests"
        [ fund_Policy_Redeemer_MintID_Tests tp
        , fund_Policy_Redeemer_BurnID_Tests tp
        , fund_Policy_Redeemer_MintFT_Tests tp
        , fund_Policy_Redeemer_BurnFT_Tests tp
        ]

--------------------------------------------------------------------------------

fund_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
fund_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = show Fund_Create_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Fund_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fund_Create_TxContext tp
            in
                [ Tasty.testCase "Minting ID correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Not including Protocol input ref must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsRef []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected exactly one Protocol input ref"]
                , Tasty.testCase "Not including two output must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected at least two outputs to script addresses"]
                , Tasty.testCase "Not including fund output must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setOutputs [investUnit_UTxO_MockData tp, investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected Fund at output index 0"]
                , Tasty.testCase "Not including invest unit output must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData tp, fund_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected InvestUnit at output index 1"]
                , Tasty.testCase "Not including utxo in policy parameter as input must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isTxOutAnInput"]
                , Tasty.testCase "Mintin IU ID, but not minting Fund ID must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1, FundT.mkMintIDRedeemer)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingIDs"]
                , Tasty.testCase "Minting Fund ID, but not minting IU ID must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1, FundT.mkMintIDRedeemer)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingIDs"]
                , Tasty.testCase "Minting a different amount IU ID must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 2
                                        , FundT.mkMintIDRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingIDs"]
                , Tasty.testCase "Minting a different amount Fund ID must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 2
                                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1
                                        , FundT.mkMintIDRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingIDs"]
                , Tasty.testCase "Minting an extra asset with wrong token name must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1
                                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
                                        , FundT.mkMintIDRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingIDs"]
                , Tasty.testCase "Minting an asset with wrong token name must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton (tpFundPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
                                        , FundT.mkMintIDRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingIDs"]
                , Tasty.testCase "Fund utxo address different from the one in policy param must fail" $ do
                    let
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutAddress =
                                    OffChainHelpers.addressValidator $
                                        LedgerApiV2.ValidatorHash
                                            "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c90000"
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected Fund at output index 0"]
                , Tasty.testCase "IU utxo address different from the one in policy param must fail" $ do
                    let
                        investUnit_UTxO_MockData' =
                            (investUnit_UTxO_MockData tp)
                                { LedgerApiV2.txOutAddress =
                                    OffChainHelpers.addressValidator $
                                        LedgerApiV2.ValidatorHash
                                            "d5dec6074942b36b50975294fd801f7f28c907476b1ecc1b57c90000"
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData tp, investUnit_UTxO_MockData']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected InvestUnit at output index 1"]
                , Tasty.testCase "Fund with wrong Commissions in datum must fail" $ do
                    let
                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdCommission_PerYear_InBPx1e3 = ProtocolT.mmdMax (ProtocolT.pdCommissionFund_PerYear_InBPx1e3 (protocol_DatumType_MockData tp)) + sum_ANY_INVALID_NUMBER
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isInRange commissionFund_PerYear_InBPx1e3"]
                , -- , Tasty.testCase "Fund with wrong Commissions Table values in datum must fail" $ do
                  --     let beginAt = FundT.fdBeginAt (fund_DatumType_MockData tp)
                  --         deadline = FundT.fdDeadline (fund_DatumType_MockData tp)
                  --         commission_PerYear_InBPx1e3 = FundT.fdCommission_PerYear_InBPx1e3 (fund_DatumType_MockData tp) + sum_ANY_INVALID_NUMBER
                  --         monthsRemainingPlusOne = FundHelpers.getRemainingMonths deadline beginAt + 1
                  --         den = 120_000_000
                  --         commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne]]
                  --         ----------------------
                  --         fund_DatumType' =
                  --             (fund_DatumType_MockData tp)
                  --                 { FundT.fdCommissions_Table_Numerator_1e6 = commissions_Table_Numerator_1e6
                  --                 }
                  --         fund_UTxO_MockData' =
                  --             (fund_UTxO_MockData tp)
                  --                 { LedgerApiV2.txOutDatum =
                  --                     LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                  --                 }
                  --         ctx' = ctx
                  --                 |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                  --     results <- testContextWrapper tp ctx'
                  --     (Just selectedRedeemer, results)
                  --         `assertResultsContainAnyOf` ["not isCorrect_Output_CommissionsTable"]
                  Tasty.testCase "Fund with wrong Commissions Table lenght in datum must fail" $ do
                    let
                        ----------------------
                        beginAt = FundT.fdBeginAt (fund_DatumType_MockData tp)
                        deadline = FundT.fdDeadline (fund_DatumType_MockData tp)
                        commission_PerYear_InBPx1e3 = FundT.fdCommission_PerYear_InBPx1e3 (fund_DatumType_MockData tp)
                        monthsRemainingPlusOne_ = FundHelpers.getRemainingMonths deadline beginAt + 1
                        den = 120_000_000
                        commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne_]]
                        ----------------------
                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdCommissions_Table_Numerator_1e6 = sum_ANY_INVALID_NUMBER : commissions_Table_Numerator_1e6
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_CommissionsTable"]
                , Tasty.testCase "Fund with wrong Category in datum must fail" $ do
                    let
                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdFundCategoryNumber = ProtocolT.fcCategoryNumber (tpFundCategory tp) + sum_ANY_INVALID_NUMBER
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Can't find Fund Category"]
                , Tasty.testCase "Having initial holdings count different from 0 must fail" $ do
                    let
                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdHoldingsCount = 1
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Datum"]
                , Tasty.testCase "Having initial wrong deadline must fail" $ do
                    let
                        ----------------------
                        beginAt = FundT.fdBeginAt (fund_DatumType_MockData tp)
                        deadline = tpTransactionDate tp - LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                        commission_PerYear_InBPx1e3 = FundT.fdCommission_PerYear_InBPx1e3 (fund_DatumType_MockData tp)
                        monthsRemainingPlusOne_ = FundHelpers.getRemainingMonths deadline beginAt + 1
                        den = 120_000_000
                        commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne_]]
                        ----------------------
                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdDeadline = deadline
                                , FundT.fdCommissions_Table_Numerator_1e6 = commissions_Table_Numerator_1e6
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isDateNotReached deadline"]
                , Tasty.testCase "Having initial deadline < beginAt must fail" $ do
                    let
                        ----------------------
                        beginAt = tpTransactionDate tp + 100
                        deadline = beginAt - 1
                        commission_PerYear_InBPx1e3 = FundT.fdCommission_PerYear_InBPx1e3 (fund_DatumType_MockData tp)
                        monthsRemainingPlusOne_ = FundHelpers.getRemainingMonths deadline beginAt + 1
                        den = 120_000_000
                        commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne_]]
                        ----------------------
                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdBeginAt = beginAt
                                , FundT.fdDeadline = deadline
                                , FundT.fdCommissions_Table_Numerator_1e6 = commissions_Table_Numerator_1e6
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not deadline > beginAt"]
                , Tasty.testCase "Having initial wrong fund lifetime must fail" $ do
                    let
                        fundLifeTime = ProtocolT.mmdMax $ ProtocolT.pdFundLifeTime $ protocol_DatumType_MockData tp
                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdDeadline = FundT.fdBeginAt (fund_DatumType_MockData tp) + fundLifeTime + LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isInRange fundLifeTime"]
                , Tasty.testCase "Fund with wrong MAYZ in datum must fail" $ do
                    let
                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdRequiredMAYZ = ProtocolT.fcRequiredMAYZ (tpFundCategory tp) + sum_ANY_INVALID_NUMBER
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Datum"]
                , Tasty.testCase "MAYZ in fund output value not according to FundCategory of datum must fail" $ do
                    let
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaFundDatum
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                                        <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ (tpFundCategory tp) + sum_ANY_INVALID_NUMBER)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not currentMAYZ == requiredMAYZ"]
                , Tasty.testCase "Fund output value without Fund ID must fail" $ do
                    let
                        fund_UTxO_MockData'' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaFundDatum
                                        <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData'', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected Fund at output index 0"]
                , Tasty.testCase "Fund output value with Fund ID other token name must fail" $ do
                    let
                        fund_UTxO_MockData'' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaFundDatum
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) "OTHER NAME" 1
                                        <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData'', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected Fund at output index 0"]
                , Tasty.testCase "Fund output value without minAda specified in datum must fail" $ do
                    let
                        fund_UTxO_MockData'' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                                        <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData'', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Value"]
                , Tasty.testCase "Fund output value with extra tokens must fail" $ do
                    let
                        fund_UTxO_MockData'' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaFundDatum
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                                        <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) "ExtraToken" 1
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData'', investUnit_UTxO_MockData tp]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_Fund_Value"]
                , Tasty.testCase "IU output value without IU ID must fail" $ do
                    let
                        investUnitUTxO'' =
                            (investUnit_UTxO_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaIUDatum
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData tp, investUnitUTxO'']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected InvestUnit at output index 1"]
                , Tasty.testCase "IU output value with IU ID other token name must fail" $ do
                    let
                        investUnitUTxO'' =
                            (investUnit_UTxO_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaIUDatum
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) "OTHER NAME" 1
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData tp, investUnitUTxO'']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected InvestUnit at output index 1"]
                , Tasty.testCase "IU output value without minAda specified in datum must fail" $ do
                    let
                        investUnitUTxO'' =
                            (investUnit_UTxO_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData tp, investUnitUTxO'']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_InvestUnit_Value"]
                , Tasty.testCase "IU output value with extra tokens must fail" $ do
                    let
                        investUnitUTxO'' =
                            (investUnit_UTxO_MockData tp)
                                { LedgerApiV2.txOutValue =
                                    LedgerAda.lovelaceValueOf minAdaIUDatum
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1
                                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) "ExtraToken" 1
                                }
                        ctx' =
                            ctx
                                |> setOutputs [fund_UTxO_MockData tp, investUnitUTxO'']
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isCorrect_Output_InvestUnit_Value"]
                ]

--------------------------------------------------------------------------------

fund_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
fund_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = show Fund_Delete_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Fund_BurnID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fund_Delete_TxContext tp
            in
                [ Tasty.testCase "Burning ID correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Burning IU ID, but not burning fund ID must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton
                                            (tpFundPolicy_CS tp)
                                            T.investUnitID_TN
                                            (negate 1)
                                        , FundT.mkBurnIDRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningIDs"]
                , Tasty.testCase "Burning Fund ID but not burning invest unit ID must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton
                                            (tpFundPolicy_CS tp)
                                            T.fundID_TN
                                            (negate 1)
                                        , FundT.mkBurnIDRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningIDs"]
                ]

--------------------------------------------------------------------------------

fund_Policy_Redeemer_MintFT_Tests :: TestParams -> Tasty.TestTree
fund_Policy_Redeemer_MintFT_Tests tp =
    let
        ------------------------
        txName = show Fund_Deposit_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Fund_MintFT_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fund_Deposit_TxContext tp (tpDepositDate tp) deposit_MockData
            in
                [ Tasty.testCase "Minting FT correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Not having Fund UTxO as ref input must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsRef []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected exactly one Fund input ref"]
                , Tasty.testCase "Not having FundHolding UTxO as input must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected exactly one FundHolding input"]
                , Tasty.testCase "Incorrect redeemer for FundHolding UTxO must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_NoDeposits_MockData tp, FundHoldingT.mkWithdrawRedeemer 10 20 10)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected FundHolding ValidatorRedeemerDeposit"]
                , Tasty.testCase "Minting another token name must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton (tpFundPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
                                        , FundT.mkMintFTRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingFT"]
                , Tasty.testCase "Minting extra token must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) deposit_MockData
                                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
                                        , FundT.mkMintFTRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingFT"]
                , Tasty.testCase "Minting more FT must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (deposit_MockData + sum_ANY_INVALID_NUMBER), FundT.mkMintFTRedeemer)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingFT"]
                , Tasty.testCase "Minting less FT must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) 1, FundT.mkMintFTRedeemer)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isMintingFT"]
                , Tasty.testCase "Minting FT with Fund before beginAt must fail" $ do
                    let
                        before = FundT.fdBeginAt (fund_DatumType_MockData tp) - LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                        ctx' =
                            ctx
                                |> setValidyRange (createValidRange before)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isFundOpen"]
                , Tasty.testCase "Minting FT with Fund after deadline must fail" $ do
                    let
                        after = FundT.fdDeadline (fund_DatumType_MockData tp) + LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                        ctx' =
                            ctx
                                |> setValidyRange (createValidRange after)
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isFundOpen"]
                , Tasty.testCase "Minting FT with Closed Fund must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsRef [fund_UTxO_MockData', investUnit_UTxO_MockData tp]

                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdClosedAt = Just $ tpDepositDate tp - LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isFundOpen"]
                ]

--------------------------------------------------------------------------------

fund_Policy_Redeemer_BurnFT_Tests :: TestParams -> Tasty.TestTree
fund_Policy_Redeemer_BurnFT_Tests tp =
    let
        ------------------------
        txName = show Fund_Withdraw_Tx
        selectedRedeemer = RedeemerLogPolicy (Just Fund_MintFT_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
    in
        ------------------------

        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
            let
                ctx = fund_Withdraw_TxContext tp (tpDepositDate tp) deposit_MockData (tpWithdrawDate tp) withdraw_MockData
            in
                [ Tasty.testCase "Burning FT correctly must succeed" $ do
                    let
                        ctx' = ctx
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Not having Fund UTxO as ref input must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsRef []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected exactly one Fund input ref"]
                , Tasty.testCase "Not having FundHolding UTxO as input must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers []
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected exactly one FundHolding input"]
                , Tasty.testCase "Incorrect redeemer for FundHolding UTxO must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsAndAddRedeemers [(fundHolding_UTxO_With_Deposits_MockData tp, FundHoldingT.mkDepositRedeemer 10 20)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["Expected FundHolding ValidatorRedeemerWithdraw"]
                , Tasty.testCase "Burning another token name must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton (tpFundPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") (-1)
                                        , FundT.mkBurnFTRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningFT"]
                , Tasty.testCase "Burning extra token must fail" $ do
                    let
                        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)
                        (_, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (tpWithdrawDate tp) withdraw_MockData investUnit_Granularity
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers
                                    [
                                        ( LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-withdrawPlusCommissionsGetBack_MockData)
                                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (LedgerApiV2.TokenName "ANOTHER NAME") 1
                                        , FundT.mkBurnFTRedeemer
                                        )
                                    ]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningFT"]
                , Tasty.testCase "Burning more FT must fail" $ do
                    let
                        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)
                        (_, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (tpWithdrawDate tp) withdraw_MockData investUnit_Granularity
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-(withdrawPlusCommissionsGetBack_MockData + sum_ANY_INVALID_NUMBER)), FundT.mkBurnFTRedeemer)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningFT"]
                , Tasty.testCase "Burning less FT must fail" $ do
                    let
                        ctx' =
                            ctx
                                |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-1), FundT.mkBurnFTRedeemer)]
                    results <- testContextWrapper tp ctx'
                    (Just selectedRedeemer, results)
                        `assertResultsContainAnyOf` ["not isBurningFT"]
                -- , Tasty.testCase "Burning FT with Fund before beginAt must fail" $ do
                --     let
                --         before = FundT.fdBeginAt (fund_DatumType_MockData tp) - LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                --         ctx' =  fund_Withdraw_TxContext tp before deposit_MockData (before+1) withdraw_MockData
                --             -- ctx
                --             --     |> setValidyRange (createValidRange before)
                --     results <- testContextWrapper tp ctx'
                --     (Just selectedRedeemer, results)
                --         `assertResultsContainAnyOf` ["not isFundOpen"]
                , Tasty.testCase "Burning FT with Fund after deadline must succeed" $ do
                    let
                        after = FundT.fdDeadline (fund_DatumType_MockData tp) + LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                        ctx' =  fund_Withdraw_TxContext tp (tpDepositDate tp) deposit_MockData after withdraw_MockData
                            -- ctx
                            --     |> setValidyRange (createValidRange after)
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                , Tasty.testCase "Burning FT with Closed Fund must succeed" $ do
                    let
                        ctx' =
                            ctx
                                |> setInputsRef [fund_UTxO_MockData', investUnit_UTxO_MockData tp]

                        fund_DatumType' =
                            (fund_DatumType_MockData tp)
                                { FundT.fdClosedAt = Just $ tpWithdrawDate tp - LedgerApiV2.POSIXTime sum_ANY_INVALID_NUMBER
                                }
                        fund_UTxO_MockData' =
                            (fund_UTxO_MockData tp)
                                { LedgerApiV2.txOutDatum =
                                    LedgerApiV2.OutputDatum $ FundT.mkDatum fund_DatumType'
                                }
                    results <- testContextWrapper tp ctx'
                    (Nothing, results)
                        `assertResultsContainAnyOf` []
                ]

--------------------------------------------------------------------------------
