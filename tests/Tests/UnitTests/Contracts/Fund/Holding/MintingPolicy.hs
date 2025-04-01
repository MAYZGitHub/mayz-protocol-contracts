--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : Fund.Holding.MintingPolicy
Description : Validation logic and tests related to the FundHolding
              minting policy.

This module defines the validation logic for the FundHolding's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Fund.Holding.MintingPolicy where
--------------------------------------------------------------------------------4

-- Non-IOG imports
import           Prelude                                   (show)
import qualified Test.Tasty                                as Tasty
import qualified Test.Tasty.HUnit                          as Tasty

-- IOG imports
import qualified Plutus.V2.Ledger.Api                      as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Generic.OffChainHelpers                   as OffChainHelpers (addressValidator)
import qualified Protocol.Fund.Holding.Types               as FundHoldingT
import qualified Protocol.Fund.Types                       as FundT
import qualified Protocol.PABTypes                         as T
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.FundHolding
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

fundHolding_Policy_Tests :: TestParams -> Tasty.TestTree
fundHolding_Policy_Tests tp =
    Tasty.testGroup
        "FundHolding Policy Tests"
        [
            fundHolding_Policy_Redeemer_MintID_Tests tp,
            fundHolding_Policy_Redeemer_BurnID_Tests tp
        ]

--------------------------------------------------------------------------------

fundHolding_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
fundHolding_Policy_Redeemer_MintID_Tests tp =
   let
        ------------------------
        txName = show FundHolding_Create_Tx
        selectedRedeemer = RedeemerLogPolicy (Just FundHolding_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fundHolding_Create_TxContext tp
                in
                    [
                        Tasty.testCase "Minting ID correctly must succeed" $ do
                        let
                            ctx' = ctx
                        results <- testContextWrapper tp ctx'
                        (Nothing, results)
                            `assertResultsContainAnyOf` []
                        , Tasty.testCase "Not including Fund UTxO input must fail" $ do
                            let
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers []
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["Expected exactly one Fund input"]
                        , Tasty.testCase "Not including FundHolding UTxO output must fail" $ do
                            let
                                ctx' = ctx
                                        |> setOutputs []
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["Expected at least two outputs to script addresses"]
                        , Tasty.testCase "Not setting valid Redeemer for consuming Fund Datum must fail" $ do
                            let
                                ctx' = ctx
                                        |> setInputsAndAddRedeemers [(fund_UTxO_MockData tp, FundT.mkDatumUpdateRedeemer)]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Redeemer_Fund"]
                        , Tasty.testCase "Including a wrong FundHolding Datum must fail" $ do
                            let
                                ctx' = ctx
                                        |> setOutputs [fund_UTxO_With_Added_FundHolding_MockData tp, wrongUTxO]
                                wrongDatum =
                                    FundHoldingT.mkFundHolding_DatumType
                                        10 -- hdFundHolding_Index
                                        0 -- hdSubTotal_FT_Minted_Accumulated
                                        0 -- hdSubTotal_FT_Minted
                                        22 -- hdSubTotal_FT_Commissions
                                        22 -- hdSubTotal_FT_Commissions_Total
                                        0 -- hdSubTotal_FT_Commissions_Release_PerMonth_1e6
                                        0 -- hdSubTotal_FT_Commissions_Collected_Protocol
                                        0 -- hdSubTotal_FT_Commissions_Collected_Managers
                                        0 -- hdSubTotal_FT_Commissions_Collected_Delegators
                                        minAdaFundHoldingDatum
                                wrongUTxO =
                                    (fundHolding_UTxO_With_NoDeposits_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum wrongDatum
                                        }
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Datum"]
                        , Tasty.testCase "Including a wrong FundHolding Value must fail" $ do
                            let
                                ctx' = ctx
                                        |> setOutputs [fund_UTxO_MockData tp, wrongUTxO]
                                wrongUTxO =
                                    (fundHolding_UTxO_With_NoDeposits_MockData tp)
                                        { LedgerApiV2.txOutValue = LedgerApiV2.txOutValue (fundHolding_UTxO_With_NoDeposits_MockData tp) <> toAlter_Value_Adding_SomeADA
                                        }
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_FundHolding_Value"]
                        , Tasty.testCase "Including a wrong FundHolding UTxO Address must fail" $ do
                            let
                                ctx' = ctx
                                        |> setOutputs [fund_UTxO_MockData tp, wrongUTxO]
                                wrongUTxO =
                                    (fundHolding_UTxO_With_NoDeposits_MockData tp)
                                        { LedgerApiV2.txOutAddress  = OffChainHelpers.addressValidator T.exampleValidatorHash
                                        }
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["Expected FundHolding at output index 1"]

                    ]


--------------------------------------------------------------------------------

fundHolding_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
fundHolding_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Delete_Tx
        selectedRedeemer = RedeemerLogPolicy (Just FundHolding_BurnID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
        -- NOTE: con esta forma puedo generar conextos a partir de TxSpecs
        -- txSpecs = fundHolding_Collect_Delegators_Commission_TxSpecs tp
        -- defaultTxSpecs = txSpecs txParams_Default
        -- defaultTestCaseParams = generateTestCaseParams defaultTxSpecs
        -- ctx = context_Gen tp txSpecs defaultTestCaseParams
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let

                    ctx = fundHolding_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Burning ID correctly must succeed" $ do
                        let
                            ctx' = ctx
                        results <- testContextWrapper tp ctx'
                        (Nothing, results)
                            `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------
