--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

{- |
Module      : Tests.PropertyBasedTests.Fund.Holding.Validator
Description : Validation logic and property-based tests related to the Protocol
              validator.

This module defines the validation logic for the Protocol's contract.

It includes multiple property-based test cases to ensure the integrity and
correctness of the validator script.
-}
module Contracts.Fund.Holding.Validator where

--------------------------------------------------------------------------------3

-- Non-IOG imports
import qualified Control.Monad.Reader                      as MReader
import           Prelude                                   as P hiding (length, negate)
import qualified Test.QuickCheck                           as QC
import qualified Test.Tasty                                as Tasty
import qualified Test.Tasty.QuickCheck                     as TastyQC

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada                                as LedgerAda
import qualified Ledger.Address                            as LedgerAddress
import qualified Plutus.V2.Ledger.Api                      as LedgerApiV2
import           PlutusTx.Prelude                          (divide, negate)

-- Project imports
import qualified Generic.OnChainHelpers                    as OnChainHelpers
import qualified Protocol.Fund.Helpers                     as FundHelpers
import qualified Protocol.Fund.Holding.Types               as FundHoldingT
import qualified Protocol.Fund.Types                       as FundT
import qualified Protocol.Fund.InvestUnit.Types                 as InvestUnitT
import qualified Protocol.OffChainHelpers                  as OffChainHelpers
import qualified Protocol.OnChainHelpers                   as OnChainHelpers
import qualified Protocol.Types                            as T
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.FundHolding
import           TestUtils.Helpers
import           TestUtils.HelpersMAYZ
import           TestUtils.QuickCheckGen.QuickCheckGen
import           TestUtils.QuickCheckGen.QuickCheckGenMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------3

fundHolding_Validator_Tests :: AppM Tasty.TestTree
fundHolding_Validator_Tests = do
    tp <- MReader.asks appTestParams
    return $
        Tasty.testGroup
        "FundHolding Validator Tests"
        [
            fundHolding_Validator_Redeemer_UpdateMinADA_Tests tp
            , fundHolding_Validator_Redeemer_Deposit_Tests tp
            , fundHolding_Validator_Redeemer_Withdraw_Tests tp
            , fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests tp
            , fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests tp
            , fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests tp
            , fundHolding_Validator_Redeemer_ReIndexing_Tests tp
            , fundHolding_Validator_Redeemer_BalanceAssets_Tests tp
            , fundHolding_Validator_Redeemer_Delete_Tests tp
        ]

--------------------------------------------------------------------------------3

fundHolding_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_UpdateMinADA_Tests tp =
    let ------------------------
        txName = show FundHolding_UpdateMinADA_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    --TODO:
                    ctx = fundHolding_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    []

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Deposit_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Deposit_Tests tp =
    let
        ------------------------
        txName = show Fund_Deposit_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Deposit_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctxParametrizable = fundHolding_Deposit_TxContext_Wrapper tp
                in
                    [
                        TastyQC.testProperty
                            "Minted FT amount is as expected"
                            (prop_deposit_mintExpectedFT tp selectedRedeemer ctxParametrizable)
                        , TastyQC.testProperty
                            "Deposit amount zero or negative must fail"
                            (prop_deposit_amountZeroFails tp selectedRedeemer ctxParametrizable)
                    ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Withdraw_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Withdraw_Tests tp =
    let
        ------------------------
        txName = show Fund_Withdraw_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Withdraw_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctxParametrizable = fundHolding_Withdraw_TxContext_Wrapper tp
                in
                    [
                        TastyQC.testProperty
                            "Burnt FT amount is as expected"
                            (prop_withdraw_burnExpectedFT tp selectedRedeemer ctxParametrizable)
                        , TastyQC.testProperty
                            "Withdraw amount zero or negative must fail"
                            (prop_withdraw_amountZeroFails tp selectedRedeemer ctxParametrizable)
                        , TastyQC.testProperty
                            "Value taken from FundHolding is as expected"
                            (prop_withdraw_takeExpectedValue tp selectedRedeemer ctxParametrizable)
                    ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Protocol_Commission_Tests tp =
     let
        ------------------------
        txName = show FundHolding_Collect_Protocol_Commission_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Protocol_Commission_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fundHolding_Collect_Protocol_Commission_TxContext tp
                in
                    [
                    --     TastyQC.testProperty
                    --     "Sending Protocol Admins commissions to a random user without adding an admin signatory must fail"
                    --     (prop_commissions_protocolSignatories tp)

                    ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Managers_Commission_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Collect_Managers_Commission_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Managers_Commission_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fundHolding_Collect_Managers_Commission_TxContext tp
                in
                    [
                        -- , TastyQC.testProperty
                        --     "Sending Fund Managers commissions to a random user without adding an admin signatory must fail"
                        --     (prop_commissions_fundAdminSignatories tp)
                    ]
--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Collect_Delegators_Commission_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Collect_Delegators_Commission_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Collect_Delegators_Commission_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fundHolding_Collect_Delegators_Commission_TxContext tp
                in
                    [
                        TastyQC.testProperty
                        "Withdraw delegators commissions amount is positive and within available"
                        (prop_commissions_delegator_positive tp selectedRedeemer ctx)
                        -- , TastyQC.testProperty
                        --     "Withdraw delegators commissionsm amount is more than available"
                        --     (prop_commissions_delegator_moreThanAvailable tp)
                    ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_ReIndexing_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_ReIndexing_Tests tp =
    let
        ------------------------
        txName = show Fund_ReIndexing_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_ReIndexing_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fundHolding_ReIndexing_TxContext tp
                in
                    [
                        TastyQC.testProperty
                        "Valid add/remove assets should succeed"
                        (prop_reIndex_varyAssets_succeed tp ctx)
                    ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_BalanceAssets_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_BalanceAssets_Tests tp =
    let
        ------------------------
        txName = show FundHolding_BalanceAssets_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_BalanceAssets_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fundHolding_BalanceAssets_TxContext tp
                in
                    [
                        --TODO
                    ]

--------------------------------------------------------------------------------

fundHolding_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
fundHolding_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = show FundHolding_Delete_Tx
        selectedRedeemer = RedeemerLogValidator (Just FundHolding_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = fundHolding_Delete_TxContext tp
                in
                    [
                        --TODO
                    ]

--------------------------------------------------------------------------------

prop_deposit_mintExpectedFT :: TestParams -> RedeemerLog -> TxContextParametrizable -> IntegerPlusZero -> IntegerPlusZero -> QC.Property
prop_deposit_mintExpectedFT tp selectedRedeemer ctxParametrizable (IntegerPlusZero deposit') (IntegerPlusZero mintAmount') = QC.once $ do
        let
            ---------------------
            ctx = ctxParametrizable [TxParam "depositDate" (tpDepositDate tp), TxParam "depositAmount" deposit']
            --------------------
            ctx' = ctx
                    |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) mintAmount', FundT.mkMintFTRedeemer)]
            --------------------
        results <- testContextWrapper tp ctx'
        assertResultsContainAnyOfIfCondition (Just selectedRedeemer, results) ["not isMintingFT"] (deposit' /= mintAmount')

--------------------------------------------------------------------------------

prop_deposit_amountZeroFails :: TestParams -> RedeemerLog -> TxContextParametrizable -> IntegerMinusZero -> QC.Property
prop_deposit_amountZeroFails tp selectedRedeemer ctxParametrizable (IntegerMinusZero deposit') = QC.once $ do
        let
            ---------------------
            ctx = ctxParametrizable [TxParam "depositDate" (tpDepositDate tp), TxParam "depositAmount" deposit']
            --------------------
            ctx' = ctx
            --------------------
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not Correct Deposit Amount","Deposit cannot be negative"]

--------------------------------------------------------------------------------

prop_withdraw_burnExpectedFT :: TestParams -> RedeemerLog -> TxContextParametrizable -> IntegerPlusZero -> IntegerPlusZero -> IntegerPlusZero -> QC.Property
prop_withdraw_burnExpectedFT tp selectedRedeemer ctxParametrizable (IntegerPlusZero deposit') (IntegerPlusZero withdraw') (IntegerPlusZero burnAmount') =
    QC.once $ do
        let
            --------------------
            depositDate = tpDepositDate tp
            depositAmount = deposit'
            withdrawDate = tpWithdrawDate tp
            withdrawAmount = withdraw'
            --------------------
            input_Fund_UTxO = fund_UTxO_MockData tp
            input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
            --------------------
            input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
            input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
            input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
            investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues input_InvestUnit)
            --------------------
            base_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
            base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO base_FundHolding_UTxO
            --------------------
            input_FundHolding_UTxO = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp input_Fund_Datum base_FundHolding_Datum input_InvestUnit 0 depositAmount depositDate
            input_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding_UTxO
            -- input_FundHolding_Value = LedgerApiV2.txOutValue input_FundHolding_UTxO
            --------------------
            (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6) = calculateWithdrawCommissionsUsingMonths_Parametrizable tp input_Fund_Datum withdrawDate withdrawAmount investUnit_Granularity
            --------------------
            ctx = ctxParametrizable [
                            TxParam "depositDate" depositDate,
                            TxParam "depositAmount" depositAmount,
                            TxParam "withdrawDate" withdrawDate,
                            TxParam "withdrawAmount" withdrawAmount
                            ]
            --------------------
            ctx' = ctx
                    |> setMintAndAddRedeemers [(LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) $ -burnAmount', FundT.mkBurnFTRedeemer)]
        --------------------
        results <- testContextWrapper tp ctx'
        --------------------
        if commissions_FT_Release_PerMonth_1e6 >= FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 input_FundHolding_Datum then
                assertResultsContainAnyOf (Just selectedRedeemer, results)
                ["not isEnough_Commissions_Release_PerMonth"]
        else
            if commissionsForUserFTToGetBack >= FundHoldingT.hdSubTotal_FT_Commissions input_FundHolding_Datum then
                assertResultsContainAnyOf (Just selectedRedeemer, results)
                ["not isEnough_FT_ForComission"]
            else if withdrawPlusCommissionsGetBack /= burnAmount' then
                assertResultsContainAnyOf (Just selectedRedeemer, results) ["not isBurningFT"]
            else
                assertResultsContainAnyOf (Just selectedRedeemer, results) []

--------------------------------------------------------------------------------

prop_withdraw_amountZeroFails :: TestParams -> RedeemerLog -> TxContextParametrizable -> IntegerPlusZero -> IntegerMinusZero -> QC.Property
prop_withdraw_amountZeroFails tp selectedRedeemer ctxParametrizable (IntegerPlusZero deposit') (IntegerMinusZero withdraw') =
    QC.once $ do
        let
            --------------------
            depositDate = tpDepositDate tp
            depositAmount = deposit'
            withdrawDate = tpWithdrawDate tp
            withdrawAmount = withdraw'
            --------------------
            ctx = ctxParametrizable [
                            TxParam "depositDate" depositDate,
                            TxParam "depositAmount" depositAmount,
                            TxParam "withdrawDate" withdrawDate,
                            TxParam "withdrawAmount" withdrawAmount
                            ]
            --------------------
            ctx' = ctx
            --------------------
        results <- testContextWrapper tp ctx'
        --------------------
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not Correct Withdraw Amount", "Withdraw cannot be negative"]

--------------------------------------------------------------------------------

prop_withdraw_takeExpectedValue :: TestParams -> RedeemerLog -> TxContextParametrizable -> QC.Property
prop_withdraw_takeExpectedValue tp selectedRedeemer ctxParametrizable =
    do
        let
            --------------------
            depositDate = tpDepositDate tp
            depositAmount = deposit_MockData
            withdrawDate = tpWithdrawDate tp
            withdrawAmount = withdraw_MockData
            --------------------
            ctx = ctxParametrizable [
                            TxParam "depositDate" depositDate,
                            TxParam "depositAmount" depositAmount,
                            TxParam "withdrawDate" withdrawDate,
                            TxParam "withdrawAmount" withdrawAmount
                            ]
            --------------------
            input_Fund_UTxO = fund_UTxO_MockData tp
            input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
            --------------------
            input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
            input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
            input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
            --------------------
            base_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
            base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO base_FundHolding_UTxO
            --------------------
            input_FundHolding_UTxO = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp input_Fund_Datum base_FundHolding_Datum input_InvestUnit 0 depositAmount depositDate
            input_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding_UTxO
            input_FundHolding_Value = LedgerApiV2.txOutValue input_FundHolding_UTxO
            --------------------
            !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues input_InvestUnit)
            (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6) = calculateWithdrawCommissionsUsingMonths_Parametrizable tp input_Fund_Datum withdrawDate withdrawAmount investUnit_Granularity
            --------------------
            investUnit_Value = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 input_InvestUnit withdrawPlusCommissionsGetBack
            --------------------
            output_FundHolding_Datum = FundHelpers.mkUpdated_FundHolding_Datum_With_Withdraw
                    input_FundHolding_Datum
                    withdrawAmount commissionsForUserFTToGetBack commissions_FT_Release_PerMonth_1e6
            output_FundHolding_UTxO' = input_FundHolding_UTxO
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        FundHoldingT.mkDatum output_FundHolding_Datum
                , LedgerApiV2.txOutValue = input_FundHolding_Value <> negate investUnit_Value <> negate (LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionsForUserFTToGetBack)
                }
            -----------------
            randomValue = varyValue investUnit_Value
            --------------------
        QC.forAll randomValue $ \value -> do
            let
                --------------------
                output_FundHolding_UTxO =
                    output_FundHolding_UTxO'
                        { LedgerApiV2.txOutValue =
                            LedgerApiV2.txOutValue output_FundHolding_UTxO'
                                <> negate value
                        }
                --------------------
                ctx' = ctx
                        |> setOutputs [output_FundHolding_UTxO]
                --------------------
            results <- testContextWrapper tp ctx'
            if LedgerApiV2.txOutValue output_FundHolding_UTxO
                    /= (LedgerApiV2.txOutValue input_FundHolding_UTxO <> negate investUnit_Value <> negate (LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionsForUserFTToGetBack)) then
                assertResultsContainAnyOf (Just selectedRedeemer, results)
                ["wrong output value"]
            else
                assertResultsContainAnyOf (Just selectedRedeemer, results) []

--------------------------------------------------------------------------------

prop_commissions_protocolSignatories :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_commissions_protocolSignatories tp selectedRedeemer ctx =
    QC.forAll (genValid_Collect_Protocol_Commissions_Params tp) $ \cpparams -> do
        let
            fundHoldingDatumType' = getCPPFundHoldingDatumType cpparams
            withdraw' = getCPPWithdrawAmount cpparams
            input_FundHolding_UTxO =
                (fundHolding_UTxO_With_Deposits_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum fundHoldingDatumType'
                    }

            outputHoldingUTxO =
                input_FundHolding_UTxO
                    { LedgerApiV2.txOutValue =
                        LedgerApiV2.txOutValue input_FundHolding_UTxO
                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (negate withdraw')
                    , LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $
                            FundHoldingT.mkDatum $
                                fundHoldingDatumType'
                                    {
                                        FundHoldingT.hdSubTotal_FT_Commissions =
                                        FundHoldingT.hdSubTotal_FT_Commissions
                                            fundHoldingDatumType'
                                            - withdraw'
                                    , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol =
                                        FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol
                                            fundHoldingDatumType'
                                            + withdraw'
                                    }
                    }
            ctx' = ctx
                    |> setSignatories []
                    |> setInputsAndAddRedeemers [(input_FundHolding_UTxO, FundHoldingT.mkCollect_Protocol_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw')]
                    |> setOutputs [outputHoldingUTxO]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]

--------------------------------------------------------------------------------

prop_commissions_fundAdminSignatories :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_commissions_fundAdminSignatories tp selectedRedeemer ctx =
    QC.forAll (genValid_Collect_Managers_Params tp) $ \cfaparams -> do
        let
            fundHoldingDatumType' = getCFAPFundHoldingDatumType cfaparams
            withdraw' = getCFAPWithdrawAmount cfaparams
            input_FundHolding_UTxO =
                (fundHolding_UTxO_With_Deposits_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum fundHoldingDatumType'
                    }

            outputHoldingUTxO =
                input_FundHolding_UTxO
                    { LedgerApiV2.txOutValue =
                        LedgerApiV2.txOutValue input_FundHolding_UTxO
                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (negate withdraw')
                    , LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $
                            FundHoldingT.mkDatum $
                                fundHoldingDatumType'
                                    { FundHoldingT.hdSubTotal_FT_Commissions =
                                        FundHoldingT.hdSubTotal_FT_Commissions
                                            fundHoldingDatumType'
                                            - withdraw'
                                    , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers =
                                        FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers
                                            fundHoldingDatumType'
                                            + withdraw'
                                    }
                    }
            ctx' = ctx
                    |> setSignatories []
                    |> setInputsAndAddRedeemers [(input_FundHolding_UTxO, FundHoldingT.mkCollect_Managers_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw')]
                    |> setOutputs [outputHoldingUTxO]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not isSignedByAny admins nor isAdminTokenPresent"]

--------------------------------------------------------------------------------

prop_commissions_delegator_zeroOrLess :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_commissions_delegator_zeroOrLess tp selectedRedeemer ctx =
    QC.forAll (genInvalid_Collect_Delegators_ParamsZeroOrLess tp) $ \cmparams -> do
        let
            fundHoldingDatumType' = getCMPFundHoldingDatumType cmparams
            withdraw' = getCMPWithdrawAmount cmparams

            input_FundHolding_UTxO =
                (fundHolding_UTxO_With_Deposits_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum fundHoldingDatumType'
                    }

            outputHoldingUTxO =
                input_FundHolding_UTxO
                    { LedgerApiV2.txOutValue =
                        LedgerApiV2.txOutValue input_FundHolding_UTxO
                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (negate withdraw')
                    , LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $
                            FundHoldingT.mkDatum $
                                fundHoldingDatumType'
                                    { FundHoldingT.hdSubTotal_FT_Commissions =
                                        FundHoldingT.hdSubTotal_FT_Commissions
                                            fundHoldingDatumType'
                                            - withdraw'
                                    , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators =
                                        FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators
                                            fundHoldingDatumType'
                                            + withdraw'
                                    }
                    }
            ctx' = ctx
                    |> setInputsAndAddRedeemers [(input_FundHolding_UTxO, FundHoldingT.mkCollect_Delegators_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw')]
                    |> setOutputs [outputHoldingUTxO]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not withdraw > 0"]

--------------------------------------------------------------------------------

prop_commissions_delegator_positive :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_commissions_delegator_positive tp selectedRedeemer ctx =
    QC.forAll (genValid_Collect_Delegators_Params tp) $ \cmparams -> do
        let
            fundHoldingDatumType' = getCMPFundHoldingDatumType cmparams
            withdraw' = getCMPWithdrawAmount cmparams
            input_FundHolding_UTxO =
                (fundHolding_UTxO_With_Deposits_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum fundHoldingDatumType'
                    }

            outputHoldingUTxO =
                input_FundHolding_UTxO
                    { LedgerApiV2.txOutValue =
                        LedgerApiV2.txOutValue input_FundHolding_UTxO
                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (negate withdraw')
                    , LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $
                            FundHoldingT.mkDatum $
                                fundHoldingDatumType'
                                    { FundHoldingT.hdSubTotal_FT_Commissions =
                                        FundHoldingT.hdSubTotal_FT_Commissions
                                            fundHoldingDatumType'
                                            - withdraw'
                                    , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators =
                                        FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators
                                            fundHoldingDatumType'
                                            + withdraw'
                                    }
                    }
        -----------------------------------
            ctx' = ctx
                    |> setInputsAndAddRedeemers [(input_FundHolding_UTxO, FundHoldingT.mkCollect_Delegators_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw')]
                    |> setOutputs [outputHoldingUTxO]
        -----------------------------------
        results <- testContextWrapper tp ctx'
        -----------------------------------
        if withdraw' <= 0 then
            (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not withdraw > 0"]
        else
            (Just selectedRedeemer, results)
                `assertResultsContainAnyOf` []

--------------------------------------------------------------------------------

prop_commissions_delegator_moreThanAvailable :: TestParams -> RedeemerLog -> LedgerApiV2.ScriptContext -> QC.Property
prop_commissions_delegator_moreThanAvailable tp selectedRedeemer ctx =
    QC.forAll (genInvalid_Collect_Delegators_ParamsMoreThanAvailables tp) $ \cmparams -> do
        let
            fundHoldingDatumType' = getCMPFundHoldingDatumType cmparams
            withdraw' = getCMPWithdrawAmount cmparams
            randomUserUTxO =
                LedgerApiV2.TxOut
                    (LedgerAddress.pubKeyHashAddress (LedgerAddress.PaymentPubKeyHash "aa") Nothing)
                    ( LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens
                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) withdraw'
                    )
                    LedgerApiV2.NoOutputDatum
                    Nothing

            input_FundHolding_UTxO =
                (fundHolding_UTxO_With_Deposits_MockData tp)
                    { LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum fundHoldingDatumType'
                    }

            outputHoldingUTxO =
                input_FundHolding_UTxO
                    { LedgerApiV2.txOutValue =
                        LedgerApiV2.txOutValue input_FundHolding_UTxO
                            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (negate withdraw')
                    , LedgerApiV2.txOutDatum =
                        LedgerApiV2.OutputDatum $
                            FundHoldingT.mkDatum $
                                fundHoldingDatumType'
                                    { FundHoldingT.hdSubTotal_FT_Commissions =
                                        FundHoldingT.hdSubTotal_FT_Commissions
                                            fundHoldingDatumType'
                                            - withdraw'
                                    , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators =
                                        FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators
                                            fundHoldingDatumType'
                                            + withdraw'
                                    }
                    }
            ctx' = ctx
                    |> setInputsAndAddRedeemers [(input_FundHolding_UTxO, FundHoldingT.mkCollect_Delegators_CommissionRedeemer (tpCollectCommissionsDate tp) withdraw')]
                    |> setOutputs [outputHoldingUTxO, randomUserUTxO]
        results <- testContextWrapper tp ctx'
        (Just selectedRedeemer, results)
            `assertResultsContainAnyOf` ["not isCommissionsAvailable"]

--------------------------------------------------------------------------------

prop_reIndex_varyAssets_succeed :: TestParams -> LedgerApiV2.ScriptContext -> QC.Property
prop_reIndex_varyAssets_succeed tp ctx =
    QC.forAll (genReIndexParams False tp True) $ \(ReIndexParams riFund_UTXO' riFundHolding_UTxOs riTotal_Deposits_IU' riInvestUnit' riTokensToAdd' riTokensToRemove' riTokensPrices') -> do
        let
            ----------------------------
            input_Fund_UTxO = riFund_UTXO'
            input_FundHolding_UTxO = head riFundHolding_UTxOs
            inputsRef_FundHolding_UTxOs = tail riFundHolding_UTxOs
            total_Deposits_IU = riTotal_Deposits_IU'
            tokensPrices = riTokensPrices'
            ----------------------------
            investUnit = riInvestUnit'
            tokensToAdd = riTokensToAdd'
            tokensToRemove = riTokensToRemove'
            ----------------------------
            input_InvestUnit_Datum =
                InvestUnitT.mkDatum $
                    InvestUnitT.mkInvestUnit_DatumType
                        (tpFundPolicy_CS tp)
                        investUnit
                        minAdaIUDatum
            ----------------------------
            input_InvestUnit_UTxO = (investUnit_UTxO_MockData tp) {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum input_InvestUnit_Datum}
            ----------------------------
            valueOf_TotalTokensToAdd = OnChainHelpers.flattenValueToValue [(cs, tn, (am * total_Deposits_IU) `divide` 100) | (cs, tn, am) <- T.iuValues riTokensToAdd']
            valueOf_TotalTokensToRemove = OnChainHelpers.flattenValueToValue [(cs, tn, (am * total_Deposits_IU) `divide` 100) | (cs, tn, am) <- T.iuValues riTokensToRemove']
            ----------------------------
            output_FundHolding_UTxO =
                input_FundHolding_UTxO
                    { LedgerApiV2.txOutValue =
                        LedgerApiV2.txOutValue input_FundHolding_UTxO
                            <> valueOf_TotalTokensToAdd
                            <> negate valueOf_TotalTokensToRemove
                    }
            ----------------------------
            output_InvestUnit' = OnChainHelpers.flattenValueAdd (T.iuValues investUnit) (T.iuValues tokensToAdd)
            output_InvestUnit = OnChainHelpers.flattenValueSub output_InvestUnit' (T.iuValues tokensToRemove)
            ----------------------------
            output_InvestUnit_Datum =
                InvestUnitT.mkDatum $
                    InvestUnitT.mkInvestUnit_DatumType
                        (tpFundPolicy_CS tp)
                        (T.InvestUnit output_InvestUnit)
                        minAdaIUDatum
            ----------------------------
            output_InvestUnit_UTxO = input_InvestUnit_UTxO {LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum output_InvestUnit_Datum}
            ----------------------------
            oracleData = T.OracleReIdx_Data tokensPrices (tpReIdxDate tp)
            priceData = OnChainHelpers.oracleReIdxDataToBBS oracleData
            oracleSignature = Ledger.sign' priceData (tpOraclePrivateKey tp)
            ----------------------------
            ctx' = ctx
                    |> setInputsRef (protocol_UTxO_MockData tp : input_Fund_UTxO : uTxOForValidatorAsReference tp (tpFundHoldingValidator tp) : uTxOForValidatorAsReference tp (tpInvestUnitValidator tp) :  inputsRef_FundHolding_UTxOs)
                    |> setInputsAndAddRedeemers [(input_FundHolding_UTxO, FundHoldingT.mkReIndexingRedeemer riTokensToAdd' riTokensToRemove'), (input_InvestUnit_UTxO, InvestUnitT.mkReIndexingRedeemer
                            riTokensToAdd'
                            riTokensToRemove'
                            oracleData
                            oracleSignature)]
                    |> setOutputs [output_FundHolding_UTxO, output_InvestUnit_UTxO]
        ----------------------------
        results <- testContextWrapper tp ctx'
        ----------------------------
        (Nothing, results)
            `assertResultsContainAnyOf` []

--------------------------------------------------------------------------------
