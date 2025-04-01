--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4

{- |
Module      : Delegation.Validator
Description : Validation logic and tests related to the Delegation validator.

This module defines the validation logic for the Delegation's validator

It includes multiple test cases to ensure the integrity and correctness of the
validator script.
-}
module Contracts.Delegation.Validator where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Test.Tasty                              as Tasty
import qualified Test.Tasty.HUnit                        as Tasty

-- IOG imports
import           PlutusTx.Prelude

-- Project imports
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Delegation
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

delegation_Validator_Tests :: TestParams -> Tasty.TestTree
delegation_Validator_Tests tp =
    Tasty.testGroup
        "Delegation Validator Tests"
        [ 
        delegation_Validator_Redeemer_UpdateMinADA_Tests tp
        , delegation_Validator_Redeemer_Deposit_Tests tp
        , delegation_Validator_Redeemer_Withdraw_Tests tp
        , delegation_Validator_Redeemer_Delete_Tests tp
        ]

--------------------------------------------------------------------------------

delegation_Validator_Redeemer_UpdateMinADA_Tests :: TestParams -> Tasty.TestTree
delegation_Validator_Redeemer_UpdateMinADA_Tests tp =
    let
        ------------------------
        txName = "Delegation_UpdateMinADA_Tx"
        selectedRedeemer = RedeemerLogValidator (Just Delegation_UpdateMinADA_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = delegation_UpdateMinADA_TxContext tp toAlter_minAda
                in
                    [
                        Tasty.testCase "Changing min ADA correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

delegation_Validator_Redeemer_Deposit_Tests :: TestParams -> Tasty.TestTree
delegation_Validator_Redeemer_Deposit_Tests tp =
    let
        ------------------------
        txName = "Delegation_Deposit_Tx"
        selectedRedeemer = RedeemerLogValidator (Just Delegation_Deposit_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = delegation_Deposit_TxContext tp 100
                in
                    [
                        Tasty.testCase "Depositing correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapperTrace tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

delegation_Validator_Redeemer_Withdraw_Tests :: TestParams -> Tasty.TestTree
delegation_Validator_Redeemer_Withdraw_Tests tp =
    let
        ------------------------
        txName = "Delegation_Withdraw_Tx"
        selectedRedeemer = RedeemerLogValidator (Just Delegation_Withdraw_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = delegation_Withdraw_TxContext tp 100
                in
                    [
                        Tasty.testCase "Withdrawing correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapperTrace tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------

delegation_Validator_Redeemer_Delete_Tests :: TestParams -> Tasty.TestTree
delegation_Validator_Redeemer_Delete_Tests tp =
    let
        ------------------------
        txName = "Delegation_Delete_Tx"
        selectedRedeemer = RedeemerLogValidator (Just Delegation_Delete_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = delegation_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Deleting correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------
