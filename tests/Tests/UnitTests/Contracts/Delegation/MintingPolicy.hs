--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : Delegation.MintingPolicy
Description : Validation logic and tests related to the Delegation minting policy.

This module defines the validation logic for the Delegation's minting policy.

It includes multiple test cases to ensure the integrity and correctness of the
minting script.
-}
module Contracts.Delegation.MintingPolicy where
--------------------------------------------------------------------------------
-- Non-IOG imports
import qualified Test.Tasty                              as Tasty
import qualified Test.Tasty.HUnit                        as Tasty

-- IOG imports
import           PlutusTx.Prelude

-- Project imports
import qualified Plutus.V2.Ledger.Api                    as LedgerApiV2
import qualified Protocol.Protocol.Types                 as ProtocolT
import qualified Protocol.Delegation.Types                as DelegationT
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxContext.Delegation
import           TestUtils.HelpersMAYZ
import           TestUtils.TestContext.Asserts
import           TestUtils.TestContext.Helpers
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

delegation_Policy_Tests :: TestParams -> Tasty.TestTree
delegation_Policy_Tests tp =
    Tasty.testGroup
        "Delegation Policy Tests"
        [
            delegation_Policy_Redeemer_MintID_Tests tp,
            delegation_Policy_Redeemer_BurnID_Tests tp
        ]

--------------------------------------------------------------------------------

delegation_Policy_Redeemer_MintID_Tests :: TestParams -> Tasty.TestTree
delegation_Policy_Redeemer_MintID_Tests tp =
    let
        ------------------------
        txName = "Delegation_Create_Tx"
        selectedRedeemer = RedeemerLogPolicy (Just Delegation_MintID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = delegation_Create_TxContext tp
                in
                    [
                        Tasty.testCase "Minting ID correctly must succeed" $ do
                        results <- testContextWrapper tp ctx
                        (Nothing, results)
                            `assertResultsContainAnyOf` []

                        , Tasty.testCase "Delegation with wrong MAYZ in datum must fail" $ do
                            let
                                wrongDatum = DelegationT.mkDatum $
                                                (delegation_DatumType_MockData tp)
                                                    { DelegationT.ddDelegated_MAYZ =  delegation_MockData + sum_ANY_INVALID_NUMBER}
                                wrongUTxO =
                                    (delegation_UTxO_MockData tp)
                                        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum wrongDatum
                                        }

                                ctx' = ctx
                                        |> setOutputs [wrongUTxO]
                            results <- testContextWrapper tp ctx'
                            (Just selectedRedeemer, results)
                                    `assertResultsContainAnyOf` ["not isCorrect_Output_Delegation_Value"]

                    ]


--------------------------------------------------------------------------------

delegation_Policy_Redeemer_BurnID_Tests :: TestParams -> Tasty.TestTree
delegation_Policy_Redeemer_BurnID_Tests tp =
    let
        ------------------------
        txName = "Delegation_Delete_Tx"
        selectedRedeemer = RedeemerLogPolicy (Just Delegation_BurnID_TestRedeemer)
        redeemerName = getRedeemerNameFromLog selectedRedeemer
        ------------------------
    in
        Tasty.testGroup ("TX NAME: " ++ txName ++ " - REDEEMER: " ++ redeemerName ++ " - Tests") $
                let
                    ctx = delegation_Delete_TxContext tp
                in
                    [
                        Tasty.testCase "Burning ID correctly must succeed" $ do
                            let ctx' = ctx
                            results <- testContextWrapper tp ctx'
                            (Nothing, results)
                                `assertResultsContainAnyOf` []
                    ]

--------------------------------------------------------------------------------
