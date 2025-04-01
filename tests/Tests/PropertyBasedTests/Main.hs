--------------------------------------------------------------------------------4
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------4
{- |
Module      : Tests.PropertyBasedTests.Main
Description : Property-Based Tests.
-}
module Main where
--------------------------------------------------------------------------------4

-- Non-IOG imports
import qualified Control.Monad.IO.Class               as MonadIO
import qualified Control.Monad.Reader                 as MReader
import qualified Data.Proxy                           as DataProxy
import           Prelude
import qualified Test.Tasty                           as Tasty
import qualified Test.Tasty.Options                   as TastyOptions
import qualified Test.Tasty.QuickCheck                as TastyQC
import qualified System.IO as SystemIO
import qualified System.Directory as SystemDirectory
-- Project imports
import           Contracts.Fund.Holding.MintingPolicy
import           Contracts.Fund.Holding.Validator
import           Contracts.Fund.MintingPolicy
import           Contracts.Fund.Validator
import           Contracts.InvestUnit.Validator
import           Contracts.Protocol.MintingPolicy
import           Contracts.Protocol.Validator
import           Contracts.SwapOffer.MintingPolicy
import           Contracts.SwapOffer.Validator
import           TestTree.Commissions
import           TestTree.ScaleAndRunding
import           TestTree.Values
import           TestUtils.Helpers
import           TestUtils.HelpersMAYZ
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------4

main :: IO ()
main = do
    putStrLn "---------------"
    -- Prompt user to delete previous test contracts
    putStr "Do you want to delete previous exported test smart contracts? (yes/no): "
    SystemIO.hFlush SystemIO.stdout  -- Ensure the prompt is displayed before user input
    response <- getLine
    let exportFolder = "export/test/"
    case response of
        "yes" -> do
            exists <- SystemDirectory.doesDirectoryExist exportFolder
            if exists 
                then do
                    SystemDirectory.removeDirectoryRecursive exportFolder
                    putStrLn "Previous test contracts deleted."
                else putStrLn "No previous test contracts found."
        _ -> putStrLn "Keeping previous test contracts."
    putStrLn "---------------"
    tp <- getTestParams "export/test/deploy.json"
    numTests <- getNumTestCases 100
    putStrLn "---------------"
    let !appConfig = AppConfig {appTestParams = tp}
    MReader.runReaderT (runApp $ appMain numTests) appConfig

--------------------------------------------------------------------------------4

appMain :: Int -> AppM ()
appMain numTests = do
    -------------
    protocol_Policy_Tests_ <- protocol_Policy_Tests
    protocol_Validator_Tests_ <- protocol_Validator_Tests
    fund_Policy_Tests_ <- fund_Policy_Tests
    fund_Validator_Tests_ <- fund_Validator_Tests
    fundHolding_Validator_Tests_ <- fundHolding_Validator_Tests
    fundHolding_Policy_Tests_ <- fundHolding_Policy_Tests
    investUnit_Validator_Tests_ <- investUnit_Validator_Tests
    swapOffer_Policy_Tests_ <- swapOffer_Policy_Tests
    swapOffer_Validator_Tests_ <- swapOffer_Validator_Tests
    -------------
    valueTests_ <- value_Tests
    comission_Tests_ <- comission_Tests
    scale_And_Runding_Tests_ <- scale_And_Runding_Tests
    -------------
    let testGroup =
            Tasty.testGroup
                "Property-Based Tests"
                [ Tasty.testGroup
                    "Contracts Tests"
                    [
                      protocol_Policy_Tests_
                    , protocol_Validator_Tests_
                    , fund_Policy_Tests_
                    , fund_Validator_Tests_
                    , fundHolding_Policy_Tests_
                    , fundHolding_Validator_Tests_
                    , investUnit_Validator_Tests_
                    , swapOffer_Policy_Tests_
                    , swapOffer_Validator_Tests_
                    ]
                , Tasty.testGroup
                    "Helpers Tests"
                    [
                        Tasty.testGroup "Value Tests"
                            [valueTests_]
                        , Tasty.testGroup "Comission Tests"
                            [comission_Tests_]
                        , Tasty.testGroup "Scale and Runding Tests"
                            [scale_And_Runding_Tests_]
                    ]
                ]
    MonadIO.liftIO $ Tasty.defaultMainWithIngredients
        (Tasty.includingOptions [TastyOptions.Option (DataProxy.Proxy :: DataProxy.Proxy TastyQC.QuickCheckTests)] : Tasty.defaultIngredients)
        (Tasty.localOption (TastyQC.QuickCheckTests numTests) testGroup )

