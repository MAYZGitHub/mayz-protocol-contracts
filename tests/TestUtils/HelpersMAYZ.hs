{-# LANGUAGE TypeApplications #-}
--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2
{- |
Module      : TestUtils.Helpers
Description : Common Mock Data and Auxiliary Functions for testing.
-}
module TestUtils.HelpersMAYZ where

--------------------------------------------------------------------------------

-- Non-IOG imports
import           Prelude                        as P
import qualified Protocol.DeployPAB                as DeployPAB
import qualified System.Directory               as SystemDirectory

-- IOG imports
import qualified Ledger
import qualified Ledger.Crypto                  as LedgerCrypo
import qualified Plutus.V2.Ledger.Api           as LedgerApiV2
import qualified PlutusTx
import qualified PlutusTx.Builtins              as TxBuiltins
import           PlutusTx.Prelude               as Ptx hiding (($), (*), (+), (++), (-), (.), (<>), (==))

-- Project imports

import qualified Generic.DeployHelpers          as DeployHelpers
import qualified Generic.OffChainHelpers        as OffChainHelpers
import qualified Generic.OnChainHelpers         as OnChainHelpers
import qualified Protocol.BuyOrder.Types        as BuyOrderT
import qualified Protocol.Constants             as T
import qualified Protocol.Delegation.Types      as DelegationT
import qualified Protocol.Fund.Helpers          as FundHelpers
import qualified Protocol.Fund.Holding.Types    as FundHoldingT
import qualified Protocol.Fund.Types            as FundT
import qualified Protocol.Fund.InvestUnit.Types      as InvestUnitT
import qualified Protocol.PABTypes              as T
import qualified Protocol.Protocol.Types        as ProtocolT
import qualified Protocol.Script.Types          as ScriptT
import qualified Protocol.SwapOffer.Types       as SwapOfferT
import qualified Protocol.Types                 as T
import           TestUtils.Constants
import           TestUtils.Helpers
import           TestUtils.TestContext.Evaluate
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------

getTestParams :: String -> IO TestParams
getTestParams filePath = do
    maybeDeployParams <- loadDeployParams filePath
    !params <- case maybeDeployParams of
            Just params -> generateTestParams params
            Nothing     -> P.error "Failed to load deploy parameters"
    ------------------------------
    P.putStrLn $ "Using Test Params: " <> show params
    ------------------------------
    return params

--------------------------------------------------------------------------------

getTestCompiledCodeScripts :: String -> IO TestCompiledCodeScripts
getTestCompiledCodeScripts filePath = do
    maybeDeployParams <- loadDeployParams filePath
    case maybeDeployParams of
            Just params -> generateScripts params
            Nothing     -> P.error "Failed to load deploy parameters"

--------------------------------------------------------------------------------

loadDeployParams :: FilePath -> IO (Maybe T.DeployAllPreParams)
loadDeployParams filePath = do
    fileExists <- SystemDirectory.doesFileExist filePath
    if fileExists
        then do
            ------------------------------
            P.putStrLn $ "Reading Deploy File: " <> filePath
            ------------------------------
            OffChainHelpers.readDecodedFromFile filePath
        else do
            deployParams <- DeployPAB.deploy_PRE "export" "test" False
            return $ Just deployParams

--------------------------------------------------------------------------------

generateTestParams :: T.DeployAllPreParams -> IO TestParams
generateTestParams deployAllParams = do
    putStrLn "Generating Test Params..."
    testCompiledCodeScripts <- generateScripts deployAllParams
    let
        ------------
        protocolPolicyID_TxOutRef =
            LedgerApiV2.TxOutRef
                { LedgerApiV2.txOutRefId = "0000000000000000000000000000000000000000000000000000000000000000"
                , LedgerApiV2.txOutRefIdx = 0
                }
        fundPolicy_TxOutRef =
            LedgerApiV2.TxOutRef
                { LedgerApiV2.txOutRefId = "0000000000000000000000000000000000000000000000000000000000000008"
                , LedgerApiV2.txOutRefIdx = 1
                }
        ------------
        protocolAdmins = ["0000000000000000000000000000000000000000000000000000000000000004"]
        fundAdmins = ["0000000000000000000000000000000000000000000000000000000000000005"]
        delegatorsAdmins = ["0000000000000000000000000000000000000000000000000000000000000006"]
        swapOfferAdmin = "0000000000000000000000000000000000000000000000000000000000000007"
        delegationAdmin = "0000000000000000000000000000000000000000000000000000000000000011"
        ------------
        tokenEmergencyAdminPolicy_CS = "a0c49fcf369da4a230072fe7477cd3ad0716e9d62bf7d7e3aa0ae062"
        tokenAdminPolicy_CS = "0000000000000000000000000000000000000000000000000000000000000002"
        tokenMAYZ_CS = T.tokenMAYZ_CS_aux
        tokenMAYZ_TN = T.tokenMAYZ_TN_aux
        fundFT_TN = "FT"
        ------------
        fundLifeTime =
            ProtocolT.MinMaxDef
                (LedgerApiV2.POSIXTime ((1 * 30 * 24 * 60 * 60 * 1000) :: Integer)) -- min
                (LedgerApiV2.POSIXTime ((24 * 30 * 24 * 60 * 60 * 1000) :: Integer)) -- max
                (LedgerApiV2.POSIXTime ((12 * 30 * 24 * 60 * 60 * 1000) :: Integer)) -- def
        requiredMAYZForSwapOffer = 1_000
        requiredMAYZForBuyOrder = 1_000
        minMaxDef_CommissionFund_PerYear_InBPx1e3 = ProtocolT.MinMaxDef 0 10_000_000 120_000 :: ProtocolT.MinMaxDef Integer
        minMaxDef_CommissionSwapOffer_InBPx1e3 = ProtocolT.MinMaxDef 0 10_000_000 120_000 :: ProtocolT.MinMaxDef Integer
        minMaxDef_CommissionBuyOrder_InBPx1e3 = ProtocolT.MinMaxDef 0 10_000_000 120_000 :: ProtocolT.MinMaxDef Integer
        share_InBPx1e2_Protocol = 300_000
        share_InBPx1e2_Managers = 400_000
        share_InBPx1e2_Delegators = 300_000
        ------------
        beginDate = LedgerApiV2.POSIXTime ((1 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        deadlineDate = ProtocolT.mmdDef fundLifeTime
        closedAtDate = Nothing  :: Maybe LedgerApiV2.POSIXTime
        ------------
        commission_PerYear_InBPx1e3 = ProtocolT.mmdDef minMaxDef_CommissionFund_PerYear_InBPx1e3
        monthsRemainingPlusOne = FundHelpers.getRemainingMonths deadlineDate beginDate + 1
        -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120 000 000
        den = 120_000_000
        commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne]]
        ------------
    let
        transactionDate = LedgerApiV2.POSIXTime ((2 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        depositDate = LedgerApiV2.POSIXTime ((3 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        withdrawDate = LedgerApiV2.POSIXTime ((4 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        collectCommissionsDate = LedgerApiV2.POSIXTime ((6 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        reIdxDate = LedgerApiV2.POSIXTime ((7 * 30 * 24 * 60 * 60 * 1000) :: Integer)
        ------------
    let
        oraclePrivateKey =
            LedgerCrypo.generateFromSeed' $
                OffChainHelpers.stringToStrictByteString
                    "dad cupboard hotel cause mansion feature oppose prevent \
                    \install venue finish galaxy tone green volcano neglect \
                    \coil toast future exchange prize social next tape"
    let
        oraclePublicKey = LedgerCrypo.toPublicKey oraclePrivateKey
    let
        oraclePPK = Ledger.PaymentPubKey oraclePublicKey

    let
        exampleFundCategoryNumber :: Integer
        exampleFundCategoryNumber = 0

        exampleFundCategoryRequiredMAYZ :: Integer
        exampleFundCategoryRequiredMAYZ = 7

        exampleFundCategoryMaxUI :: Integer
        exampleFundCategoryMaxUI = 100

        exampleFundCategory :: ProtocolT.FundCategory
        exampleFundCategory =
            ProtocolT.FundCategory
                { ProtocolT.fcCategoryNumber = exampleFundCategoryNumber
                , ProtocolT.fcRequiredMAYZ = exampleFundCategoryRequiredMAYZ
                , ProtocolT.fcMaxUI = exampleFundCategoryMaxUI
                }

    let
        protocolPolicyID =
            do
                let
                    code = tccsProtocolPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleProtocolPolicyParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.getTxId $ LedgerApiV2.txOutRefId protocolPolicyID_TxOutRef
                    param2 = TxBuiltins.mkI $ LedgerApiV2.txOutRefIdx protocolPolicyID_TxOutRef
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        protocolPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy protocolPolicyID

    let
        protocolValidator =
            do
                let
                    code = tccsProtocolValidator_Pre testCompiledCodeScripts
                    -- params = exampleProtocolValidatorParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        protocolValidator_Hash = OffChainHelpers.hashValidator protocolValidator

    let
        scriptPolicyID =
            do
                let
                    code = tccsScriptPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleScriptPolicyParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        scriptPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy scriptPolicyID

    let
        scriptValidator =
            do
                let
                    code = tccsScriptValidator_Pre testCompiledCodeScripts
                    -- params = exampleScriptValidatorParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol scriptPolicyID_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        scriptValidator_Hash = OffChainHelpers.hashValidator scriptValidator

    let
        swapOfferValidator =
            do
                let
                    code = tccsSwapOfferValidator_Pre testCompiledCodeScripts
                    -- params = exampleSwapOfferValidParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        swapOfferValidator_Hash = OffChainHelpers.hashValidator swapOfferValidator

    let
        swapOfferPolicyID =
            do
                let
                    code = tccsSwapOfferPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleSwapOfferPolicyParams
                    -- protocolPolicyID_CS swapOffer_Validator_Hash tokenMAYZ_CS tokenMAYZ_TN
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    (LedgerApiV2.ValidatorHash hash) = swapOfferValidator_Hash
                    param2 = TxBuiltins.mkB hash
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2 
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        swapOfferPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy swapOfferPolicyID

    let
        buyOrderValidator =
            do
                let
                    code = tccsBuyOrderValidator_Pre testCompiledCodeScripts
                    -- params = exampleBuyOrderValidaParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1
                LedgerApiV2.mkValidatorScript appliedCode
    let
        buyOrderValidator_Hash = OffChainHelpers.hashValidator buyOrderValidator

    let
        buyOrderPolicyID =
            do
                let
                    code = tccsBuyOrderPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleSwapOfferPolicyParams
                    -- protocolPolicyID_CS buyOrder_Validator_Hash tokenMAYZ_CS tokenMAYZ_TN
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    (LedgerApiV2.ValidatorHash hash) = buyOrderValidator_Hash
                    param2 = TxBuiltins.mkB hash
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2 
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        buyOrderPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy buyOrderPolicyID

    let
        delegationValidator =
            do
                let
                    code = tccsDelegationValidator_Pre testCompiledCodeScripts
                    -- params = exampleDelegationValiParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        delegationValidator_Hash = OffChainHelpers.hashValidator delegationValidator

    let
        delegationPolicyID =
            do
                let
                    code = tccsDelegationPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleSwapOfferPolicyParams
                    --  protocolPolicyID_CS delegation_Validator_Hash tokenMAYZ_CS tokenMAYZ_TN
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    (LedgerApiV2.ValidatorHash hash) = delegationValidator_Hash
                    param2 = TxBuiltins.mkB hash
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        delegationPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy delegationPolicyID

    let
        investUnitValidator =
            do
                let
                    code = tccsInvestUnitValidator_Pre testCompiledCodeScripts
                    -- params = exampleInvestUnitValiParams
                    -- protocolPolicyID_CS tokenEmergencyAdminPolicy_CS
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        investUnitValidator_Hash = OffChainHelpers.hashValidator investUnitValidator

    let
        fundValidator =
            do
                let
                    code = tccsFundValidator_Pre testCompiledCodeScripts
                    -- params = exampleFundValidator_Params
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2
                LedgerApiV2.mkValidatorScript appliedCode
    let
        fundValidator_Hash = OffChainHelpers.hashValidator fundValidator

    let
        fundPolicy =
            do
                let
                    code = tccsFundPolicy_Pre testCompiledCodeScripts
                    --  protocolPolicyID_CS fundPolicy_TxHash fundPolicy_TxOutputIndex fundValidator_Hash tokenMAYZ_CS tokenMAYZ_TN
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.getTxId $ LedgerApiV2.txOutRefId fundPolicy_TxOutRef
                    param3 = TxBuiltins.mkI $ LedgerApiV2.txOutRefIdx fundPolicy_TxOutRef
                    (LedgerApiV2.ValidatorHash hash) = fundValidator_Hash
                    param4 = TxBuiltins.mkB hash
                    appliedCode =
                        code
                            `PlutusTx.applyCode` PlutusTx.liftCode param1
                            `PlutusTx.applyCode` PlutusTx.liftCode param2
                            `PlutusTx.applyCode` PlutusTx.liftCode param3
                            `PlutusTx.applyCode` PlutusTx.liftCode param4
                LedgerApiV2.mkMintingPolicyScript appliedCode
    let
        fundPolicy_CS = OffChainHelpers.getCurSymbolOfPolicy fundPolicy

    let
        fundHoldingValidator =
            do
                let
                    code = tccsFundHoldingValidator_Pre testCompiledCodeScripts
                    -- protocolPolicyID_CS fundPolicy_CS tokenEmergencyAdminPolicy_CS
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol protocolPolicyID_CS
                    param2 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol fundPolicy_CS
                    param3 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol tokenEmergencyAdminPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1 `PlutusTx.applyCode` PlutusTx.liftCode param2 `PlutusTx.applyCode` PlutusTx.liftCode param3
                LedgerApiV2.mkValidatorScript appliedCode
    let
        fundHoldingValidator_Hash = OffChainHelpers.hashValidator fundHoldingValidator

    let
        fundHoldingPolicyID =
            do
                let
                    code = tccsFundHoldingPolicyID_Pre testCompiledCodeScripts
                    -- params = exampleFundHoldingPolParams
                    param1 = TxBuiltins.mkB $ LedgerApiV2.unCurrencySymbol fundPolicy_CS
                    appliedCode = code `PlutusTx.applyCode` PlutusTx.liftCode param1
                LedgerApiV2.mkMintingPolicyScript appliedCode

    let
        fundHoldingPolicyID_CS = OffChainHelpers.getCurSymbolOfPolicy fundHoldingPolicyID

    let tp = TestParams
            { tpProtocolPolicyID = protocolPolicyID
            , tpProtocolPolicyID_CS = protocolPolicyID_CS
            , tpProtocolValidator = protocolValidator
            , tpProtocolValidator_Hash = protocolValidator_Hash
            , tpScriptPolicyID = scriptPolicyID
            , tpScriptPolicyID_CS = scriptPolicyID_CS
            , tpScriptValidator = scriptValidator
            , tpScriptValidator_Hash = scriptValidator_Hash
            , tpSwapOfferPolicyID = swapOfferPolicyID
            , tpSwapOfferPolicyID_CS = swapOfferPolicyID_CS
            , tpSwapOfferValidator = swapOfferValidator
            , tpSwapOfferValidator_Hash = swapOfferValidator_Hash
            , tpBuyOrderPolicyID = buyOrderPolicyID
            , tpBuyOrderPolicyID_CS = buyOrderPolicyID_CS
            , tpBuyOrderValidator = buyOrderValidator
            , tpBuyOrderValidator_Hash = buyOrderValidator_Hash
            , tpDelegationPolicyID = delegationPolicyID
            , tpDelegationPolicyID_CS = delegationPolicyID_CS
            , tpDelegationValidator = delegationValidator
            , tpDelegationValidator_Hash = delegationValidator_Hash
            , tpInvestUnitValidator = investUnitValidator
            , tpInvestUnitValidator_Hash = investUnitValidator_Hash
            , tpFundPolicy = fundPolicy
            , tpFundPolicy_CS = fundPolicy_CS
            , tpFundValidator = fundValidator
            , tpFundValidator_Hash = fundValidator_Hash
            , tpFundHoldingPolicyID = fundHoldingPolicyID
            , tpFundHoldingPolicyID_CS = fundHoldingPolicyID_CS
            , tpFundHoldingValidator = fundHoldingValidator
            , tpFundHoldingValidator_Hash = fundHoldingValidator_Hash
            , tpProtocolPolicyID_TxOutRef = protocolPolicyID_TxOutRef
            , tpFundPolicy_TxOutRef = fundPolicy_TxOutRef
            , tpTokenEmergencyAdminPolicy_CS = tokenEmergencyAdminPolicy_CS
            , tpTokenAdminPolicy_CS = tokenAdminPolicy_CS
            , tpFundFT_TN = fundFT_TN
            , tpTokenMAYZ_CS = tokenMAYZ_CS
            , tpTokenMAYZ_TN = tokenMAYZ_TN
            , tpProtocolAdmins = protocolAdmins
            , tpFundAdmins = fundAdmins
            , tpDelegatorsAdmins = delegatorsAdmins
            , tpSwapOfferAdmin = swapOfferAdmin
            , tpDelegationAdmin = delegationAdmin
            , tpOraclePrivateKey = oraclePrivateKey
            , tpOraclePaymentPubKey = oraclePPK
            , tpFundCategory = exampleFundCategory
            , tpFundLifeTime = fundLifeTime
            , tpRequiredMAYZForSwapOffer = requiredMAYZForSwapOffer
            , tpRequiredMAYZForBuyOrder = requiredMAYZForBuyOrder
            , tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 = minMaxDef_CommissionFund_PerYear_InBPx1e3
            , tp_MinMaxDef_CommissionSwapOffer_InBPx1e3 = minMaxDef_CommissionSwapOffer_InBPx1e3
            , tp_MinMaxDef_CommissionBuyOrder_InBPx1e3 = minMaxDef_CommissionBuyOrder_InBPx1e3
            , tpShare_InBPx1e2_Protocol = share_InBPx1e2_Protocol
            , tpShare_InBPx1e2_Managers = share_InBPx1e2_Managers
            , tpShare_InBPx1e2_Delegators = share_InBPx1e2_Delegators
            , tpBeginAt = beginDate
            , tpDeadline = deadlineDate
            , tpClosedAt = closedAtDate
            , tpCommission_PerYear_InBPx1e3 = commission_PerYear_InBPx1e3
            , tpCommissions_Table_Numerator_1e6 = commissions_Table_Numerator_1e6
            , tpTransactionDate = transactionDate
            , tpDepositDate = depositDate
            , tpWithdrawDate = withdrawDate
            , tpCollectCommissionsDate = collectCommissionsDate
            , tpReIdxDate = reIdxDate
            }
    return tp

--------------------------------------------------------------------------------

generateScripts :: T.DeployAllPreParams -> IO TestCompiledCodeScripts
generateScripts deployAllParams = do
    putStrLn "Generating Scripts..."
    let
        !testCompiledCodeScripts =
            TestCompiledCodeScripts
                { tccsProtocolPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapProtocolPolicyID_Pre_CborHex deployAllParams
                , tccsProtocolValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapProtocolValidator_Pre_CborHex deployAllParams
                , tccsScriptPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapScriptPolicyID_Pre_CborHex deployAllParams
                , tccsScriptValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapScriptValidator_Pre_CborHex deployAllParams
                , tccsSwapOfferPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapSwapOfferPolicyID_Pre_CborHex deployAllParams
                , tccsSwapOfferValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapSwapOfferValidator_Pre_CborHex deployAllParams
                , tccsBuyOrderPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapBuyOrderPolicyID_Pre_CborHex deployAllParams
                , tccsBuyOrderValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapBuyOrderValidator_Pre_CborHex deployAllParams
                , tccsDelegationPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapDelegationPolicyID_Pre_CborHex deployAllParams
                , tccsDelegationValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapDelegationValidator_Pre_CborHex deployAllParams
                , tccsInvestUnitValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapInvestUnitValidator_Pre_CborHex deployAllParams
                , tccsFundPolicy_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapFundPolicy_Pre_CborHex deployAllParams
                , tccsFundValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapFundValidator_Pre_CborHex deployAllParams
                , tccsFundHoldingPolicyID_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapFundHoldingPolicyID_Pre_CborHex deployAllParams
                , tccsFundHoldingValidator_Pre = DeployHelpers.readCompiledCodeFromJsonString $ T.dapFundHoldingValidator_Pre_CborHex deployAllParams
                }
    return testCompiledCodeScripts

--------------------------------------------------------------------------------

testContextWrapper :: TestParams ->  LedgerApiV2.ScriptContext -> IO [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Maybe Integer))]
testContextWrapper tp ctx  =
    let results = testContext (findValidator tp) (findMintingPolicy tp) (findValidatorRedeemerNameMaybe tp) (findMintingPolicyRedeemerNameMaybe tp) ctx False
    in results

testContextWrapperTrace :: TestParams ->  LedgerApiV2.ScriptContext -> IO [(Maybe RedeemerLog, (LedgerApiV2.LogOutput, P.Either LedgerApiV2.EvaluationError LedgerApiV2.ExBudget, Maybe Integer))]
testContextWrapperTrace tp ctx =
    let results = testContext (findValidator tp) (findMintingPolicy tp) (findValidatorRedeemerNameMaybe tp) (findMintingPolicyRedeemerNameMaybe tp) ctx True
    in results

--------------------------------------------------------------------------------

findValidator :: TestParams -> LedgerApiV2.ScriptHash -> Maybe LedgerApiV2.Validator
findValidator tp scriptHash
    | scriptHash == OffChainHelpers.hashScriptValidator (tpProtocolValidator tp) = Just (tpProtocolValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpScriptValidator tp) = Just (tpScriptValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpSwapOfferValidator tp) = Just (tpSwapOfferValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpBuyOrderValidator tp) = Just (tpBuyOrderValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpDelegationValidator tp) = Just (tpDelegationValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpInvestUnitValidator tp) = Just (tpInvestUnitValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpFundValidator tp) = Just (tpFundValidator tp)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpFundHoldingValidator tp) = Just (tpFundHoldingValidator tp)
    | otherwise = Nothing

findMintingPolicy :: TestParams -> LedgerApiV2.ScriptHash -> Maybe LedgerApiV2.MintingPolicy
findMintingPolicy tp scriptHash
    | scriptHash == OffChainHelpers.hashScriptMinting (tpProtocolPolicyID tp) = Just (tpProtocolPolicyID tp)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpScriptPolicyID tp) = Just (tpScriptPolicyID tp)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpSwapOfferPolicyID tp) = Just (tpSwapOfferPolicyID tp)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpBuyOrderPolicyID tp) = Just (tpBuyOrderPolicyID tp)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpDelegationPolicyID tp) = Just (tpDelegationPolicyID tp)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpFundPolicy tp) = Just (tpFundPolicy tp)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpFundHoldingPolicyID tp) = Just (tpFundHoldingPolicyID tp)
    | otherwise = Nothing

----------------------------------------------------------------------------------------

findValidatorRedeemerNameMaybe :: TestParams -> Maybe LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe ValidatorTestRedeemer
findValidatorRedeemerNameMaybe tp scriptHashMaybe redeemer =
    case scriptHashMaybe of
            Just scriptHash -> findValidatorRedeemerName tp scriptHash redeemer
            Nothing         -> Nothing

findValidatorRedeemerName :: TestParams -> LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe ValidatorTestRedeemer
findValidatorRedeemerName tp scriptHash (LedgerApiV2.Redeemer redeemer)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpProtocolValidator tp) =
        getValidatorTestRedeemer (case ProtocolT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @ProtocolT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "Protocol_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpScriptValidator tp) =
        getValidatorTestRedeemer (case ScriptT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @ScriptT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "Script_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpSwapOfferValidator tp) =
        getValidatorTestRedeemer (case SwapOfferT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @SwapOfferT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "SwapOffer_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpBuyOrderValidator tp) =
        getValidatorTestRedeemer (case BuyOrderT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @BuyOrderT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "BuyOrder_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpDelegationValidator tp) =
        getValidatorTestRedeemer (case DelegationT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @DelegationT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "Delegation_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpInvestUnitValidator tp) =
        getValidatorTestRedeemer (case InvestUnitT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @InvestUnitT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "InvestUnit_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptValidator (tpFundValidator tp) =
        getValidatorTestRedeemer (case FundT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @FundT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "Fund_" ++  x
                    Nothing -> Nothing)
     | scriptHash == OffChainHelpers.hashScriptValidator (tpFundHoldingValidator tp) =
        getValidatorTestRedeemer (case FundHoldingT.getValidatorRedeemerName (PlutusTx.fromBuiltinData @FundHoldingT.ValidatorRedeemer redeemer) of
                    Just x  -> Just $ "FundHolding_" ++ x
                    Nothing -> Nothing)
    | otherwise = Nothing

----------------------------------------------------------------------------------------

findMintingPolicyRedeemerNameMaybe :: TestParams -> Maybe LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe PolicyTestRedeemer
findMintingPolicyRedeemerNameMaybe tp scriptHashMaybe redeemer =
    case scriptHashMaybe of
            Just scriptHash ->findMintingPolicyRedeemerName tp scriptHash redeemer
            Nothing         -> Nothing

findMintingPolicyRedeemerName :: TestParams -> LedgerApiV2.ScriptHash -> LedgerApiV2.Redeemer -> Maybe PolicyTestRedeemer
findMintingPolicyRedeemerName tp scriptHash (LedgerApiV2.Redeemer redeemer)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpProtocolPolicyID tp) =
        Nothing
    | scriptHash == OffChainHelpers.hashScriptMinting (tpScriptPolicyID tp) =
        getPolicyTestRedeemer (case ScriptT.getPolicyRedeemerName (PlutusTx.fromBuiltinData @ScriptT.PolicyRedeemer redeemer) of
                    Just x  -> Just $ "Script_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpSwapOfferPolicyID tp) =
        getPolicyTestRedeemer (case SwapOfferT.getPolicyRedeemerName (PlutusTx.fromBuiltinData @SwapOfferT.PolicyRedeemer redeemer) of
                    Just x  -> Just $ "SwapOffer_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpBuyOrderPolicyID tp) =
        getPolicyTestRedeemer (case BuyOrderT.getPolicyRedeemerName (PlutusTx.fromBuiltinData @BuyOrderT.PolicyRedeemer redeemer) of
                    Just x  -> Just $ "BuyOrder_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpDelegationPolicyID tp) =
        getPolicyTestRedeemer (case DelegationT.getPolicyRedeemerName (PlutusTx.fromBuiltinData @DelegationT.PolicyRedeemer redeemer) of
                    Just x  -> Just $ "Delegation_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpFundPolicy tp) =
        getPolicyTestRedeemer (case FundT.getPolicyRedeemerName (PlutusTx.fromBuiltinData @FundT.PolicyRedeemer redeemer) of
                    Just x  -> Just $ "Fund_" ++ x
                    Nothing -> Nothing)
    | scriptHash == OffChainHelpers.hashScriptMinting (tpFundHoldingPolicyID tp) =
        getPolicyTestRedeemer (case FundHoldingT.getPolicyRedeemerName (PlutusTx.fromBuiltinData @FundHoldingT.PolicyRedeemer redeemer) of
                    Just x  -> Just $ "FundHolding_" ++ x
                    Nothing -> Nothing)
    | otherwise = Nothing


--------------------------------------------------------------------------------
-- Auxiliary Functions
--------------------------------------------------------------------------------

calculateDepositCommissionsUsingMonths_ :: TestParams -> LedgerApiV2.POSIXTime -> Integer -> (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonths_ tp =
    FundHelpers.calculateDepositCommissionsUsingMonths
        (tpCommissions_Table_Numerator_1e6 tp)
        (tpDeadline tp)

calculateDepositCommissionsUsingMonths_Parametrizable :: TestParams -> FundT.FundDatumType -> LedgerApiV2.POSIXTime -> Integer -> (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonths_Parametrizable _ fundDatum date deposit =
    -- remainingMonths P.> 0 P.&&
    if remainingMonths + 1 Ptx.< Ptx.length commissionsTable then
        -- DebugTrace.trace ("calculateDepositCommissionsUsingMonths_Parametrizable: " P.++ P.show  (remainingMonths,commissionsTable)) $
            FundHelpers.calculateDepositCommissionsUsingMonths
                commissionsTable
                deadline
                date
                deposit
    else
        (deposit, 0, 0)  -- Return full deposit to user, no commissions
    where
        deadline = FundT.fdDeadline fundDatum
        commissionsTable = FundT.fdCommissions_Table_Numerator_1e6 fundDatum
        remainingMonths = FundHelpers.getRemainingMonths deadline date


calculateWithdrawCommissionsUsingMonths_ :: TestParams -> LedgerApiV2.POSIXTime -> Integer -> Integer -> (Integer, Integer, Integer)
calculateWithdrawCommissionsUsingMonths_ tp =
    FundHelpers.calculateWithdrawCommissionsUsingMonths
        (tpCommissions_Table_Numerator_1e6 tp)
        (tpDeadline tp)

calculateWithdrawCommissionsUsingMonths_Parametrizable :: TestParams -> FundT.FundDatumType -> LedgerApiV2.POSIXTime -> Integer -> Integer -> (Integer, Integer, Integer)
calculateWithdrawCommissionsUsingMonths_Parametrizable _ fundDatum date withdraw investUnit_Granularity =
    if Ptx.length commissionsTable Ptx.> (remainingMonths + 1) then
        -- DebugTrace.trace ("calculateDepositCommissionsUsingMonths_Parametrizable: " P.++ P.show  (remainingMonths,commissionsTable)) $
            let (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6) = FundHelpers.calculateWithdrawCommissionsUsingMonths
                    commissionsTable
                    deadline
                    date
                    withdraw investUnit_Granularity
            in
                debugTraceIf_ swTrace ("valid calculateDepositCommissions: " P.++ P.show  (date, deadline, remainingMonths, Ptx.length commissionsTable, commissionsTable, (withdraw, commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6)))
                    (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6)
    else
        debugTraceIf_ swTrace ("invalid calculateDepositCommissions: " P.++ P.show  (date, deadline, remainingMonths, Ptx.length commissionsTable, commissionsTable, withdraw))
            (0, withdraw, 0)  -- Return full withdraw to user, no commissions
    where
        deadline = FundT.fdDeadline fundDatum
        commissionsTable = FundT.fdCommissions_Table_Numerator_1e6 fundDatum
        remainingMonths = FundHelpers.getRemainingMonths deadline date


--------------------------------------------------------------------------------

mkFundHoldingID_TN :: Integer -> LedgerApiV2.TokenName
mkFundHoldingID_TN index =
    LedgerApiV2.TokenName $ T.fundHoldingID_TN_basename <> OnChainHelpers.intToBBS index

--------------------------------------------------------------------------------

mkValue_From_InvestUnit_And_Amount :: T.InvestUnit -> Integer -> LedgerApiV2.Value
mkValue_From_InvestUnit_And_Amount investUnit amount =
    let
        tokens = T.iuValues investUnit
        value = P.foldr (\(cs, tn, amount_) acc -> acc <> LedgerApiV2.singleton cs tn (amount_ * amount `divide` 100)) P.mempty tokens
    in
        value

----------------------------------------------------------------------------------------
