--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.Contracts.TxSpecs.FundHolding where

-- Non-IOG imports
import qualified Data.List                              as DataList
import qualified Prelude                                as P

-- IOG imports
import qualified Plutus.V2.Ledger.Api                   as LedgerApiV2
import           PlutusTx.Prelude

-- Project imports
import qualified Generic.Constants                      as T
import qualified Generic.OnChainHelpers                 as OnChainHelpers
import qualified Protocol.Fund.Helpers                  as FundHelpers
import qualified Protocol.Fund.Holding.Types            as FundHoldingT
import qualified Protocol.Fund.Types                    as FundT
import qualified Protocol.Fund.InvestUnit.Types              as InvestUnitT
import qualified Protocol.OnChainHelpers                as OnChainHelpers
import qualified Protocol.Protocol.Types                as ProtocolT
import qualified Protocol.Types                         as T
import           TestUtils.Automatic.ContextGenerator
import           TestUtils.Automatic.HelpersMAYZ
import           TestUtils.Automatic.Types
import           TestUtils.Constants
import           TestUtils.Contracts.InitialData
import           TestUtils.Contracts.TxSpecs.InvestUnit
import           TestUtils.Helpers
import           TestUtils.HelpersMAYZ
import           TestUtils.Types
import           TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- FundHolding Contract
--------------------------------------------------------------------------------

fundHolding_Create_TxSpecs :: TestParams -> TxSpecs
fundHolding_Create_TxSpecs tp =
    let
        -----------------
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        consume_Fund_ValidRedeemerData =
            FundT.mkFundHoldingAddRedeemer
        consume_Fund_InvalidRedeemerData =
            Nothing
        consume_Fund_InvalidRedeemerType =
            Just FundT.mkFundHoldingDeleteRedeemer
        consume_Fund_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_Fund_UTxO_gen =
            consume_TxOut_Gen
                input_Fund_UTxO_gen
                consume_Fund_ValidRedeemerData
                consume_Fund_InvalidRedeemerData
                consume_Fund_InvalidRedeemerType
                consume_Fund_InvalidRedeemerNonExist
        -----------------
        output_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_With_Added_FundHolding_MockData tp) Fund_TestEntity op
        -----------------
        output_FundHolding_UTxO_gen op _ = txOut_With_TestEntity_Gen
                    tp
                    (fundHolding_UTxO_With_NoDeposits_MockData tp)
                    FundHolding_TestEntity
                    op
        -----------------
        mint_FundHoldingID_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (FundHoldingID_TestToken, FundHolding_MintID_TestRedeemer) 1
                op

        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = []
            , tsInputsFromWallet = []
            , tsInputs =
                [
                    ( Fund_TestEntity
                    , consume_Fund_UTxO_gen
                    , Fund_FundHoldingAdd_TestRedeemer
                    )
                ]
            , tsOutputs = [(Fund_TestEntity, output_Fund_UTxO_gen), (FundHolding_TestEntity, output_FundHolding_UTxO_gen)]
            , tsMints =
                [
                    ( FundHoldingID_TestToken
                    , mint_FundHoldingID_gen
                    , FundHolding_MintID_TestRedeemer
                    )
                ]
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

fundHolding_Delete_TxSpecs :: TestParams -> TxSpecs
fundHolding_Delete_TxSpecs tp =
    let
        -----------------
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_With_Added_FundHolding_MockData tp) Fund_TestEntity op
         -----------------
        consume_Fund_ValidRedeemerData =
            FundT.mkFundHoldingDeleteRedeemer
        consume_Fund_InvalidRedeemerData =
            Nothing
        consume_Fund_InvalidRedeemerType =
            Just FundT.mkFundHoldingAddRedeemer
        consume_Fund_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_Fund_UTxO_gen =
            consume_TxOut_Gen
                input_Fund_UTxO_gen
                consume_Fund_ValidRedeemerData
                consume_Fund_InvalidRedeemerData
                consume_Fund_InvalidRedeemerType
                consume_Fund_InvalidRedeemerNonExist
        -----------------
        output_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_With_Deleted_FundHolding_MockData tp) Fund_TestEntity op
        -----------------
        input_FundHolding_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (fundHolding_UTxO_With_NoDeposits_MockData tp)
                FundHolding_TestEntity
                op
        -----------------
        consume_FundHolding_ValidRedeemerData = FundHoldingT.mkDeleteRedeemer
        consume_FundHolding_InvalidRedeemerData = Nothing
        consume_FundHolding_InvalidRedeemerType =
            Just FundHoldingT.mkUpdateMinADARedeemer
        consume_FundHolding_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding_UTxO_gen =
            consume_TxOut_Gen
                input_FundHolding_UTxO_gen
                consume_FundHolding_ValidRedeemerData
                consume_FundHolding_InvalidRedeemerData
                consume_FundHolding_InvalidRedeemerType
                consume_FundHolding_InvalidRedeemerNonExist
        -----------------
        burn_FundHoldingID_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (FundHoldingID_TestToken, FundHolding_BurnID_TestRedeemer) 1
                op
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = []
            , tsInputsRefScripts = [uTxOForValidatorAsReference tp (tpFundValidator tp), uTxOForValidatorAsReference tp (tpFundHoldingValidator tp), uTxOForMintingAsReference tp (tpFundHoldingPolicyID tp)]
            , tsInputsFromWallet = []
            , tsInputs =
                [
                    ( Fund_TestEntity
                    , consume_Fund_UTxO_gen
                    , Fund_FundHoldingAdd_TestRedeemer
                    ),
                    ( FundHolding_TestEntity
                    , consume_FundHolding_UTxO_gen
                    , FundHolding_Delete_TestRedeemer
                    )
                ]
            , tsOutputs = [(Fund_TestEntity, output_Fund_UTxO_gen)]
            , tsMints =
                [
                    ( FundHoldingID_TestToken
                    , burn_FundHoldingID_gen
                    , FundHolding_BurnID_TestRedeemer
                    )
                ]
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = []
            }

--------------------------------------------------------------------------------

fundHolding_UpdateMinADA_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fundHolding_UpdateMinADA_TxSpecs tp txParams =
    let -----------------
        newMinADA = getTxParam "newMinADA" txParams :: Integer
        -----------------
        input_Fund_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (fund_UTxO_MockData tp) Fund_TestEntity op
        -----------------
        input_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        fundHolding_Input_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO input_FundHolding_UTxO
        -----------------
        input_FundHolding_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                input_FundHolding_UTxO
                FundHolding_TestEntity
                op
        -----------------
        fundHolding_Updated_DatumType =
            FundHelpers.mkUpdated_FundHolding_Datum_With_MinADAChanged
                fundHolding_Input_Datum
                newMinADA
        -----------------
        origValue = LedgerApiV2.txOutValue input_FundHolding_UTxO
        newAmount = FundHoldingT.hdMinADA fundHolding_Updated_DatumType
        -----------------
        output_FundHolding_UTxO =
            input_FundHolding_UTxO
                { LedgerApiV2.txOutDatum =
                    LedgerApiV2.OutputDatum $
                        FundHoldingT.mkDatum fundHolding_Updated_DatumType
                , LedgerApiV2.txOutValue =
                    changeValue_Amount
                        origValue
                        OnChainHelpers.adaAssetClass
                        newAmount
                }
        -----------------
        output_FundHolding_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                output_FundHolding_UTxO
                FundHolding_TestEntity
                op
        -----------------
        consume_FundHolding_ValidRedeemerData = FundHoldingT.mkUpdateMinADARedeemer
        consume_FundHolding_InvalidRedeemerData = Nothing
        consume_FundHolding_InvalidRedeemerType = Just FundHoldingT.mkDeleteRedeemer
        consume_FundHolding_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding_UTxO_gen =
            consume_TxOut_Gen
                input_FundHolding_UTxO_gen
                consume_FundHolding_ValidRedeemerData
                consume_FundHolding_InvalidRedeemerData
                consume_FundHolding_InvalidRedeemerType
                consume_FundHolding_InvalidRedeemerNonExist
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = []
            , tsInputs =
                [
                    ( FundHolding_TestEntity
                    , consume_FundHolding_UTxO_gen
                    , FundHolding_UpdateMinADA_TestRedeemer
                    )
                ]
            , tsInputsFromWallet = []
            , tsOutputs = [(FundHolding_TestEntity, output_FundHolding_UTxO_gen)]
            , tsMints = []
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [("Valid Min ADA positive", True), ("Invalid Min ADA negative", True)]
            }

--------------------------------------------------------------------------------

fundHolding_Deposit_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fundHolding_Deposit_TxSpecs tp txParams =
    let
        -----------------
        beginDate = getTxParam "beginDate" txParams :: LedgerApiV2.POSIXTime
        deadlineDate = getTxParam "deadlineDate" txParams :: LedgerApiV2.POSIXTime
        fundCommission_PerYear_InBPx1e3 = getTxParam "fundCommission_PerYear_InBPx1e3" txParams :: Integer
        depositDate = getTxParam "depositDate" txParams :: LedgerApiV2.POSIXTime
        depositAmount = getTxParam "depositAmount" txParams :: Integer
        investUnitTokens = getTxParam "investUnitTokens" txParams :: T.InvestUnit
        --------------------
        depositAmountSafe = if depositAmount < 1 then 1 else depositAmount
        --------------------
        input_Fund_UTxO extras =
            let
                depositDate' =
                    case DataList.find P.snd extras of
                        Just ("Valid DepositDate on BeginAt", _)  -> beginDate + T.validTxTimeRange
                        Just ("Valid DepositDate on Deadline", _) -> deadlineDate - T.validTxTimeRange
                        _                                         -> depositDate
                closedAt =
                    case DataList.find P.snd extras of
                        Just ("Invalid Deposit with Fund Closed", _) -> Just $ depositDate' - T.validTxTimeRange
                        _                                            -> Nothing
            in
                fund_UTxO_With_Added_FundHolding_MockData_Parametrizable tp beginDate deadlineDate closedAt fundCommission_PerYear_InBPx1e3
        -----------------
        input_Fund_Datum extras = FundT.getFund_DatumType_From_UTxO (input_Fund_UTxO extras)
        -----------------
        input_Fund_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_Fund_UTxO extras) Fund_TestEntity op
        -----------------
        input_InvestUnit_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (investUnit_UTxO_MockData_Parametrizable tp investUnitTokens) InvestUnit_TestEntity op
        -----------------
        input_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO (fundHolding_UTxO_With_NoDeposits_MockData tp)
        -----------------
        input_FundHolding_UTxO_gen op _ =
            txOut_With_TestEntity_Gen
                tp
                (fundHolding_UTxO_With_NoDeposits_MockData tp)
                FundHolding_TestEntity
                op
        -----------------
        consume_FundHolding_ValidRedeemerData depositDate'= FundHoldingT.mkDepositRedeemer depositDate' depositAmount
        consume_FundHolding_InvalidRedeemerData depositDate' = Just $ FundHoldingT.mkDepositRedeemer depositDate' (depositAmount + sum_ANY_INVALID_NUMBER)
        consume_FundHolding_InvalidRedeemerType =
            Just FundHoldingT.mkUpdateMinADARedeemer
        consume_FundHolding_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding_UTxO_gen op extras =
            let
                depositDate' =
                    case DataList.find P.snd extras of
                        Just ("Valid DepositDate on BeginAt", _)  -> beginDate + T.validTxTimeRange
                        Just ("Valid DepositDate on Deadline", _) -> deadlineDate - T.validTxTimeRange
                        _                                         -> depositDate
            in
                consume_TxOut_Gen
                    input_FundHolding_UTxO_gen
                    (consume_FundHolding_ValidRedeemerData depositDate')
                    (consume_FundHolding_InvalidRedeemerData depositDate')
                    consume_FundHolding_InvalidRedeemerType
                    consume_FundHolding_InvalidRedeemerNonExist
                    op
                    extras
        -----------------
        output_FundHolding_UTxO_gen op extras =
            let
                depositDate' =
                    case DataList.find P.snd extras of
                        Just ("Valid DepositDate on BeginAt", _)  -> beginDate + T.validTxTimeRange
                        Just ("Valid DepositDate on Deadline", _) -> deadlineDate - T.validTxTimeRange
                        _                                         -> depositDate
            in
                txOut_With_TestEntity_Gen tp (fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp (input_Fund_Datum extras) input_FundHolding_Datum investUnitTokens 0 depositAmountSafe depositDate') FundHolding_TestEntity op
        -----------------
        mint_FundFT_gen op _ =
            mint_Value_With_TestToken_Gen
                tp
                (FundFT_TestToken, Fund_MintFT_TestRedeemer) depositAmountSafe
                op
        -----------------
        validityRange_gen' op extras =
            let
                depositDate' =
                    case DataList.find P.snd extras of
                        Just ("Valid DepositDate on BeginAt", _)  -> beginDate + T.validTxTimeRange
                        Just ("Valid DepositDate on Deadline", _) -> deadlineDate - T.validTxTimeRange
                        _                                         -> depositDate
            in
                validityRange_gen tp depositDate' op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen), (InvestUnit_TestEntity, input_InvestUnit_UTxO_gen)]
            , tsInputsRefScripts = [uTxOForValidatorAsReference tp (tpFundHoldingValidator tp), uTxOForMintingAsReference tp (tpFundPolicy tp)]
            , tsInputsFromWallet = []
            , tsInputs =
                [
                    ( FundHolding_TestEntity
                    , consume_FundHolding_UTxO_gen
                    , FundHolding_Delete_TestRedeemer
                    )
                ]
            , tsOutputs = [(FundHolding_TestEntity, output_FundHolding_UTxO_gen)]
            , tsMints =
                [
                    ( FundFT_TestToken
                    , mint_FundFT_gen
                    , Fund_MintFT_TestRedeemer
                    )
                ]
            , tsUseSignatures = Nothing
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [
                ("Valid Deposit", True),
                ("Deposit very small", True),
                ("Deposit max", True),
                ("Deposit with 0% commission rate", True),
                ("Deposit with max commission rate", True),
                ("Invalid quantity of Tokens in Invest Unit", True),
                ("Invalid Deposit amount <= 0", True),
                ("Invalid Deposit amount > MAX", True),
                ("Invalid Deposit amount not multiplier of Invest Unit granularity", True),
                ("Valid Deposit with MinLifeTime", True),
                ("Valid DepositDate on BeginAt", False),
                ("Invalid DepositDate too early", True),
                ("Valid DepositDate on Deadline", False),
                ("Invalid DepositDate too late", True),
                ("Invalid Deposit with Fund Closed", False)
                ]
            }

--------------------------------------------------------------------------------

fundHolding_Withdraw_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fundHolding_Withdraw_TxSpecs tp txParams =
    let
        -----------------
        beginDate = getTxParam "beginDate" txParams :: LedgerApiV2.POSIXTime
        deadlineDate = getTxParam "deadlineDate" txParams :: LedgerApiV2.POSIXTime
        fundCommission_PerYear_InBPx1e3 = getTxParam "fundCommission_PerYear_InBPx1e3" txParams :: Integer
        depositDate = getTxParam "depositDate" txParams :: LedgerApiV2.POSIXTime
        depositAmount = getTxParam "depositAmount" txParams :: Integer
        withdrawDate_ = getTxParam "withdrawDate" txParams :: LedgerApiV2.POSIXTime
        withdrawAmount = getTxParam "withdrawAmount" txParams :: Integer
        investUnitTokens = getTxParam "investUnitTokens" txParams :: T.InvestUnit
        --------------------
        withdrawAmountSafe = if withdrawAmount < 1 then 1 else withdrawAmount
        --------------------
        -- FundHolding base para comenzar a editar
        base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO (fundHolding_UTxO_With_NoDeposits_MockData tp)
        -----------------
        getWithdrawDate extras =
            case DataList.find P.snd extras of
                Just ("Valid WithdrawDate on BeginAt", _)  -> beginDate + T.validTxTimeRange
                Just ("Valid WithdrawDate on Deadline", _) -> deadlineDate - T.validTxTimeRange
                _                                          -> withdrawDate_
        -----------------
        input_Fund_UTxO extras =
            let
                closedAt =
                    case DataList.find P.snd extras of
                        Just ("Valid Withdraw with Fund Closed", _) -> Just $ getWithdrawDate extras - T.validTxTimeRange
                        _                                           -> Nothing
            in
                fund_UTxO_With_Added_FundHolding_MockData_Parametrizable tp beginDate deadlineDate closedAt fundCommission_PerYear_InBPx1e3
        -----------------
        input_Fund_Datum extras = FundT.getFund_DatumType_From_UTxO (input_Fund_UTxO extras)
        -----------------
        input_Fund_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_Fund_UTxO extras) Fund_TestEntity op
        -----------------
        input_InvestUnit_UTxO_gen op _ =
            txOut_With_TestEntity_Gen tp (investUnit_UTxO_MockData_Parametrizable tp investUnitTokens) InvestUnit_TestEntity op
        -----------------
        input_FundHolding_UTxO extras = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp (input_Fund_Datum extras) base_FundHolding_Datum investUnitTokens 0 depositAmount depositDate
        -----------------
        input_FundHolding_UTxO_gen op extras = txOut_With_TestEntity_Gen tp (input_FundHolding_UTxO extras ) FundHolding_TestEntity op
        -----------------
        consume_FundHolding_ValidRedeemerData withdrawDate withdrawPlusCommissionsGetBack = FundHoldingT.mkWithdrawRedeemer withdrawDate withdrawAmount withdrawPlusCommissionsGetBack
        consume_FundHolding_InvalidRedeemerData withdrawDate withdrawPlusCommissionsGetBack = Just $ FundHoldingT.mkWithdrawRedeemer withdrawDate withdrawAmount (withdrawPlusCommissionsGetBack + sum_ANY_INVALID_NUMBER)
        consume_FundHolding_InvalidRedeemerType = Just FundHoldingT.mkUpdateMinADARedeemer
        consume_FundHolding_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding_UTxO_gen op extras =
            let
                !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnitTokens)
                (_, withdrawPlusCommissionsGetBack, _) = calculateWithdrawCommissionsUsingMonths_Parametrizable tp (input_Fund_Datum extras) (getWithdrawDate extras) withdrawAmountSafe investUnit_Granularity
            in
                consume_TxOut_Gen
                    input_FundHolding_UTxO_gen
                    (consume_FundHolding_ValidRedeemerData (getWithdrawDate extras) withdrawPlusCommissionsGetBack)
                    (consume_FundHolding_InvalidRedeemerData (getWithdrawDate extras) withdrawPlusCommissionsGetBack)
                    consume_FundHolding_InvalidRedeemerType
                    consume_FundHolding_InvalidRedeemerNonExist
                    op
                    extras
        -----------------
        output_FundHolding_UTxO_gen op extras =
            let
                !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnitTokens)
            in
                txOut_With_TestEntity_Gen tp (fundHolding_UTxO_With_Withdraw_MockData_Parametrizable tp (input_Fund_Datum extras) (input_FundHolding_UTxO extras) investUnitTokens withdrawAmountSafe (getWithdrawDate extras) investUnit_Granularity) FundHolding_TestEntity op
        -----------------
        burn_FundFT_gen op extras =
            let
                !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnitTokens)
                (_, withdrawPlusCommissionsGetBack, _) = calculateWithdrawCommissionsUsingMonths_Parametrizable tp (input_Fund_Datum extras) (getWithdrawDate extras) withdrawAmountSafe investUnit_Granularity
            in
                mint_Value_With_TestToken_Gen
                    tp
                    (FundFT_TestToken, Fund_BurnFT_TestRedeemer) withdrawPlusCommissionsGetBack
                    op
        -----------------
        validityRange_gen' op extras = validityRange_gen tp (getWithdrawDate extras) op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen), (InvestUnit_TestEntity, input_InvestUnit_UTxO_gen)]
            , tsInputsRefScripts = [uTxOForValidatorAsReference tp (tpFundHoldingValidator tp), uTxOForMintingAsReference tp (tpFundPolicy tp)]
            , tsInputsFromWallet = []
            , tsInputs =
                [
                    ( FundHolding_TestEntity
                    , consume_FundHolding_UTxO_gen
                    , FundHolding_Delete_TestRedeemer
                    )
                ]
            , tsOutputs = [(FundHolding_TestEntity, output_FundHolding_UTxO_gen)]
            , tsMints =
                [
                    ( FundFT_TestToken
                    , burn_FundFT_gen
                    , Fund_BurnFT_TestRedeemer
                    )
                ]
            , tsUseSignatures = Nothing
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [
                ("Valid WithdrawDate on BeginAt", False),
                ("Valid WithdrawDate on Deadline", False),
                ("Valid Withdraw with Fund Closed", False),
                ("Valid Withdraw", True),
                ("Withdraw very small", True),
                ("Withdraw max", True),
                ("Withdraw with 0% commission rate", True),
                ("Withdraw with max commission rate", True),
                ("Invalid quantity of Tokens in Invest Unit", True),
                ("Invalid Withdraw amount <= 0", True),
                ("Invalid Withdraw amount > MAX", True),
                ("Invalid Withdraw amount not multiplier of Invest Unit granularity", True),
                ("Valid Withdraw with MinLifeTime", True),
                ("Valid WithdrawDate too late", True),
                ("Invalid Withdraw Commissions amount", True),
                ("Invalid Withdraw amount more than deposit", True)
                ]
            }

--------------------------------------------------------------------------------

fundHolding_Collect_Protocol_Commission_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fundHolding_Collect_Protocol_Commission_TxSpecs tp txParams =
    let
        --------------------------
        -- UTxO Mock Data --
        input_Protocol_UTxO _extras = protocol_UTxO_MockData tp
        input_Fund_UTxO _extras = fund_UTxO_With_Added_FundHolding_MockData tp
        input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
        base_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        withdrawDate = tpCollectCommissionsDate tp
        --------------------------
        input_Protocol_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_Protocol_UTxO extras) Protocol_TestEntity op
        -----------------
        input_Protocol_Datum extras = ProtocolT.getProtocol_DatumType_From_UTxO (input_Protocol_UTxO extras)
        -----------------
        input_Fund_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_Fund_UTxO extras) Fund_TestEntity op
        -----------------
        input_Fund_Datum extras = FundT.getFund_DatumType_From_UTxO (input_Fund_UTxO extras)
        -----------------
        input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
        --------------------
        input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
        --------------------
        base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO base_FundHolding_UTxO
        --------------------
        input_FundHolding_UTxO extras = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp (input_Fund_Datum extras) base_FundHolding_Datum input_InvestUnit 0 deposit_MockData (tpDepositDate tp)
        input_FundHolding_Datum extras = FundHoldingT.getFundHolding_DatumType_From_UTxO (input_FundHolding_UTxO extras)
        -----------------
        input_FundHolding_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_FundHolding_UTxO extras) FundHolding_TestEntity op
        -----------------
        withdraw extras =
            let
                deadline = FundT.fdDeadline (input_Fund_Datum extras)
                share = ProtocolT.pdShare_InBPx1e2_Protocol (input_Protocol_Datum extras)
                taken =  FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol (input_FundHolding_Datum extras)
                ------------------
                withdrawAmount = FundHelpers.getCommissionsAvailable deadline (input_FundHolding_Datum extras) share taken withdrawDate
                ------------------
            in withdrawAmount
        -----------------
        output_FundHolding_UTxO extras = fundHolding_UTxO_With_Collected_Protocol_Parametrizable tp (input_FundHolding_UTxO extras) (withdraw extras)
        -----------------
        output_FundHolding_UTxO_gen op extras =
             txOut_With_TestEntity_Gen tp (output_FundHolding_UTxO extras) FundHolding_TestEntity op
        -----------------
        consume_FundHolding_ValidRedeemerData extras = FundHoldingT.mkCollect_Protocol_CommissionRedeemer withdrawDate (withdraw extras)
        consume_FundHolding_InvalidRedeemerData = Just $ FundHoldingT.mkCollect_Protocol_CommissionRedeemer 0 0
        consume_FundHolding_InvalidRedeemerType = Just FundHoldingT.mkDeleteRedeemer
        consume_FundHolding_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding_UTxO_gen op extras =
            consume_TxOut_Gen
                input_FundHolding_UTxO_gen
                (consume_FundHolding_ValidRedeemerData extras)
                consume_FundHolding_InvalidRedeemerData
                consume_FundHolding_InvalidRedeemerType
                consume_FundHolding_InvalidRedeemerNonExist
                op
                extras
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpProtocolAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpCollectCommissionsDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Protocol_TestEntity, input_Protocol_UTxO_gen), (Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = [uTxOForValidatorAsReference tp (tpFundHoldingValidator tp)]
            , tsInputsFromWallet = []
            , tsInputs =
                [
                    ( FundHolding_TestEntity
                    , consume_FundHolding_UTxO_gen
                    , FundHolding_Collect_Protocol_Commission_TestRedeemer
                    )
                ]
            , tsOutputs = [(FundHolding_TestEntity, output_FundHolding_UTxO_gen)]
            , tsMints =
                [
                ]
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [
                ]
            }

--------------------------------------------------------------------------------

fundHolding_Collect_Managers_Commission_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fundHolding_Collect_Managers_Commission_TxSpecs tp txParams =
    let
        --------------------------
        -- UTxO Mock Data --
        input_Protocol_UTxO _extras = protocol_UTxO_MockData tp
        input_Fund_UTxO _extras = fund_UTxO_With_Added_FundHolding_MockData tp
        input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
        base_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        withdrawDate = tpCollectCommissionsDate tp
       --------------------------
        input_Protocol_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_Protocol_UTxO extras) Protocol_TestEntity op
        -----------------
        input_Protocol_Datum extras = ProtocolT.getProtocol_DatumType_From_UTxO (input_Protocol_UTxO extras)
        -----------------
        input_Fund_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_Fund_UTxO extras) Fund_TestEntity op
        -----------------
        input_Fund_Datum extras = FundT.getFund_DatumType_From_UTxO (input_Fund_UTxO extras)
        -----------------
        input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
        --------------------
        input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
        --------------------
        base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO base_FundHolding_UTxO
        --------------------
        input_FundHolding_UTxO extras = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp (input_Fund_Datum extras) base_FundHolding_Datum input_InvestUnit 0 deposit_MockData (tpDepositDate tp)
        input_FundHolding_Datum extras = FundHoldingT.getFundHolding_DatumType_From_UTxO (input_FundHolding_UTxO extras)
        -----------------
        input_FundHolding_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_FundHolding_UTxO extras) FundHolding_TestEntity op
        -----------------
        withdraw extras =
            let
                deadline = FundT.fdDeadline (input_Fund_Datum extras)
                share = ProtocolT.pdShare_InBPx1e2_Managers (input_Protocol_Datum extras)
                taken =  FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers (input_FundHolding_Datum extras)
                ------------------
                withdrawAmount = FundHelpers.getCommissionsAvailable deadline (input_FundHolding_Datum extras) share taken withdrawDate
                ------------------
            in withdrawAmount
        -----------------
        output_FundHolding_UTxO extras = fundHolding_UTxO_With_Collected_Managers_Parametrizable tp (input_FundHolding_UTxO extras) (withdraw extras)
        -----------------
        output_FundHolding_UTxO_gen op extras =
             txOut_With_TestEntity_Gen tp (output_FundHolding_UTxO extras) FundHolding_TestEntity op
        -----------------
        consume_FundHolding_ValidRedeemerData extras = FundHoldingT.mkCollect_Managers_CommissionRedeemer withdrawDate (withdraw extras)
        consume_FundHolding_InvalidRedeemerData = Just $ FundHoldingT.mkCollect_Managers_CommissionRedeemer 0 0
        consume_FundHolding_InvalidRedeemerType = Just FundHoldingT.mkDeleteRedeemer
        consume_FundHolding_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding_UTxO_gen op extras =
            consume_TxOut_Gen
                input_FundHolding_UTxO_gen
                (consume_FundHolding_ValidRedeemerData extras)
                consume_FundHolding_InvalidRedeemerData
                consume_FundHolding_InvalidRedeemerType
                consume_FundHolding_InvalidRedeemerNonExist
                op
                extras
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpCollectCommissionsDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Protocol_TestEntity, input_Protocol_UTxO_gen), (Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = [uTxOForValidatorAsReference tp (tpFundHoldingValidator tp)]
            , tsInputsFromWallet = []
            , tsInputs =
                [
                    ( FundHolding_TestEntity
                    , consume_FundHolding_UTxO_gen
                    , FundHolding_Collect_Managers_Commission_TestRedeemer
                    )
                ]
            , tsOutputs = [(FundHolding_TestEntity, output_FundHolding_UTxO_gen)]
            , tsMints =
                [
                ]
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [
                ]
            }
--------------------------------------------------------------------------------

fundHolding_Collect_Delegators_Commission_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fundHolding_Collect_Delegators_Commission_TxSpecs tp txParams =
    let
        --------------------------
        -- UTxO Mock Data --
        input_Protocol_UTxO _extras = protocol_UTxO_MockData tp
        input_Fund_UTxO _extras = fund_UTxO_With_Added_FundHolding_MockData tp
        input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
        base_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        withdrawDate = tpCollectCommissionsDate tp
       --------------------------
        input_Protocol_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_Protocol_UTxO extras) Protocol_TestEntity op
        -----------------
        input_Protocol_Datum extras = ProtocolT.getProtocol_DatumType_From_UTxO (input_Protocol_UTxO extras)
        -----------------
        input_Fund_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_Fund_UTxO extras) Fund_TestEntity op
        -----------------
        input_Fund_Datum extras = FundT.getFund_DatumType_From_UTxO (input_Fund_UTxO extras)
        -----------------
        input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
        --------------------
        input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
        --------------------
        base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO base_FundHolding_UTxO
        --------------------
        input_FundHolding_UTxO extras = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp (input_Fund_Datum extras) base_FundHolding_Datum input_InvestUnit 0 deposit_MockData (tpDepositDate tp)
        input_FundHolding_Datum extras = FundHoldingT.getFundHolding_DatumType_From_UTxO (input_FundHolding_UTxO extras)
        -----------------
        input_FundHolding_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_FundHolding_UTxO extras) FundHolding_TestEntity op
        -----------------
        withdraw extras =
            let
                deadline = FundT.fdDeadline (input_Fund_Datum extras)
                share = ProtocolT.pdShare_InBPx1e2_Delegators (input_Protocol_Datum extras)
                taken =  FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators (input_FundHolding_Datum extras)
                ------------------
                withdrawAmount = FundHelpers.getCommissionsAvailable deadline (input_FundHolding_Datum extras) share taken withdrawDate
                ------------------
            in withdrawAmount
        -----------------
        output_FundHolding_UTxO extras = fundHolding_UTxO_With_Collected_Delegators_Parametrizable tp (input_FundHolding_UTxO extras) (withdraw extras)
        -----------------
        output_FundHolding_UTxO_gen op extras =
             txOut_With_TestEntity_Gen tp (output_FundHolding_UTxO extras) FundHolding_TestEntity op
        -----------------
        consume_FundHolding_ValidRedeemerData extras = FundHoldingT.mkCollect_Delegators_CommissionRedeemer withdrawDate (withdraw extras)
        consume_FundHolding_InvalidRedeemerData = Just $ FundHoldingT.mkCollect_Delegators_CommissionRedeemer 0 0
        consume_FundHolding_InvalidRedeemerType = Just FundHoldingT.mkDeleteRedeemer
        consume_FundHolding_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding_UTxO_gen op extras =
            consume_TxOut_Gen
                input_FundHolding_UTxO_gen
                (consume_FundHolding_ValidRedeemerData extras)
                consume_FundHolding_InvalidRedeemerData
                consume_FundHolding_InvalidRedeemerType
                consume_FundHolding_InvalidRedeemerNonExist
                op
                extras
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpDelegatorsAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpCollectCommissionsDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Protocol_TestEntity, input_Protocol_UTxO_gen), (Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = [uTxOForValidatorAsReference tp (tpFundHoldingValidator tp)]
            , tsInputsFromWallet = []
            , tsInputs =
                [
                    ( FundHolding_TestEntity
                    , consume_FundHolding_UTxO_gen
                    , FundHolding_Collect_Delegators_Commission_TestRedeemer
                    )
                ]
            , tsOutputs = [(FundHolding_TestEntity, output_FundHolding_UTxO_gen)]
            , tsMints =
                [
                ]
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [
                ]
            }
--------------------------------------------------------------------------------

fundHolding_ReIndexing_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fundHolding_ReIndexing_TxSpecs  = investUnit_ReIndexing_TxSpecs

--------------------------------------------------------------------------------

fundHolding_BalanceAssets_TxSpecs :: TestParams -> [TxParam] -> TxSpecs
fundHolding_BalanceAssets_TxSpecs tp txParams =
 let
        --------------------------
        -- UTxO Mock Data --
        input_Fund_UTxO _ = fund_UTxO_With_Added_FundHolding_MockData tp
        input_InvestUnit_UTxO = investUnit_UTxO_MockData tp
        base_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        --------------------------
        input_Fund_Datum extras = FundT.getFund_DatumType_From_UTxO (input_Fund_UTxO extras)
        -----------------
        input_Fund_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_Fund_UTxO extras) Fund_TestEntity op
        -----------------
        input_InvestUnit_Datum = InvestUnitT.getInvestUnit_DatumType_From_UTxO input_InvestUnit_UTxO
        --------------------
        input_InvestUnit = InvestUnitT.iudInvestUnit input_InvestUnit_Datum
        --------------------
        base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO base_FundHolding_UTxO
        --------------------
        input_FundHolding1_UTxO extras = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp (input_Fund_Datum extras) base_FundHolding_Datum input_InvestUnit 0 deposit_MockData (tpDepositDate tp)
        input_FundHolding1_Datum extras = FundHoldingT.getFundHolding_DatumType_From_UTxO (input_FundHolding1_UTxO extras)
        input_FundHolding1_Value extras = LedgerApiV2.txOutValue (input_FundHolding1_UTxO extras)
        -----------------
        input_FundHolding2_UTxO extras = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp (input_Fund_Datum extras) base_FundHolding_Datum input_InvestUnit 1 deposit_MockData (tpDepositDate tp)
        input_FundHolding2_Datum extras = FundHoldingT.getFundHolding_DatumType_From_UTxO (input_FundHolding2_UTxO extras)
        input_FundHolding2_Value extras = LedgerApiV2.txOutValue (input_FundHolding2_UTxO extras)
        -----------------
        input_FundHolding1_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_FundHolding1_UTxO extras) FundHolding_TestEntity op
        -----------------
        input_FundHolding2_UTxO_gen op extras =
            txOut_With_TestEntity_Gen tp (input_FundHolding2_UTxO extras) FundHolding_TestEntity op
        -----------------
        output_FundHolding1_Datum = input_FundHolding1_Datum
        output_FundHolding1_UTxO extras = (input_FundHolding1_UTxO extras)
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    FundHoldingT.mkDatum (output_FundHolding1_Datum extras)
            , LedgerApiV2.txOutValue = input_FundHolding1_Value extras
            }
        -----------------
        output_FundHolding1_UTxO_gen op extras =
             txOut_With_TestEntity_Gen tp (output_FundHolding1_UTxO extras) FundHolding_TestEntity op
        -----------------
        output_FundHolding2_Datum = input_FundHolding2_Datum
        output_FundHolding2_UTxO extras = (input_FundHolding2_UTxO extras)
            { LedgerApiV2.txOutDatum =
                LedgerApiV2.OutputDatum $
                    FundHoldingT.mkDatum (output_FundHolding2_Datum extras)
            , LedgerApiV2.txOutValue = input_FundHolding2_Value extras
            }
        -----------------
        output_FundHolding2_UTxO_gen op extras =
             txOut_With_TestEntity_Gen tp (output_FundHolding2_UTxO extras) FundHolding_TestEntity op
        -----------------
        consume_FundHolding1_ValidRedeemerData = FundHoldingT.mkBalanceAssetsRedeemer [0,0]
        consume_FundHolding1_InvalidRedeemerData = Just $ FundHoldingT.mkBalanceAssetsRedeemer [1,-1]
        consume_FundHolding1_InvalidRedeemerType = Just FundHoldingT.mkDeleteRedeemer
        consume_FundHolding1_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding1_UTxO_gen op extras =
            consume_TxOut_Gen
                input_FundHolding1_UTxO_gen
                consume_FundHolding1_ValidRedeemerData
                consume_FundHolding1_InvalidRedeemerData
                consume_FundHolding1_InvalidRedeemerType
                consume_FundHolding1_InvalidRedeemerNonExist
                op
                extras
        -----------------
        consume_FundHolding2_ValidRedeemerData = FundHoldingT.mkBalanceAssetsRedeemer [0,0]
        consume_FundHolding2_InvalidRedeemerData = Just $ FundHoldingT.mkBalanceAssetsRedeemer [1,-1]
        consume_FundHolding2_InvalidRedeemerType = Just FundHoldingT.mkDeleteRedeemer
        consume_FundHolding2_InvalidRedeemerNonExist = Just fakeRedeemerEmpty
        -----------------
        consume_FundHolding2_UTxO_gen op extras =
            consume_TxOut_Gen
                input_FundHolding2_UTxO_gen
                consume_FundHolding2_ValidRedeemerData
                consume_FundHolding2_InvalidRedeemerData
                consume_FundHolding2_InvalidRedeemerType
                consume_FundHolding2_InvalidRedeemerNonExist
                op
                extras
        -----------------
        signatures_gen' op _ = signatures_gen tp (tpFundAdmins tp) op
        -----------------
        validityRange_gen' op _ = validityRange_gen tp (tpTransactionDate tp) op
     in -----------------
        TxSpecs
            { tsInputsRef = [(Fund_TestEntity, input_Fund_UTxO_gen)]
            , tsInputsRefScripts = [uTxOForValidatorAsReference tp (tpFundHoldingValidator tp)]
            , tsInputsFromWallet = []
            , tsInputs =
                [
                    ( FundHolding_TestEntity
                    , consume_FundHolding1_UTxO_gen
                    , FundHolding_BalanceAssets_TestRedeemer
                    ),
                    ( FundHolding_TestEntity
                    , consume_FundHolding2_UTxO_gen
                    , FundHolding_BalanceAssets_TestRedeemer
                    )
                ]
            , tsOutputs = [(FundHolding_TestEntity, output_FundHolding1_UTxO_gen),(FundHolding_TestEntity, output_FundHolding2_UTxO_gen)]
            , tsMints =
                [
                ]
            , tsUseSignatures = Just signatures_gen'
            , tsUseValidityRange = Just validityRange_gen'
            , tsExtras = [
                ]
            }

--------------------------------------------------------------------------------
