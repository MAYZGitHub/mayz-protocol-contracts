--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

{- |
Module      : TestUtils.Contracts.InitialData
Description : Mock Data
-}
module TestUtils.Contracts.InitialData where

-- Non-IOG imports

-- IOG imports
import qualified Ledger
import qualified Ledger.Ada as LedgerAda
import qualified Ledger.Crypto as LedgerCrypto
import qualified Ledger.Value as LedgerValue
import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import PlutusTx.Prelude

-- Project imports
import qualified Generic.OffChainHelpers as OffChainHelpers
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Constants as T
import qualified Protocol.Delegation.Types as DelegationT
import qualified Protocol.Fund.Helpers as FundHelpers
import qualified Protocol.Fund.Helpers as FundT
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.InvestUnit.Types as InvestUnitT
import qualified Protocol.Fund.Types as FundT
import qualified Protocol.OffChainHelpers as OffChainHelpers
import qualified Protocol.OnChainHelpers as OnChainHelpers
import qualified Protocol.Protocol.Types as ProtocolT
import qualified Protocol.SwapOffer.Types as SwapOfferT
import qualified Protocol.Types as T
import TestUtils.HelpersMAYZ
    ( calculateDepositCommissionsUsingMonths_,
      calculateDepositCommissionsUsingMonths_Parametrizable,
      calculateWithdrawCommissionsUsingMonths_,
      calculateWithdrawCommissionsUsingMonths_Parametrizable,
      mkFundHoldingID_TN )
import TestUtils.TypesMAYZ

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

minAdaForUTxOWithTokens :: Integer
minAdaForUTxOWithTokens = 3_100_000

minAdaProtocolDatum :: Integer
minAdaProtocolDatum = 3_200_000

minAdaFundDatum :: Integer
minAdaFundDatum = 3_300_000

minAdaIUDatum :: Integer
minAdaIUDatum = 3_400_000

minAdaFundHoldingDatum :: Integer
minAdaFundHoldingDatum = 20_000_000

minAdaSwapOfferDatum :: Integer
minAdaSwapOfferDatum = 3_600_000

minAdaDelegationDatum :: Integer
minAdaDelegationDatum = 3_600_000

minAdaScriptDatum :: Integer
minAdaScriptDatum = 50_000_000

toAlter_minAda :: Integer
toAlter_minAda = 16_222_000

toAlter_Value_Adding_SomeADA :: LedgerApiV2.Value
toAlter_Value_Adding_SomeADA = LedgerAda.lovelaceValueOf 10_000_000

--------------------------------------------------------------------------------

investUnit_Initial_Token_CS :: LedgerApiV2.CurrencySymbol
investUnit_Initial_Token_CS = "AF23"

investUnit_Initial_Token_TN :: LedgerApiV2.TokenName
investUnit_Initial_Token_TN = "tokenA"

investUnit_Initial_Token_Amount :: Integer
investUnit_Initial_Token_Amount = 500 -- este esta multiplicado por 100 para tener granularidad

investUnit_Initial_Token_Price :: Integer
investUnit_Initial_Token_Price = 20

investUnit_AfterReIdx_Token_CS :: LedgerApiV2.CurrencySymbol
investUnit_AfterReIdx_Token_CS = "4E8D"

investUnit_AfterReIdx_Token_TN :: LedgerApiV2.TokenName
investUnit_AfterReIdx_Token_TN = "tokenB"

investUnit_AfterReIdx_Token_Amount :: Integer
investUnit_AfterReIdx_Token_Amount = 1_000 -- este esta multiplicado por 100 para tener granularidad

investUnit_AfterReIdx_Token_Price :: Integer
investUnit_AfterReIdx_Token_Price = 10

investUnit_Initial :: T.InvestUnit
investUnit_Initial = T.InvestUnit [(investUnit_Initial_Token_CS, investUnit_Initial_Token_TN, investUnit_Initial_Token_Amount)]

investUnit_AfterReIdx :: T.InvestUnit
investUnit_AfterReIdx = T.InvestUnit [(investUnit_AfterReIdx_Token_CS, investUnit_AfterReIdx_Token_TN, investUnit_AfterReIdx_Token_Amount)]

--------------------------------------------------------------------------------

tokensReIdxPrice :: T.InvestUnit
tokensReIdxPrice =
    T.InvestUnit
        [ (investUnit_Initial_Token_CS, investUnit_Initial_Token_TN, investUnit_Initial_Token_Price)
        , (investUnit_AfterReIdx_Token_CS, investUnit_AfterReIdx_Token_TN, investUnit_AfterReIdx_Token_Price)
        ]

oracleReIdxData :: TestParams -> T.OracleReIdx_Data
oracleReIdxData tp = T.OracleReIdx_Data tokensReIdxPrice (tpReIdxDate tp)

oracleReIdxSignature :: TestParams -> Ledger.Signature
oracleReIdxSignature tp = LedgerCrypto.sign' (OnChainHelpers.oracleReIdxDataToBBS (oracleReIdxData tp)) (tpOraclePrivateKey tp)

tokenFTPrice1xe6 :: TestParams -> Integer -> T.InvestUnit
tokenFTPrice1xe6 tp token_FT_Price1xe6 =
    T.InvestUnit
        [ (tpFundPolicy_CS tp, tpFundFT_TN tp, token_FT_Price1xe6)
        ]

mkOracleData :: TestParams -> Integer -> LedgerApiV2.POSIXTime -> T.Oracle_Data
mkOracleData tp token_FT_Price1xe6 = T.Oracle_Data (tokenFTPrice1xe6 tp token_FT_Price1xe6)

mkOracleDataSignature :: TestParams -> T.Oracle_Data -> Ledger.Signature
mkOracleDataSignature tp oracleData = LedgerCrypto.sign' (OnChainHelpers.oracleDataToBBS oracleData) (tpOraclePrivateKey tp)

--------------------------------------------------------------------------------

deposit_MockData :: Integer
deposit_MockData = 200_000_000

withdraw_MockData :: Integer
withdraw_MockData = 50_000_000

withdraw_Commissions_MockData :: Integer
withdraw_Commissions_MockData = 10

delegation_MockData :: Integer
delegation_MockData = 1_000_000

--------------------------------------------------------------------------------

protocol_Datum_MockData :: TestParams -> LedgerApiV2.Datum
protocol_Datum_MockData tp = ProtocolT.mkDatum $ protocol_DatumType_MockData tp

protocol_DatumType_MockData :: TestParams -> ProtocolT.ProtocolDatumType
protocol_DatumType_MockData tp =
    ProtocolT.mkProtocol_DatumType
        (tpScriptPolicyID_CS tp) -- pdScriptPolicyID_CS
        (tpScriptValidator_Hash tp) -- pdScriptValidator_Hash
        (tpOraclePaymentPubKey tp) -- pdOraclePaymentPubKey
        T.oracleData_Valid_Time_aux -- pdOracleData_Valid_Time
        (tpProtocolAdmins tp) -- pdAdmins
        (tpDelegatorsAdmins tp) -- pdDelegatorsAdmins
        (tpTokenAdminPolicy_CS tp) -- pdTokenAdminPolicy_CS
        [tpFundCategory tp] -- fundCategories
        (tpFundLifeTime tp) -- pdFundLifeTime
        (LedgerValue.AssetClass (tpTokenMAYZ_CS tp, tpTokenMAYZ_TN tp)) -- pdTokenMAYZ_AC
        (tpRequiredMAYZForSwapOffer tp) -- pdRequiredMAYZForSwapOffers
        (tpRequiredMAYZForBuyOrder tp) -- pdRequiredMAYZForBuyOrders
        (tp_MinMaxDef_CommissionFund_PerYear_InBPx1e3 tp) -- pdCommissionFund_PerYear_InBPx1e3
        (tp_MinMaxDef_CommissionSwapOffer_InBPx1e3 tp) -- pdCommissionSwapOffer_InBPx1e3
        (tp_MinMaxDef_CommissionBuyOrder_InBPx1e3 tp) -- pdCommissionBuyOrder_InBPx1e3
        (tpShare_InBPx1e2_Protocol tp) -- pdShare_InBPx1e2_Protocol
        (tpShare_InBPx1e2_Managers tp) -- pdShare_InBPx1e2_Managers
        (tpShare_InBPx1e2_Delegators tp) -- pdShare_InBPx1e2_Delegators
        T.maxDepositAndWithdraw_aux -- pdMaxDepositAndWithdraw
        minAdaProtocolDatum -- pdMinADA

protocol_UTxO_MockData :: TestParams -> LedgerApiV2.TxOut
protocol_UTxO_MockData tp =
    let
        datum = protocol_Datum_MockData tp
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpProtocolValidator_Hash tp)
            (LedgerAda.lovelaceValueOf minAdaProtocolDatum <> LedgerApiV2.singleton (tpProtocolPolicyID_CS tp) T.protocolID_TN 1)
            (LedgerApiV2.OutputDatum datum)
            Nothing

--------------------------------------------------------------------------------

protocol_spend_UTxO_And_TxOutRef_MockData :: TestParams -> (LedgerApiV2.TxOut, LedgerApiV2.TxOutRef)
protocol_spend_UTxO_And_TxOutRef_MockData tp =
    ( LedgerApiV2.TxOut
        (Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash "a2") Nothing)
        (LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens)
        LedgerApiV2.NoOutputDatum
        Nothing
    , tpProtocolPolicyID_TxOutRef tp
    )

--------------------------------------------------------------------------------

fund_DatumType_MockData :: TestParams -> FundT.FundDatumType
fund_DatumType_MockData tp =
    FundT.mkFund_DatumType
        (tpFundPolicy_CS tp) -- fdFundPolicy_CS
        (tpFundFT_TN tp) -- fdFundFT_TN
        (tpFundValidator_Hash tp) -- fdFundValidator_Hash
        (tpFundHoldingPolicyID_CS tp) -- fdFundHoldingPolicyID_CS
        (tpFundHoldingValidator_Hash tp) -- fdFundHoldingValidator_Hash
        (tpInvestUnitValidator_Hash tp) -- fdInvestUnitValidator_Hash
        (tpFundAdmins tp) -- fdAdmins
        (tpTokenAdminPolicy_CS tp) -- fdTokenAdminPolicy_CS
        (ProtocolT.fcCategoryNumber $ tpFundCategory tp) -- fdFundCategoryNumber
        (tpBeginAt tp) -- fdBeginAt
        (tpDeadline tp) -- fdDeadline
        (tpClosedAt tp) -- fdClosedAt
        (tpCommission_PerYear_InBPx1e3 tp) -- fdCommissions_Table_Numerator_1e6
        (tpCommissions_Table_Numerator_1e6 tp) -- fdCommissions_Table_Numerator_1e6
        0 -- fdHoldingsCount
        0 -- fdHoldingsIndex
        T.maxDepositAndWithdraw_aux -- fdMaxDepositAndWithdraw
        (LedgerValue.AssetClass (tpTokenMAYZ_CS tp, tpTokenMAYZ_TN tp)) -- fdTokenMAYZ_AC
        (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp) -- fdRequiredMAYZ
        minAdaFundDatum -- fdMinADA

fund_DatumType_MockData_Parametrizable :: TestParams -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Maybe LedgerApiV2.POSIXTime -> Integer -> FundT.FundDatumType
fund_DatumType_MockData_Parametrizable tp beginDate deadlineDate closedAt commission_PerYear_InBPx1e3 =
    let
        ------------
        monthsRemainingPlusOne = FundHelpers.getRemainingMonths deadlineDate beginDate + 1
        -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120 000 000
        den = 120_000_000
        commissions_Table_Numerator_1e6 = [OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator $ OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den month | month <- [0 .. monthsRemainingPlusOne]]
    in
        ------------

        FundT.mkFund_DatumType
            (tpFundPolicy_CS tp) -- fdFundPolicy_CS
            (tpFundFT_TN tp) -- fdFundFT_TN
            (tpFundValidator_Hash tp) -- fdFundValidator_Hash
            (tpFundHoldingPolicyID_CS tp) -- fdFundHoldingPolicyID_CS
            (tpFundHoldingValidator_Hash tp) -- fdFundHoldingValidator_Hash
            (tpInvestUnitValidator_Hash tp) -- fdInvestUnitValidator_Hash
            (tpFundAdmins tp) -- fdAdmins
            (tpTokenAdminPolicy_CS tp) -- fdTokenAdminPolicy_CS
            (ProtocolT.fcCategoryNumber $ tpFundCategory tp) -- fdFundCategoryNumber
            beginDate
            deadlineDate
            closedAt
            commission_PerYear_InBPx1e3
            commissions_Table_Numerator_1e6
            0 -- fdHoldingsCount
            0 -- fdHoldingsIndex
            T.maxDepositAndWithdraw_aux -- fdMaxDepositAndWithdraw
            (LedgerValue.AssetClass (tpTokenMAYZ_CS tp, tpTokenMAYZ_TN tp)) -- fdTokenMAYZ_AC
            (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp) -- fdRequiredMAYZ
            minAdaFundDatum -- fdMinADA

fund_DatumType_MockData_Parametrizable2 :: TestParams -> Integer -> FundT.FundDatumType
fund_DatumType_MockData_Parametrizable2 tp num_FundHolding_UTxOs =
    FundT.mkFund_DatumType
        (tpFundPolicy_CS tp) -- fdFundPolicy_CS
        (tpFundFT_TN tp) -- fdFundFT_TN
        (tpFundValidator_Hash tp) -- fdFundValidator_Hash
        (tpFundHoldingPolicyID_CS tp) -- fdFundHoldingPolicyID_CS
        (tpFundHoldingValidator_Hash tp) -- fdFundHoldingValidator_Hash
        (tpInvestUnitValidator_Hash tp) -- fdInvestUnitValidator_Hash
        (tpFundAdmins tp) -- fdAdmins
        (tpTokenAdminPolicy_CS tp) -- fdTokenAdminPolicy_CS
        (ProtocolT.fcCategoryNumber $ tpFundCategory tp) -- fdFundCategoryNumber
        (tpBeginAt tp) -- fdBeginAt
        (tpDeadline tp) -- fdDeadline
        (tpClosedAt tp) -- fdClosedAt
        (tpCommission_PerYear_InBPx1e3 tp) -- fdCommissions_Table_Numerator_1e6
        (tpCommissions_Table_Numerator_1e6 tp) -- fdCommissions_Table_Numerator_1e6
        num_FundHolding_UTxOs -- fdHoldingsCount
        0 -- fdHoldingsIndex
        T.maxDepositAndWithdraw_aux -- fdMaxDepositAndWithdraw
        (LedgerValue.AssetClass (tpTokenMAYZ_CS tp, tpTokenMAYZ_TN tp)) -- fdTokenMAYZ_AC
        (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp) -- fdRequiredMAYZ
        minAdaFundDatum -- fdMinADA

fund_Datum_MockData :: TestParams -> LedgerApiV2.Datum
fund_Datum_MockData tp = FundT.mkDatum $ fund_DatumType_MockData tp

fund_UTxO_MockData :: TestParams -> LedgerApiV2.TxOut
fund_UTxO_MockData tp =
    let
        datum = fund_Datum_MockData tp
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpFundValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaFundDatum
                <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing

fund_UTxO_MockData_Parametrizable2 :: TestParams -> Integer -> LedgerApiV2.TxOut
fund_UTxO_MockData_Parametrizable2 tp num_FundHolding_UTxOs =
    let
        datum = FundT.mkDatum $ fund_DatumType_MockData_Parametrizable2 tp num_FundHolding_UTxOs
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpFundValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaFundDatum
                <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.fundID_TN 1
                <> LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) (ProtocolT.fcRequiredMAYZ $ tpFundCategory tp)
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing

fund_DatumType_With_Added_FundHolding_MockData :: TestParams -> FundT.FundDatumType
fund_DatumType_With_Added_FundHolding_MockData tp =
    (fund_DatumType_MockData tp)
        { FundT.fdHoldingsCount = 1
        , FundT.fdHoldingsIndex = 1
        }

fund_Datum_With_Added_FundHolding_MockData :: TestParams -> LedgerApiV2.Datum
fund_Datum_With_Added_FundHolding_MockData tp = FundT.mkDatum $ fund_DatumType_With_Added_FundHolding_MockData tp

fund_UTxO_With_Added_FundHolding_MockData :: TestParams -> LedgerApiV2.TxOut
fund_UTxO_With_Added_FundHolding_MockData tp =
    let
        datum = fund_Datum_With_Added_FundHolding_MockData tp
    in
        (fund_UTxO_MockData tp)
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum datum
            }

fund_UTxO_With_Added_FundHolding_MockData_Parametrizable :: TestParams -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Maybe LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.TxOut
fund_UTxO_With_Added_FundHolding_MockData_Parametrizable tp beginDate deadlineDate closedAt fundCommission_PerYear_InBPx1e3 =
    let
        datum = FundT.mkDatum $ FundT.mkUpdated_Fund_Datum_With_HoldingAdded (fund_DatumType_MockData_Parametrizable tp beginDate deadlineDate closedAt fundCommission_PerYear_InBPx1e3)
    in
        (fund_UTxO_MockData tp)
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum datum
            }

fund_DatumType_With_Deleted_FundHolding_MockData :: TestParams -> FundT.FundDatumType
fund_DatumType_With_Deleted_FundHolding_MockData tp =
    (fund_DatumType_With_Added_FundHolding_MockData tp)
        { FundT.fdHoldingsCount = FundT.fdHoldingsCount (fund_DatumType_With_Added_FundHolding_MockData tp) - 1
        }

fund_Datum_With_Deleted_FundHolding_MockData :: TestParams -> LedgerApiV2.Datum
fund_Datum_With_Deleted_FundHolding_MockData tp = FundT.mkDatum $ fund_DatumType_With_Deleted_FundHolding_MockData tp

fund_UTxO_With_Deleted_FundHolding_MockData :: TestParams -> LedgerApiV2.TxOut
fund_UTxO_With_Deleted_FundHolding_MockData tp =
    let
        datum = fund_Datum_With_Deleted_FundHolding_MockData tp
    in
        (fund_UTxO_With_Added_FundHolding_MockData tp)
            { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum datum
            }

-- | UTxO that is spent in order to mint the Fund ID.
fund_spend_UTxO_And_TxOutRef_MockData :: TestParams -> (LedgerApiV2.TxOut, LedgerApiV2.TxOutRef)
fund_spend_UTxO_And_TxOutRef_MockData tp =
    ( LedgerApiV2.TxOut
        (Ledger.pubKeyHashAddress (Ledger.PaymentPubKeyHash "a2") Nothing)
        (LedgerAda.lovelaceValueOf minAdaForUTxOWithTokens)
        LedgerApiV2.NoOutputDatum
        Nothing
    , tpFundPolicy_TxOutRef tp
    )

--------------------------------------------------------------------------------

fundHolding_DatumType_With_NoDeposits_MockData :: TestParams -> FundHoldingT.FundHoldingDatumType
fundHolding_DatumType_With_NoDeposits_MockData _ =
    FundHoldingT.mkFundHolding_DatumType
        0 -- hdFundHolding_Index
        0 -- hdSubTotal_FT_Minted_Accumulated
        0 -- hdSubTotal_FT_Minted
        0 -- hdSubTotal_FT_Commissions
        0 -- hdSubTotal_FT_Commissions_Total
        0 -- hdSubTotal_FT_Commissions_Release_PerMonth_1e6
        0 -- hdSubTotal_FT_Commissions_Collected_Protocol
        0 -- hdSubTotal_FT_Commissions_Collected_Managers
        0 -- hdSubTotal_FT_Commissions_Collected_Delegators
        minAdaFundHoldingDatum -- hdMinADA

fundHolding_Datum_With_NoDeposits_MockData :: TestParams -> LedgerApiV2.Datum
fundHolding_Datum_With_NoDeposits_MockData tp = FundHoldingT.mkDatum $ fundHolding_DatumType_With_NoDeposits_MockData tp

fundHolding_UTxO_With_NoDeposits_MockData :: TestParams -> LedgerApiV2.TxOut
fundHolding_UTxO_With_NoDeposits_MockData tp =
    let
        datum = fundHolding_Datum_With_NoDeposits_MockData tp
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpFundHoldingValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaFundHoldingDatum
                <> LedgerApiV2.singleton (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN 0) 1
            )
            (LedgerApiV2.OutputDatum datum)
            Nothing

--------------------------------------------------------------------------------

fundHolding_DatumType_With_Deposits_MockData :: TestParams -> FundHoldingT.FundHoldingDatumType
fundHolding_DatumType_With_Deposits_MockData tp =
    let
        (userFT_MockData, commissionsFT_MockData, depositCommsRPMNum1e6_MockData) = calculateDepositCommissionsUsingMonths_ tp (tpDepositDate tp) deposit_MockData
    in
        FundHelpers.mkUpdated_FundHolding_Datum_With_Deposit (fundHolding_DatumType_With_NoDeposits_MockData tp) deposit_MockData userFT_MockData commissionsFT_MockData depositCommsRPMNum1e6_MockData

fundHolding_Datum_With_Deposits_MockData :: TestParams -> LedgerApiV2.Datum
fundHolding_Datum_With_Deposits_MockData tp = FundHoldingT.mkDatum $ fundHolding_DatumType_With_Deposits_MockData tp

fundHolding_UTxO_With_Deposits_MockData :: TestParams -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Deposits_MockData tp =
    let
        (_, commissionsFT_MockData, _) = calculateDepositCommissionsUsingMonths_ tp (tpDepositDate tp) deposit_MockData
    in
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpFundHoldingValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaFundHoldingDatum
                <> LedgerApiV2.singleton investUnit_Initial_Token_CS investUnit_Initial_Token_TN ((deposit_MockData * investUnit_Initial_Token_Amount) `divide` 100)
                <> LedgerApiV2.singleton (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN 0) 1
                <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionsFT_MockData
            )
            (LedgerApiV2.OutputDatum (fundHolding_Datum_With_Deposits_MockData tp))
            Nothing

fundHolding_UTxO_With_Deposits_MockData_Parametrizable :: TestParams -> FundT.FundDatumType -> FundHoldingT.FundHoldingDatumType -> T.InvestUnit -> Integer -> Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp fundDatum fundHoldingDatum_In investUnit index deposit depositDate =
    -- DebugTrace.trace ("fundHolding_UTxO_With_Deposits_MockData_Parametrizable: " P.++ P.show  (deposit,depositDate)) $
    let
        --------------------
        (userFT, commissionsFT, commissions_FT_Release_PerMonth_1e6) = calculateDepositCommissionsUsingMonths_Parametrizable tp fundDatum depositDate deposit
        --------------------
        fundHoldingDatum_Control_With_Deposit =
            (FundHelpers.mkUpdated_FundHolding_Datum_With_Deposit fundHoldingDatum_In deposit userFT commissionsFT commissions_FT_Release_PerMonth_1e6)
                { FundHoldingT.hdFundHolding_Index = index
                }
        --------------------
        tokens_InvestUnit_Value = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 investUnit deposit
    in
        --------------------
        -- deadline = FundT.fdDeadline fundDatum
        -- remainingMonths = FundHelpers.getRemainingMonths deadline depositDate
        --------------------

        -- DebugTrace.trace ("fundHolding_UTxO_With_Deposits_MockData_Parametrizable: " P.++ P.show  (depositDate, deadline, remainingMonths, deposit, userFT, commissionsFT, commissions_FT_Release_PerMonth_1e6)) $
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpFundHoldingValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaFundHoldingDatum
                <> tokens_InvestUnit_Value
                <> LedgerApiV2.singleton (tpFundHoldingPolicyID_CS tp) (mkFundHoldingID_TN index) 1
                <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionsFT
            )
            (LedgerApiV2.OutputDatum (FundHoldingT.mkDatum fundHoldingDatum_Control_With_Deposit))
            Nothing

fundHolding_UTxO_With_Withdraw_MockData_Parametrizable :: TestParams -> FundT.FundDatumType -> LedgerApiV2.TxOut -> T.InvestUnit -> Integer -> LedgerApiV2.POSIXTime -> Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Withdraw_MockData_Parametrizable tp fundDatum fundHolding_UTxO_With_Deposit investUnit withdraw withdrawDate investUnit_Granularity =
    -- DebugTrace.trace ("fundHolding_UTxO_With_Withdraw_MockData_Parametrizable: " P.++ P.show  (withdraw,withdrawDate, fundHolding_UTxO_With_Deposit)) $
    let
        --------------------
        fundHoldingDatum_In = FundHoldingT.getFundHolding_DatumType_From_UTxO fundHolding_UTxO_With_Deposit
        --------------------
        (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6) = calculateWithdrawCommissionsUsingMonths_Parametrizable tp fundDatum withdrawDate withdraw investUnit_Granularity
        --------------------
        fundHoldingDatum_Control_With_Withdraw = FundHelpers.mkUpdated_FundHolding_Datum_With_Withdraw fundHoldingDatum_In withdraw commissionsForUserFTToGetBack commissions_FT_Release_PerMonth_1e6
        --------------------
        tokens_InvestUnit_Value = OffChainHelpers.mkValue_From_InvestUnit_And_Amount2 investUnit withdrawPlusCommissionsGetBack
        --------------------
        -- deadline = FundT.fdDeadline fundDatum
        -- remainingMonths = FundHelpers.getRemainingMonths deadline withdrawDate
        --------------------
        !valueFor_FT_CommissionsToGetBack = LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) commissionsForUserFTToGetBack
        --------------------
        fundHolding_UTxO_With_Withdraw =
            fundHolding_UTxO_With_Deposit
                { LedgerApiV2.txOutValue =
                    LedgerApiV2.txOutValue fundHolding_UTxO_With_Deposit
                        <> negate tokens_InvestUnit_Value
                        <> negate valueFor_FT_CommissionsToGetBack
                , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum (FundHoldingT.mkDatum fundHoldingDatum_Control_With_Withdraw)
                }
    in
        -- DebugTrace.trace ("fundHolding_UTxO_With_Withdraw_MockData_Parametrizable: " P.++ P.show  (withdrawDate, deadline, remainingMonths, withdraw, commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6))
        fundHolding_UTxO_With_Withdraw

--------------------------------------------------------------------------------

fundHolding_DatumType_With_Withdraw_MockData :: TestParams -> FundHoldingT.FundHoldingDatumType
fundHolding_DatumType_With_Withdraw_MockData tp =
    let
        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)
        (commissionsFTToGetBack_MockData, _, withdrawCommsRPMNum1e6_MockData) = calculateWithdrawCommissionsUsingMonths_ tp (tpWithdrawDate tp) withdraw_MockData investUnit_Granularity
    in
        FundHelpers.mkUpdated_FundHolding_Datum_With_Withdraw (fundHolding_DatumType_With_Deposits_MockData tp) withdraw_MockData commissionsFTToGetBack_MockData withdrawCommsRPMNum1e6_MockData

fundHolding_Datum_With_Withdraw_MockData :: TestParams -> LedgerApiV2.Datum
fundHolding_Datum_With_Withdraw_MockData tp = FundHoldingT.mkDatum $ fundHolding_DatumType_With_Withdraw_MockData tp

fundHolding_UTxO_With_Withdraw_MockData :: TestParams -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Withdraw_MockData tp =
    let
        !investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit (T.iuValues investUnit_Initial)
        (commissionsFTToGetBack_MockData, withdrawPlusCommissionsGetBack_MockData, _) = calculateWithdrawCommissionsUsingMonths_ tp (tpWithdrawDate tp) withdraw_MockData investUnit_Granularity
    in
        (fundHolding_UTxO_With_Deposits_MockData tp)
            { LedgerApiV2.txOutValue =
                LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                    <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-commissionsFTToGetBack_MockData)
                    <> LedgerApiV2.singleton investUnit_Initial_Token_CS investUnit_Initial_Token_TN (-((withdrawPlusCommissionsGetBack_MockData * investUnit_Initial_Token_Amount) `divide` 100))
            , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum (fundHolding_Datum_With_Withdraw_MockData tp)
            }

-------------------

fundHolding_UTxO_With_Collected_Protocol :: TestParams -> Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Collected_Protocol tp withdraw_Commissions_MockData' =
    let
        newDatumType = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission (fundHolding_DatumType_With_Deposits_MockData tp) withdraw_Commissions_MockData'
    in
        (fundHolding_UTxO_With_Deposits_MockData tp)
            { LedgerApiV2.txOutValue =
                LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                    <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-withdraw_Commissions_MockData')
            , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum newDatumType
            }

fundHolding_UTxO_With_Collected_Protocol_Parametrizable :: TestParams -> LedgerApiV2.TxOut -> Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Collected_Protocol_Parametrizable tp fundHolding_UTxO_With_Deposit withdraw =
    let
        --------------------
        fundHoldingDatum_In = FundHoldingT.getFundHolding_DatumType_From_UTxO fundHolding_UTxO_With_Deposit
        --------------------
        fundHolding_Datum_Out = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission fundHoldingDatum_In withdraw
        --------------------
        fundHolding_UTxO_Out =
            fundHolding_UTxO_With_Deposit
                { LedgerApiV2.txOutValue =
                    LedgerApiV2.txOutValue fundHolding_UTxO_With_Deposit
                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-withdraw)
                , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum (FundHoldingT.mkDatum fundHolding_Datum_Out)
                }
    in
        fundHolding_UTxO_Out

fundHolding_UTxO_With_Collected_Managers :: TestParams -> Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Collected_Managers tp withdraw_Commissions_MockData' =
    let
        newDatumType = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission (fundHolding_DatumType_With_Deposits_MockData tp) withdraw_Commissions_MockData'
    in
        (fundHolding_UTxO_With_Deposits_MockData tp)
            { LedgerApiV2.txOutValue =
                LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                    <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-withdraw_Commissions_MockData')
            , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum newDatumType
            }

fundHolding_UTxO_With_Collected_Managers_Parametrizable :: TestParams -> LedgerApiV2.TxOut -> Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Collected_Managers_Parametrizable tp fundHolding_UTxO_With_Deposit withdraw =
    let
        --------------------
        fundHoldingDatum_In = FundHoldingT.getFundHolding_DatumType_From_UTxO fundHolding_UTxO_With_Deposit
        --------------------
        fundHolding_Datum_Out = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission fundHoldingDatum_In withdraw
        --------------------
        fundHolding_UTxO_Out =
            fundHolding_UTxO_With_Deposit
                { LedgerApiV2.txOutValue =
                    LedgerApiV2.txOutValue fundHolding_UTxO_With_Deposit
                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-withdraw)
                , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum (FundHoldingT.mkDatum fundHolding_Datum_Out)
                }
    in
        fundHolding_UTxO_Out

fundHolding_UTxO_With_Collected_Delegators :: TestParams -> Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Collected_Delegators tp withdraw_Commissions_MockData' =
    let
        newDatumType = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission (fundHolding_DatumType_With_Deposits_MockData tp) withdraw_Commissions_MockData'
    in
        (fundHolding_UTxO_With_Deposits_MockData tp)
            { LedgerApiV2.txOutValue =
                LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
                    <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-withdraw_Commissions_MockData')
            , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum $ FundHoldingT.mkDatum newDatumType
            }

fundHolding_UTxO_With_Collected_Delegators_Parametrizable :: TestParams -> LedgerApiV2.TxOut -> Integer -> LedgerApiV2.TxOut
fundHolding_UTxO_With_Collected_Delegators_Parametrizable tp fundHolding_UTxO_With_Deposit withdraw =
    let
        --------------------
        fundHoldingDatum_In = FundHoldingT.getFundHolding_DatumType_From_UTxO fundHolding_UTxO_With_Deposit
        --------------------
        fundHolding_Datum_Out = FundHelpers.mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission fundHoldingDatum_In withdraw
        --------------------
        fundHolding_UTxO_Out =
            fundHolding_UTxO_With_Deposit
                { LedgerApiV2.txOutValue =
                    LedgerApiV2.txOutValue fundHolding_UTxO_With_Deposit
                        <> LedgerApiV2.singleton (tpFundPolicy_CS tp) (tpFundFT_TN tp) (-withdraw)
                , LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum (FundHoldingT.mkDatum fundHolding_Datum_Out)
                }
    in
        fundHolding_UTxO_Out

--------------------------------------------------------------------------------

investUnit_Datum_MockData :: TestParams -> LedgerApiV2.Datum
investUnit_Datum_MockData tp =
    InvestUnitT.mkDatum $
        InvestUnitT.mkInvestUnit_DatumType
            (tpFundPolicy_CS tp)
            investUnit_Initial
            minAdaIUDatum

investUnit_UTxO_MockData :: TestParams -> LedgerApiV2.TxOut
investUnit_UTxO_MockData tp =
    LedgerApiV2.TxOut
        (OffChainHelpers.addressValidator $ FundT.fdInvestUnitValidator_Hash (fund_DatumType_MockData tp))
        ( LedgerAda.lovelaceValueOf minAdaIUDatum
            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1
        )
        (LedgerApiV2.OutputDatum (investUnit_Datum_MockData tp))
        Nothing

investUnit_Datum_MockData_Parametrizable :: TestParams -> T.InvestUnit -> LedgerApiV2.Datum
investUnit_Datum_MockData_Parametrizable tp investUnit =
    InvestUnitT.mkDatum $
        InvestUnitT.mkInvestUnit_DatumType
            (tpFundPolicy_CS tp)
            investUnit
            minAdaIUDatum

investUnit_UTxO_MockData_Parametrizable :: TestParams -> T.InvestUnit -> LedgerApiV2.TxOut
investUnit_UTxO_MockData_Parametrizable tp investUnit =
    LedgerApiV2.TxOut
        (OffChainHelpers.addressValidator $ FundT.fdInvestUnitValidator_Hash (fund_DatumType_MockData tp))
        ( LedgerAda.lovelaceValueOf minAdaIUDatum
            <> LedgerApiV2.singleton (tpFundPolicy_CS tp) T.investUnitID_TN 1
        )
        (LedgerApiV2.OutputDatum (investUnit_Datum_MockData_Parametrizable tp investUnit))
        Nothing

investUnit_Datum_After_ReIdx_MockData :: TestParams -> LedgerApiV2.Datum
investUnit_Datum_After_ReIdx_MockData tp =
    InvestUnitT.mkDatum $
        InvestUnitT.mkInvestUnit_DatumType
            (tpFundPolicy_CS tp)
            investUnit_AfterReIdx
            minAdaIUDatum

--------------------------------------------------------------------------------

investUnit_UTxO_After_ReIdx_MockData :: TestParams -> LedgerApiV2.TxOut
investUnit_UTxO_After_ReIdx_MockData tp =
    (investUnit_UTxO_MockData tp)
        { LedgerApiV2.txOutDatum = LedgerApiV2.OutputDatum (investUnit_Datum_After_ReIdx_MockData tp)
        }

fundHolding_UTxO_After_Reidx_MockData :: TestParams -> T.InvestUnit -> T.InvestUnit -> LedgerApiV2.TxOut
fundHolding_UTxO_After_Reidx_MockData tp investUnit_Initial' investUnit_AfterReIdx' =
    let
        !total_Deposits_IU = FundHoldingT.hdSubTotal_FT_Minted (fundHolding_DatumType_With_Deposits_MockData tp)
        !valueOf_TotalTokensToAdd = OnChainHelpers.flattenValueToValue [(cs, tn, (am * total_Deposits_IU) `divide` 100) | (cs, tn, am) <- T.iuValues investUnit_AfterReIdx']
        !valueOf_TotalTokensToRemove = OnChainHelpers.flattenValueToValue [(cs, tn, (am * total_Deposits_IU) `divide` 100) | (cs, tn, am) <- T.iuValues investUnit_Initial']
        !valueOf_FundHoldingDatum_In = LedgerApiV2.txOutValue (fundHolding_UTxO_With_Deposits_MockData tp)
        valueFor_FundHoldingDatum_Control_WithTokensExchanged = valueOf_FundHoldingDatum_In <> valueOf_TotalTokensToAdd <> negate valueOf_TotalTokensToRemove
    in
        (fundHolding_UTxO_With_Deposits_MockData tp)
            { LedgerApiV2.txOutValue = valueFor_FundHoldingDatum_Control_WithTokensExchanged
            }

--------------------------------------------------------------------------------

swapOffer_DatumType_MockData :: TestParams -> SwapOfferT.SwapOffer_DatumType
swapOffer_DatumType_MockData tp =
    SwapOfferT.mkSwapOffer_DatumType
        (tpSwapOfferPolicyID_CS tp) -- swapOfferPolicyID_CS
        (tpFundPolicy_CS tp) -- fundPolicy_CS
        (tpSwapOfferAdmin tp) -- sellerPaymentPKH
        Nothing -- sellerStakePKH
        (ProtocolT.mmdDef $ ProtocolT.pdCommissionSwapOffer_InBPx1e3 $ protocol_DatumType_MockData tp) -- askedCommission_Rate_InBPx1e3
        50_000_000 -- amount_FT_Available
        50_000_000 -- amount_ADA_Available
        0 -- total_FT_Earned
        0 -- total_ADA_Earned
        T.swapOffer_AllowSell
        T.swapOffer_AllowSell
        T.swapOffer_Status_Open -- order_Status
        (LedgerValue.AssetClass (tpTokenMAYZ_CS tp, tpTokenMAYZ_TN tp)) -- tokenMAYZ_AC
        (ProtocolT.pdRequiredMAYZForSwapOffer $ protocol_DatumType_MockData tp)
        minAdaSwapOfferDatum -- minADA

swapOffer_Datum_MockData :: TestParams -> LedgerApiV2.Datum
swapOffer_Datum_MockData tp = SwapOfferT.mkDatum $ swapOffer_DatumType_MockData tp

swapOffer_UTxO_MockData :: TestParams -> LedgerApiV2.TxOut
swapOffer_UTxO_MockData tp =
    let
        !requiredMAYZ = ProtocolT.pdRequiredMAYZForSwapOffer $ protocol_DatumType_MockData tp
        ---------------------
        !valueOf_RequiredMAYZ = LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) requiredMAYZ
    in
        ---------------------
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpSwapOfferValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaSwapOfferDatum
                <> LedgerApiV2.singleton (tpSwapOfferPolicyID_CS tp) T.swapOfferID_TN 1
                <> LedgerApiV2.singleton
                    (tpFundPolicy_CS tp)
                    (tpFundFT_TN tp)
                    (SwapOfferT.sodAmount_FT_Available $ swapOffer_DatumType_MockData tp)
                <> valueOf_RequiredMAYZ
                <> LedgerAda.lovelaceValueOf
                    (SwapOfferT.sodAmount_ADA_Available $ swapOffer_DatumType_MockData tp)
            )
            (LedgerApiV2.OutputDatum $ swapOffer_Datum_MockData tp)
            Nothing

swapOffer_DatumType_MockData_Parametrizable :: TestParams -> Integer -> Integer -> SwapOfferT.SwapOffer_DatumType
swapOffer_DatumType_MockData_Parametrizable tp amount_FT_Available amount_ADA_Available =
    SwapOfferT.mkSwapOffer_DatumType
        (tpSwapOfferPolicyID_CS tp) -- swapOfferPolicyID_CS
        (tpFundPolicy_CS tp) -- fundPolicy_CS
        (tpSwapOfferAdmin tp) -- sellerPaymentPKH
        Nothing -- sellerStakePKH
        (ProtocolT.mmdDef $ ProtocolT.pdCommissionSwapOffer_InBPx1e3 $ protocol_DatumType_MockData tp) -- askedCommission_Rate_InBPx1e3
        amount_FT_Available
        amount_ADA_Available
        0 -- total_FT_Earned
        0 -- total_ADA_Earned
        T.swapOffer_AllowSell
        T.swapOffer_AllowSell
        T.swapOffer_Status_Open -- order_Status
        (LedgerValue.AssetClass (tpTokenMAYZ_CS tp, tpTokenMAYZ_TN tp)) -- tokenMAYZ_AC
        (ProtocolT.pdRequiredMAYZForSwapOffer $ protocol_DatumType_MockData tp)
        minAdaSwapOfferDatum -- minADA

swapOffer_Datum_MockData_Parametrizable :: TestParams -> Integer -> Integer -> LedgerApiV2.Datum
swapOffer_Datum_MockData_Parametrizable tp amount_FT_Available amount_ADA_Available = SwapOfferT.mkDatum $ swapOffer_DatumType_MockData_Parametrizable tp amount_FT_Available amount_ADA_Available

swapOffer_UTxO_MockData_Parametrizable :: TestParams -> Integer -> Integer -> LedgerApiV2.TxOut
swapOffer_UTxO_MockData_Parametrizable tp amount_FT_Available amount_ADA_Available =
    let
        !requiredMAYZ = ProtocolT.pdRequiredMAYZForSwapOffer $ protocol_DatumType_MockData tp
        ---------------------
        !valueOf_RequiredMAYZ = LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) requiredMAYZ
    in
        ---------------------
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpSwapOfferValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaSwapOfferDatum
                <> LedgerApiV2.singleton (tpSwapOfferPolicyID_CS tp) T.swapOfferID_TN 1
                <> LedgerApiV2.singleton
                    (tpFundPolicy_CS tp)
                    (tpFundFT_TN tp)
                    (SwapOfferT.sodAmount_FT_Available $ swapOffer_DatumType_MockData_Parametrizable tp amount_FT_Available amount_ADA_Available)
                <> valueOf_RequiredMAYZ
                <> LedgerAda.lovelaceValueOf
                    (SwapOfferT.sodAmount_ADA_Available $ swapOffer_DatumType_MockData_Parametrizable tp amount_FT_Available amount_ADA_Available)
            )
            (LedgerApiV2.OutputDatum $ swapOffer_Datum_MockData_Parametrizable tp amount_FT_Available amount_ADA_Available)
            Nothing

--------------------------------------------------------------------------------

delegation_DatumType_MockData :: TestParams -> DelegationT.Delegation_DatumType
delegation_DatumType_MockData tp =
    DelegationT.mkDelegation_DatumType
        (tpDelegationPolicyID_CS tp) -- delegationPolicyID_CS
        (tpFundPolicy_CS tp) -- fundPolicy_CS
        (tpDelegationAdmin tp) -- sellerPaymentPKH
        Nothing -- sellerStakePKH
        (LedgerValue.AssetClass (tpTokenMAYZ_CS tp, tpTokenMAYZ_TN tp)) -- tokenMAYZ_AC
        delegation_MockData -- delegated_MAYZ
        minAdaDelegationDatum -- minADA


-- delegationPolicyID_CS
--     fundPolicy_CS
--     delegatorPaymentPKH
--     delegatorStakePKH
--     tokenMAYZ_AC
--     delegated_MAYZ


delegation_Datum_MockData :: TestParams -> LedgerApiV2.Datum
delegation_Datum_MockData tp = DelegationT.mkDatum $ delegation_DatumType_MockData tp

delegation_UTxO_MockData :: TestParams -> LedgerApiV2.TxOut
delegation_UTxO_MockData tp =
    let
        !valueOf_MAYZ = LedgerApiV2.singleton (tpTokenMAYZ_CS tp) (tpTokenMAYZ_TN tp) delegation_MockData
    in
        ---------------------
        LedgerApiV2.TxOut
            (OffChainHelpers.addressValidator $ tpDelegationValidator_Hash tp)
            ( LedgerAda.lovelaceValueOf minAdaDelegationDatum
                <> LedgerApiV2.singleton (tpDelegationPolicyID_CS tp) T.delegationID_TN 1
                <> valueOf_MAYZ
                
            )
            (LedgerApiV2.OutputDatum $ delegation_Datum_MockData tp)
            Nothing

--------------------------------------------------------------------------------
