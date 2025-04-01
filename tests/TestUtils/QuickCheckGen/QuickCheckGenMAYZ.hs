--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module TestUtils.QuickCheckGen.QuickCheckGenMAYZ where

--------------------------------------------------------------------------------

-- Non-IOG imports

import qualified Control.Monad                         as ControlMonad
import qualified Debug.Trace                           as DebugTrace
import           Prelude                               as P hiding ((<>))
import qualified Test.QuickCheck                       as QC

-- IOG imports
import qualified Plutus.V2.Ledger.Api                  as LedgerApiV2
import           PlutusTx.Prelude                      (find, nub, remainder, sort)

-- Project imports

import qualified Data.List                             as DataList
import qualified Data.Maybe                            as DataMaybe
import qualified Ledger.Value                          as LedgerValue
import qualified Protocol.Fund.Helpers                 as FundHelpers
import qualified Protocol.Fund.Holding.Types           as FundHoldingT
import qualified Protocol.Fund.Types                   as FundT
import qualified Protocol.OnChainHelpers               as OnChainHelpers
import qualified Protocol.Protocol.Types               as ProtocolT
import qualified Protocol.Types                        as T
import           TestUtils.Contracts.InitialData
import           TestUtils.Helpers
import           TestUtils.QuickCheckGen.QuickCheckGen
import           TestUtils.TypesMAYZ


----------------------------------------------------------------------------------------

{- | Generates an arbitrary 'MinMaxDef' value.
     'MinMaxDef' is a generic type that captures a range with a minimum,
     maximum, and a default value. This instance will create random instances
     within a reasonable range for testing purposes.
-}
instance QC.Arbitrary a => QC.Arbitrary (ProtocolT.MinMaxDef a) where
    arbitrary = ((ProtocolT.MinMaxDef <$> QC.arbitrary) <*> QC.arbitrary) <*> QC.arbitrary

{- | Generates an arbitrary 'ValidatorHash'.
     'ValidatorHash' is a hash that uniquely identifies a validator script.
     This instance creates a random 'ValidatorHash' using the 'QC.Arbitrary'
     instance for 'ByteString'.
-}

-- | Generates an arbitrary 'FundCategory'.
instance QC.Arbitrary ProtocolT.FundCategory where
    arbitrary = ((ProtocolT.FundCategory <$> QC.arbitrary) <*> QC.arbitrary) <*> QC.arbitrary

instance QC.Arbitrary T.InvestUnit where
    arbitrary = genInvestUnit 10

----------------------------------------------------------------------------------------

genInvestUnit :: Integer -> QC.Gen T.InvestUnit
genInvestUnit maxQty = genInvestUnitWithGranularity 1 maxQty False False

genInvestUnitWithGranularity :: Integer -> Integer -> Bool ->  Bool -> QC.Gen T.InvestUnit
genInvestUnitWithGranularity qtyMin qtyMax useForcedGranularity doNotUseDecimalsWhenIsNotForcedGranularity = do
    -- Generate a random number of tokens within the specified range
    tokenCount <- QC.choose (qtyMin, qtyMax)
    -- Generate the specified number of tokens
    tokens <- QC.vectorOf (fromIntegral tokenCount) (genInvestUnitToken useForcedGranularity doNotUseDecimalsWhenIsNotForcedGranularity)
    return $ T.InvestUnit tokens

----------------------------------------------------------------------------------------

-- genInvestUnitTokensToAdd :: Bool -> Integer -> T.InvestUnit -> QC.Gen [T.InvestUnitToken]
-- genInvestUnitTokensToAdd validAmountRespectDivisibility totalFTMinted existingInvestUnit = do
--     let existingTokens = T.iuValues existingInvestUnit
--     -- Decide how many new tokens to add (0 to 3)
--     numNewTokens <- QC.choose (0, 3)
--     -- Generate new tokens
--     newTokens <- ControlMonad.replicateM numNewTokens $ do
--         cs <- QC.arbitrary :: QC.Gen LedgerApiV2.CurrencySymbol
--         tn <- QC.arbitrary :: QC.Gen LedgerApiV2.TokenName
--         val <- genValidAmount validAmountRespectDivisibility totalFTMinted
--         return (cs, tn, val)
--     -- Potentially modify existing tokens
--     modifiedExistingTokens <- mapM (modifyExistingToken validAmountRespectDivisibility totalFTMinted) existingTokens
--     -- Combine new and modified existing tokens
--     return $ newTokens ++ modifiedExistingTokens

-- genInvestUnitTokensToRemove :: Bool -> Integer -> T.InvestUnit -> QC.Gen [T.InvestUnitToken]
-- genInvestUnitTokensToRemove validAmountRespectDivisibility totalFTMinted investUnit = do
--     let tokens = T.iuValues investUnit
--     tokensToModify <- QC.sublistOf tokens
--     mapM (modifyExistingToken validAmountRespectDivisibility totalFTMinted) tokensToModify

-- genValidAmount :: Bool -> Integer -> QC.Gen Integer
-- genValidAmount validAmountRespectDivisibility totalFTMinted =
--     if validAmountRespectDivisibility
--     then QC.suchThat QC.arbitrary (\v -> v > 0 && (v * totalFTMinted) `rem` 100 == 0)
--     else QC.suchThat QC.arbitrary (> 0)

-- modifyExistingToken :: Bool -> Integer -> T.InvestUnitToken -> QC.Gen T.InvestUnitToken
-- modifyExistingToken validAmountRespectDivisibility totalFTMinted (cs, tn, currentAmount) = do
--     modifier <- genValidAmount validAmountRespectDivisibility totalFTMinted
--     increaseAmount <- QC.arbitrary
--     let newAmount = if increaseAmount
--                     then currentAmount + modifier
--                     else max 0 (currentAmount - modifier)
--     return (cs, tn, newAmount)

-- genTokenPrices :: [T.InvestUnitToken] -> QC.Gen [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
-- genTokenPrices = mapM (\(cs, tn, _) -> do
--         price <- QC.choose (1, 1_000_000)
--         return (cs, tn, price)
--     )

----------------------------------------------------------------------------------------

{-
Examples of how genInvestUnitToken generates amounts:

1. When useForcedGranularity is True:

   Example 1 (granularity = 10):
   - base = 42 (randomly chosen between 1 and 1,000,000)
   - granularity = 10
   - offset = 3 (randomly chosen between 1 and 9)
   - amount = (42 * 10) + 3 = 423
   This represents 42.3 in the actual token value.

   Example 2 (granularity = 100):
   - base = 785 (randomly chosen between 1 and 1,000,000)
   - granularity = 100
   - offset = 55 (randomly chosen between 1 and 99)
   - amount = (785 * 100) + 55 = 78555
   This represents 785.55 in the actual token value.

2. When useForcedGranularity is False:

   Example 3:
   - amount = 12345678 (randomly chosen between 1 and 100,000,000)
   This represents 123456.78 in the actual token value.

   Example 4:
   - amount = 100 (randomly chosen between 1 and 100,000,000)
   This represents 1.00 in the actual token value.

Key Points:
1. With forced granularity (True):
   - Amounts will always end in 0-9 (for granularity 10) or 00-99 (for granularity 100).
   - This ensures we have amounts with exactly 1 or 2 decimal places.

2. Without forced granularity (False):
   - Amounts can be any integer from 1 to 100,000,000.
   - This allows for a wider range of possibilities, including whole numbers and various decimal places.

3. Interpretation:
   - The generated amounts are always integers.
   - To get the actual token value, divide the amount by 100.
   - For example, an amount of 12345 represents 123.45 tokens.

4. Importance for testing:
   - Forced granularity helps test scenarios where tokens must have specific decimal place precision.
   - Non-forced granularity allows testing with a broader range of token amounts.
-}
genInvestUnitToken :: Bool -> Bool -> QC.Gen T.InvestUnitToken
genInvestUnitToken useForcedGranularity doNotUseDecimalsWhenIsNotForcedGranularity = do
    cs <- genCurrencySymbol
    tn <- genTokenName
    amount <- if useForcedGranularity
              then do
                -- When forced granularity is used, we ensure the amount
                -- is based on either 10 or 100 as the smallest unit.
                -- This simulates tokens with 1 or 2 decimal places.
                -- The resulting amount will be of the form:
                -- (n * granularity) + offset
                -- where offset < granularity
                base <- QC.choose (1, 1_000_000)
                granularity <- QC.elements [10, 100]
                offset <- QC.choose (1, granularity - 1)
                return (base * granularity + offset)
              else
                if doNotUseDecimalsWhenIsNotForcedGranularity then do
                    -- cuando quiero numeros multiplos de 100 solamente
                    base <- QC.choose (1, 1_000_000)
                    return (base * 100)
                else
                    -- Without forced granularity, we allow any integer amount
                    QC.choose (1, 100_000_000)
    return (cs, tn, amount)

----------------------------------------------------------------------------------------


{- | Generates arbitrary prices for two invest units representing tokens to add
     remove respectively, such that the total price in Ada is preserved.

     PRE-CONDITION: The first asset in the second IU must not be present in the
     assets of the first IU, since it acts as a "balancer" for the total prices.
     Also, the first amount of the second IU must be 1 so it can always balance.
-}
getRandomPrice :: T.InvestUnit -> T.InvestUnit -> QC.Gen T.InvestUnit
getRandomPrice iu1 iu2 = do
    let assets = nub $ map (\(cs, tn, _) -> (cs, tn))
                           (T.iuValues iu1 ++ tail (T.iuValues iu2))
    prices <- ControlMonad.mapM
                (\(cs, tn) -> do
                    price <- QC.arbitrary `QC.suchThat` (/= 0) :: QC.Gen Integer
                    return (cs, tn, abs price)
                )
                assets
    let priceAda1 = sum (findPriceADA prices <$> T.iuValues iu1)
        priceAda2 = sum (findPriceADA prices <$> tail (T.iuValues iu2))
        balancingPrice = let (cs, tn, _) = head (T.iuValues iu2)
                         in (cs, tn, priceAda1 - priceAda2)
    return $ T.InvestUnit $ prices ++ [balancingPrice]
  where
    findPriceADA prices' (cs, tn, amt) =
        case find (\(cs', tn', _) -> cs' == cs && tn' == tn) prices' of
            Nothing            -> 0
            Just (_, _, price) -> amt * price

--------------------------------------------------------------------------------




-- -- | Generates an arbitrary 'ProtocolDatumType'.
-- instance QC.Arbitrary ProtocolT.ProtocolDatumType where
--     arbitrary =
--         ProtocolT.ProtocolDatumType
--             <$> (return 2 :: QC.Gen Integer) -- pdProtocolVersion
--             <*> QC.arbitrary -- pdScriptPolicyID_CS
--             <*> QC.arbitrary -- pdScriptValidator_Hash
--             <*> QC.arbitrary -- pdOraclePaymentPubKey
--             <*> ( do
--                     admins <- QC.listOf1 arbitrary :: QC.Gen [LedgerApiV2.PubKeyHash]
--                     return $ sort admins -- pdAdmins
--                 )
--             <*> QC.arbitrary -- pdTokenAdminPolicy_CS
--             <*> (do sort <$> QC.arbitrary) -- pdFundCategories
--             <*> QC.arbitrary -- pdFundLifeTime
--             <*> QC.arbitrary -- pdRequiredMAYZForSwapOffers
--             <*> QC.arbitrary -- pdRequiredMAYZForBuyOrders
--             <*> QC.arbitrary -- pdCommissionFund_PerYear_InBPx1e3
--             <*> QC.arbitrary -- pdCommissionSwapOffer_InBPx1e3
--             <*> QC.arbitrary -- pdCommissionBuyOrder_InBPx1e3
--             <*> QC.arbitrary -- pdShare_InBPx1e2_Protocol
--             <*> QC.arbitrary -- pdShare_InBPx1e2_Managers
--             <*> QC.arbitrary -- pdShare_InBPx1e2_Delegators
--             <*> (do sort <$> QC.arbitrary) -- pdDelegatorsAdmins
--             <*> QC.arbitrary -- pdMinADA


-- Custom generator for valid ProtocolDatumType
genValidProtocolDatumType :: TestParams -> QC.Gen ProtocolT.ProtocolDatumType
genValidProtocolDatumType tp = do
    let
        protocolDatumType = protocol_DatumType_MockData tp

    admins <- ( do
                    pkh <- QC.listOf1 QC.arbitrary :: QC.Gen [LedgerApiV2.PubKeyHash]
                    -- DebugTrace.trace ("Generated admins: " ++ show pkh) $
                    return $ sort pkh
                )
    delegatorsAdmins <- ( do
                    pkh <- QC.listOf1 QC.arbitrary :: QC.Gen [LedgerApiV2.PubKeyHash]
                    return $ sort pkh
                )
    return $ protocolDatumType {ProtocolT.pdAdmins = admins, ProtocolT.pdDelegatorsAdmins = delegatorsAdmins}

-- instance QC.Arbitrary FundT.FundDatumType where
--     arbitrary = do
--         FundT.FundDatumType
--             <$> (return 2 :: QC.Gen Integer) -- fdFundVersion
--             <*> QC.arbitrary -- fdFundPolicy_CS
--             <*> QC.arbitrary -- fdFundFT_TN
--             <*> QC.arbitrary -- fdFundValidator_Hash
--             <*> QC.arbitrary -- fdFundHoldingPolicyID_CS
--             <*> QC.arbitrary -- fdFundHoldingValidator_Hash
--             <*> QC.arbitrary -- fdInvestUnitValidator_Hash
--             <*> ( do
--                     admins <- QC.listOf1 arbitrary :: QC.Gen [LedgerApiV2.PubKeyHash]
--                     return $ sort admins
--                 ) -- fdAdmins
--             <*> QC.arbitrary -- fdTokenAdminPolicy_CS
--             <*> QC.arbitrary -- fdFundCategoryNumber
--             <*> QC.arbitrary -- fdBeginAt
--             <*> QC.arbitrary -- fdDeadline
--             <*> QC.arbitrary -- fdClosedAt
--             <*> QC.arbitrary -- fdCommissions_Table_Numerator_1e6
--             <*> QC.arbitrary -- fdHoldingsCount
--             <*> QC.arbitrary -- fdHoldingsIndex
--             <*> QC.arbitrary -- fdMinADA


-- Custom generator for valid FundDatumType
genValidFundDatumType:: TestParams -> QC.Gen FundT.FundDatumType
genValidFundDatumType tp = do
    let
        fund_DatumType = fund_DatumType_MockData tp
    admins <- ( do
                pkh <- QC.listOf1 QC.arbitrary :: QC.Gen [LedgerApiV2.PubKeyHash]
                -- DebugTrace.trace ("Generated admins: " ++ show pkh) $
                return $ sort pkh
            )
    return $ fund_DatumType {FundT.fdAdmins = admins}

-- Custom generator for valid FundHoldingDatumType
genValidFundHoldingDatumType :: TestParams -> QC.Gen FundHoldingT.FundHoldingDatumType
genValidFundHoldingDatumType tp = do
    let
        fundHoldingDatumType = fundHolding_DatumType_With_NoDeposits_MockData tp
    return fundHoldingDatumType
    -- fundHolding_Index <- abs <$> QC.arbitrary
    -- subTotal_FT_Minted_Accumulated <- abs <$> QC.arbitrary
    -- subTotal_FT_Minted <- abs <$> QC.arbitrary
    -- subTotal_FT_Commissions <- abs <$> QC.arbitrary
    -- -- We multiply by 100 to get bigger values.
    -- subTotal_FT_Commissions_Acumulated <- abs . (* 100) <$> QC.arbitrary
    -- subTotal_FT_Commissions_Release_PerMonth_1e6 <- abs <$> QC.arbitrary
    -- -- We force the next three values for always giving a positive amount
    -- -- of available commissions at the given date and fund deadline.
    -- let
    --     fund_DatumType_ = fund_DatumType_MockData tp
    --     protocolDatumType_ = protocol_DatumType_MockData tp
    --     date = tpCollectCommissionsDate tp
    --     deadline = FundT.fdDeadline fund_DatumType_
    --     monthsRemainingRational = FundHelpers.getRemainingMonths deadline date
    --     rate = TxRatio.unsafeRatio subTotal_FT_Commissions_Release_PerMonth_1e6 1_000_000
    --     commisionsReady =
    --         TxRatio.fromInteger subTotal_FT_Commissions_Acumulated
    --             PlutusTx.- (TxRatio.fromInteger monthsRemainingRational * rate)
    -- subTotal_FT_Commissions_Collected_Protocol <- do
    --     let share = ProtocolT.pdShare_InBPx1e2_Protocol protocolDatumType_
    --         sharePct = TxRatio.unsafeRatio share 10_000
    --         commisionsReady_share = commisionsReady * sharePct
    --     QC.chooseInteger (0, TxRatio.truncate commisionsReady_share)
     -- subTotal_FT_Commissions_Collected_Managers <- do
    --     let share = ProtocolT.pdShare_InBPx1e2_Managers protocolDatumType_
    --         sharePct = TxRatio.unsafeRatio share 10_000
    --         commisionsReady_share = commisionsReady * sharePct
    --     QC.chooseInteger (0, TxRatio.truncate commisionsReady_share)
    -- subTotal_FT_Commissions_Collected_Delegators <- do
    --     let share = ProtocolT.pdShare_InBPx1e2_Delegators protocolDatumType_
    --         sharePct = TxRatio.unsafeRatio share 10_000
    --         commisionsReady_share = commisionsReady * sharePct
    --     QC.chooseInteger (0, TxRatio.truncate commisionsReady_share)
    -- FundHoldingT.FundHoldingDatumType
    --     fundHolding_Index
    --     subTotal_FT_Minted_Accumulated
    --     subTotal_FT_Minted
    --     subTotal_FT_Commissions
    --     subTotal_FT_Commissions_Acumulated
    --     subTotal_FT_Commissions_Release_PerMonth_1e6
    --     subTotal_FT_Commissions_Collected_Protocol
    --     subTotal_FT_Commissions_Collected_Managers
    --     subTotal_FT_Commissions_Collected_Delegators
    --     <$> (abs <$> QC.arbitrary)

------------------------------------------------------------------------------------

genValidFundHolding_UTxO_Parametrizable :: TestParams -> FundT.FundDatumType -> FundHoldingT.FundHoldingDatumType -> T.InvestUnit -> Integer -> LedgerApiV2.POSIXTime -> Bool -> QC.Gen LedgerApiV2.TxOut
genValidFundHolding_UTxO_Parametrizable tp fundDatum fundHoldingDatum_In investUnit@(T.InvestUnit investUnitTokens) index depositDate validGranularity = do
    let minDeposit = 1
    let maxDeposit = 10_000_000
    let investUnit_Granularity = OnChainHelpers.getDecimalsInInvestUnit investUnitTokens
    deposit <- if validGranularity then
        -- Generate a valid deposit amount
        -- It will be divisible by the InvestUnit's granularity
        do
            amount <- QC.choose (minDeposit `div` investUnit_Granularity, maxDeposit `div` investUnit_Granularity)
            return $ amount * investUnit_Granularity
    else
        -- Generate an invalid deposit amount
        -- It will not be divisible by the InvestUnit's granularity
        do
            baseAmount <- QC.choose (minDeposit `div` investUnit_Granularity, maxDeposit `div` investUnit_Granularity)
            invalidOffset <- QC.choose (1, investUnit_Granularity - 1)
            return $ (baseAmount * investUnit_Granularity) + invalidOffset
    let fundHolding_UTxO = fundHolding_UTxO_With_Deposits_MockData_Parametrizable tp fundDatum fundHoldingDatum_In investUnit index deposit depositDate
    return fundHolding_UTxO

------------------------------------------------------------------------------------

data Valid_Collect_Protocol_Commissions_Params
    = Valid_Collect_Protocol_Commissions_Params
          { getCPPFundHoldingDatumType :: FundHoldingT.FundHoldingDatumType
          , getCPPWithdrawAmount       :: Integer
          }
    deriving (Show)

genValid_Collect_Protocol_Commissions_Params :: TestParams -> QC.Gen Valid_Collect_Protocol_Commissions_Params
genValid_Collect_Protocol_Commissions_Params tp = do
    fundHoldingDatumType <- genValidFundHoldingDatumType tp
    let fund_DatumType_ = fund_DatumType_MockData tp
        protocolDatumType_ = protocol_DatumType_MockData tp
        date = tpCollectCommissionsDate tp
        deadline = FundT.fdDeadline fund_DatumType_
        share = ProtocolT.pdShare_InBPx1e2_Protocol protocolDatumType_
        availableWithoutTaken =
            FundHelpers.getCommissionsAvailable
                deadline
                fundHoldingDatumType
                share
                0
                date

    if availableWithoutTaken > 0 then do
        taken <- QC.chooseInteger (0, availableWithoutTaken - 1)
        let available = availableWithoutTaken - taken
        withdrawAmount <- QC.chooseInteger (1, available)
        return $ Valid_Collect_Protocol_Commissions_Params (fundHoldingDatumType { FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol = taken }) withdrawAmount
    else
        return $ Valid_Collect_Protocol_Commissions_Params (fundHoldingDatumType { FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol = 0 }) 0

    -- if availableWithoutTaken > 0
    -- then do
    --     taken <- QC.chooseInteger (0, availableWithoutTaken - 1)
    --     let available = availableWithoutTaken - taken
    --     withdrawAmount <- QC.chooseInteger (1, available)
    --     return $ Valid_Collect_Protocol_Commissions_Params (fundHoldingDatumType { FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol = taken }) withdrawAmount
    -- else
    --     return Nothing
    --     -- return $ Valid_Collect_Protocol_Commissions_Params (fundHoldingDatumType { FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol = 0 }) 0


------------------------------------------------------------------------------------

data Valid_Collect_Managers_Params
    = Valid_Collect_Managers_Params
          { getCFAPFundHoldingDatumType :: FundHoldingT.FundHoldingDatumType
          , getCFAPWithdrawAmount       :: Integer
          }
    deriving (Show)

genValid_Collect_Managers_Params :: TestParams -> QC.Gen Valid_Collect_Managers_Params
genValid_Collect_Managers_Params tp = do
        fundHoldingDatumType <- genValidFundHoldingDatumType tp
        let fund_DatumType_ = fund_DatumType_MockData tp
            protocolDatumType_ = protocol_DatumType_MockData tp
            date = tpCollectCommissionsDate tp
            deadline = FundT.fdDeadline fund_DatumType_
            share = ProtocolT.pdShare_InBPx1e2_Protocol protocolDatumType_
            availableWithoutTaken =
                FundHelpers.getCommissionsAvailable
                    deadline
                    fundHoldingDatumType
                    share
                    0
                    date
        -- DebugTrace.trace ("Testing availableWithoutTaken: " ++ show availableWithoutTaken) $
        if availableWithoutTaken > 0
            then do
                taken <- QC.chooseInteger (0, availableWithoutTaken - 1)
                let available = availableWithoutTaken - taken
                withdrawAmount <- QC.chooseInteger (1, available)
                DebugTrace.trace ("Testing taken: " ++ show taken) $
                    DebugTrace.trace ("Testing available: " ++ show available) $
                        DebugTrace.trace ("Testing withdrawAmount: " ++ show withdrawAmount) $
                                return $ Valid_Collect_Managers_Params  (fundHoldingDatumType { FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers = taken}) withdrawAmount
            else return $ Valid_Collect_Managers_Params  (fundHoldingDatumType { FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol = 0}) 0

------------------------------------------------------------------------------------

data Collect_Delegators_Params
    = Collect_Delegators_Params
          { getCMPFundHoldingDatumType :: FundHoldingT.FundHoldingDatumType
          , getCMPWithdrawAmount       :: Integer
          }
    deriving (Show)

genValid_Collect_Delegators_Params :: TestParams -> QC.Gen Collect_Delegators_Params
genValid_Collect_Delegators_Params tp = do
    fundHoldingDatumType <- genValidFundHoldingDatumType tp
    let fund_DatumType_ = fund_DatumType_MockData tp
        protocolDatumType_ = protocol_DatumType_MockData tp
        date = tpCollectCommissionsDate tp
        deadline = FundT.fdDeadline fund_DatumType_
        share = ProtocolT.pdShare_InBPx1e2_Delegators protocolDatumType_
        availableWithoutTaken =
            FundHelpers.getCommissionsAvailable
                deadline
                fundHoldingDatumType
                share
                0
                date
    if availableWithoutTaken > 0
    then do
        taken <- QC.chooseInteger (0, availableWithoutTaken - 1)
        let available = availableWithoutTaken - taken
        withdrawAmount <- QC.chooseInteger (1, available)
        return $ Collect_Delegators_Params (fundHoldingDatumType { FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators = taken }) withdrawAmount
    else
        return $ Collect_Delegators_Params (fundHoldingDatumType { FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators = 0 }) 0

genInvalid_Collect_Delegators_ParamsZeroOrLess :: TestParams -> QC.Gen Collect_Delegators_Params
genInvalid_Collect_Delegators_ParamsZeroOrLess tp = do
        collect_Delegators_Params <- genValid_Collect_Delegators_Params tp
        withdrawAmount <- QC.arbitrary :: QC.Gen IntegerMinusZero
        return $ collect_Delegators_Params {getCMPWithdrawAmount = let (IntegerMinusZero x) = withdrawAmount in x}

genInvalid_Collect_Delegators_ParamsMoreThanAvailables :: TestParams -> QC.Gen Collect_Delegators_Params
genInvalid_Collect_Delegators_ParamsMoreThanAvailables tp = do
    fundHoldingDatumType <- genValidFundHoldingDatumType tp
    let fund_DatumType_ = fund_DatumType_MockData tp
        protocolDatumType_ = protocol_DatumType_MockData tp
        date = tpCollectCommissionsDate tp
        deadline = FundT.fdDeadline fund_DatumType_
        share = ProtocolT.pdShare_InBPx1e2_Delegators protocolDatumType_
        availableWithoutTaken =
            FundHelpers.getCommissionsAvailable
                deadline
                fundHoldingDatumType
                share
                0
                date
    taken <- QC.chooseInteger (0, availableWithoutTaken - 1)
    let available = availableWithoutTaken - taken
    withdrawAmount <- QC.chooseInteger (available+1 , available+100)
    return $ Collect_Delegators_Params (fundHoldingDatumType { FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators = taken }) withdrawAmount

--------------------------------------------------------------------------------

data ReIndexParams
    = ReIndexParams
          { riFund_UTXO         :: LedgerApiV2.TxOut
          , riFundHolding_UTXOs :: [LedgerApiV2.TxOut]
          , riTotal_Deposits_IU :: Integer
          , riInvestUnit        :: T.InvestUnit
          , riTokensToAdd       :: T.InvestUnit
          , riTokensToRemove    :: T.InvestUnit
          , riTokensPrices      :: T.InvestUnit
          }
    deriving (Show)

--------------------------------------------------------------------------------



genReIndexParams :: Bool -> TestParams -> Bool -> QC.Gen ReIndexParams
genReIndexParams swTrace tp validAmountRespectDivisibility = do
    ------------------
    !_ <- debugTraceIfM swTrace "------------------------------------------------------------"
    !_ <- debugTraceIfM swTrace "------------------------------------------------------------"
    !_ <- debugTraceIfM swTrace "------------------------------------------------------------"
    ------------------
    let maxQtyFundHolding = 5::Integer
    let maxQtyInvestUnitTokens = 16::Integer
    let maxQtyTokensToAddOrRemove = 2::Integer
    ------------------
    num_FundHolding_UTxOs <- QC.choose (1, maxQtyFundHolding)
    !_ <- debugTraceIfM swTrace ("Number of FundHolding UTxOs: " ++ show num_FundHolding_UTxOs)
    ------------------
    let input_Fund_UTxO = fund_UTxO_MockData_Parametrizable2 tp num_FundHolding_UTxOs
        input_Fund_Datum = FundT.getFund_DatumType_From_UTxO input_Fund_UTxO
        base_FundHolding_UTxO = fundHolding_UTxO_With_NoDeposits_MockData tp
        base_FundHolding_Datum = FundHoldingT.getFundHolding_DatumType_From_UTxO base_FundHolding_UTxO
    ------------------
    input_InvestUnit <- genInvestUnit maxQtyInvestUnitTokens
    !_ <- debugTraceIfM swTrace ("-- \nInvestUnit: " ++ show input_InvestUnit)
    input_FundHolding_UTxOs <- mapM (\ix -> genValidFundHolding_UTxO_Parametrizable tp input_Fund_Datum base_FundHolding_Datum input_InvestUnit ix (tpDepositDate tp) True) [0 .. num_FundHolding_UTxOs - 1]
    let input_FundHolding_Datums = FundHoldingT.getFundHolding_DatumType_From_UTxO <$> input_FundHolding_UTxOs
    ------------------
    let totalFTMinted = sum $ map FundHoldingT.hdSubTotal_FT_Minted input_FundHolding_Datums
    !_ <- debugTraceIfM swTrace ("-- \nTotal FT Minted: " ++ show totalFTMinted)
    ------------------
    tokenPrices <- genTokenPrices (T.iuValues input_InvestUnit)
    !_ <- debugTraceIfM swTrace ("-- \nToken Prices: " ++ show tokenPrices)
    ------------------
    if validAmountRespectDivisibility
    then genReIndexParamsWithDivisibility swTrace maxQtyTokensToAddOrRemove input_Fund_UTxO input_FundHolding_UTxOs totalFTMinted input_InvestUnit tokenPrices
    else genReIndexParamsWithoutDivisibility swTrace maxQtyTokensToAddOrRemove input_Fund_UTxO input_FundHolding_UTxOs totalFTMinted input_InvestUnit tokenPrices

--------------------------------------------------------------------------------

genReIndexParamsWithDivisibility :: Bool -> Integer -> LedgerApiV2.TxOut -> [LedgerApiV2.TxOut] -> Integer -> T.InvestUnit -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> QC.Gen ReIndexParams
genReIndexParamsWithDivisibility swTrace maxQtyTokensToAddOrRemove input_Fund_UTxO input_FundHolding_UTxOs totalFTMinted input_InvestUnit@(T.InvestUnit tokens) tokenPrices = do
    ------------------
    let k = totalFTMinted `div` gcd 100 totalFTMinted
    !_ <- debugTraceIfM swTrace ("-- \nK value: " ++ show k)
    ------------------
    tokensToRemove <- genTokensToRemoveWithDivisibility swTrace maxQtyTokensToAddOrRemove tokens totalFTMinted k
    !_ <- debugTraceIfM swTrace ("--  \nTokens to Remove: " ++ show tokensToRemove)
    ------------------
    cvrt <- calculateCVT swTrace tokensToRemove tokenPrices totalFTMinted
    !_ <- debugTraceIfM swTrace ("--  \nCVRT: " ++ show cvrt)
    ------------------
    (tokensToAdd, updatedTokenPrices) <- genTokensToAddWithDivisibility swTrace maxQtyTokensToAddOrRemove tokens tokenPrices cvrt k totalFTMinted
    !_ <- debugTraceIfM swTrace ("--  \nTokens to Add: " ++ show tokensToAdd)
    !_ <- debugTraceIfM swTrace ("--  \nUpdated Token Prices: " ++ show updatedTokenPrices)
    ------------------
    cvat <- calculateCVT swTrace tokensToAdd updatedTokenPrices totalFTMinted
    !_ <- debugTraceIfM swTrace ("--  \nCVAT: " ++ show cvat)
    ------------------
    maybeAdaToken <- genADAToAdd swTrace cvrt cvat totalFTMinted k
    !_ <- debugTraceIfM swTrace ("-- \nADA Token to Add: " ++ show maybeAdaToken)
    ------------------
    let tokenToAddWithADA = maybe tokensToAdd (:tokensToAdd) maybeAdaToken
    ------------------
    let isCorrect_ReIdx_Amount_Valid_Divisibility =
            all (\(_, _, !am) -> am > 0 && (am * totalFTMinted) `remainder` 100 == 0) tokenToAddWithADA
            && all (\(_, _, !am) -> am > 0 && (am * totalFTMinted) `remainder` 100 == 0) tokensToRemove

    let formatTokenInfo (cs, tn, am) =
            "Token: " ++ show cs ++ " " ++ show tn ++
            "\n  Amount: " ++ show am ++
            "\n  Amount * totalFTMinted: " ++ show (am * totalFTMinted) ++
            "\n  Remainder: " ++ show ((am * totalFTMinted) `remainder` 100) ++
            "\n  Is Valid: " ++ show (am > 0 && (am * totalFTMinted) `remainder` 100 == 0)
    ------------------
    let invalidTokensToAdd = filter (\(_, _, am) -> not (am > 0 && (am * totalFTMinted) `remainder` 100 == 0)) tokenToAddWithADA
    let invalidTokensToRemove = filter (\(_, _, am) -> not (am > 0 && (am * totalFTMinted) `remainder` 100 == 0)) tokensToRemove
    ------------------
    let debugMessage =
            "-- \nInvalid_Divisibility:\n" ++
            "totalFTMinted: " ++ show totalFTMinted ++ "\n" ++
            "Invalid Tokens To Add:\n" ++ concatMap (\t -> formatTokenInfo t ++ "\n") invalidTokensToAdd ++
            "Invalid Tokens To Remove:\n" ++ concatMap (\t -> formatTokenInfo t ++ "\n") invalidTokensToRemove
    ------------------
    !_ <- debugTraceIfM (swTrace && isCorrect_ReIdx_Amount_Valid_Divisibility) ("-- \nValid_Divisibility: " ++ show isCorrect_ReIdx_Amount_Valid_Divisibility)
    !_ <- debugTraceIfM (swTrace && not isCorrect_ReIdx_Amount_Valid_Divisibility) debugMessage
    ------------------
    return $ ReIndexParams
        input_Fund_UTxO
        input_FundHolding_UTxOs
        totalFTMinted
        input_InvestUnit
        (T.InvestUnit tokenToAddWithADA )
        (T.InvestUnit tokensToRemove)
        (T.InvestUnit $ updatedTokenPrices ++ [(LedgerValue.adaSymbol, LedgerValue.adaToken, 1)])

--------------------------------------------------------------------------------

genTokensToRemoveWithDivisibility :: Bool -> Integer ->  [T.InvestUnitToken] -> Integer -> Integer ->QC.Gen [T.InvestUnitToken]
genTokensToRemoveWithDivisibility swTrace maxQtyTokensToAddOrRemove tokens totalFTMinted k = do
    ------------------
    -- Choose the number of tokens to remove (at least one)
    numToRemove <- QC.choose (1, min (fromIntegral maxQtyTokensToAddOrRemove) (length tokens))
    !_ <- debugTraceIfM swTrace ("-- \nNumber of tokens to remove: " ++ show numToRemove)
    ------------------
    -- Select tokens to remove based on numToRemove
    tokensToRemove <- do
        sublist <- QC.sublistOf tokens
        if null sublist || length sublist < numToRemove
            then do
                rest <- QC.shuffle tokens
                return $ take numToRemove rest
            else return $ take numToRemove sublist
    !_ <- debugTraceIfM swTrace ("Selected tokens to remove: " ++ show tokensToRemove)
    ------------------
    ControlMonad.forM tokensToRemove $ \(cs, tn, maxAmount) -> do
        let trq_iu_min = 1 * ((100 * k) `div` totalFTMinted)
        -- Choose a Target Removal Quantity per IU (TRQ_IU)
        trq_iu <- QC.choose (trq_iu_min, maxAmount)
        !_ <- debugTraceIfM swTrace ("TRQ_IU for token " ++ show (cs, tn) ++ ": " ++ show trq_iu)
        -- Calculate Estimated Scaling Factor (ESF)
        let esf = (trq_iu * totalFTMinted) `div` 100
        !_ <- debugTraceIfM swTrace ("ESF for token " ++ show (cs, tn) ++ ": " ++ show esf)
        -- Calculate Scaling Factor Index (SFI)
        let sfi = esf `div` k
        !_ <- debugTraceIfM swTrace ("SFI for token " ++ show (cs, tn) ++ ": " ++ show sfi)
        -- Calculate Scaling Factor (SF)
        let sf = sfi * k
        !_ <- debugTraceIfM swTrace ("SF for token " ++ show (cs, tn) ++ ": " ++ show sf)
        -- Calculate Removal Quantity per IU (RQ_IU)
        let rq_iu = (sf * 100) `div` totalFTMinted
        !_ <- debugTraceIfM swTrace ("RQ_IU for token " ++ show (cs, tn) ++ ": " ++ show rq_iu)
        return (cs, tn, rq_iu)

--------------------------------------------------------------------------------

genTokensToAddWithDivisibility ::  Bool -> Integer -> [T.InvestUnitToken] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> Integer -> Integer -> Integer -> QC.Gen ([T.InvestUnitToken],[(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)])
genTokensToAddWithDivisibility swTrace maxQtyTokensToAddOrRemove originalTokens tokenPrices cvrt k totalFTMinted = do
    ------------------
    numToAdd <- QC.choose (1::Int, fromIntegral maxQtyTokensToAddOrRemove)
    !_ <- debugTraceIfM swTrace ("-- \nNumber of tokens to add: " ++ show numToAdd)
    ------------------
    ------------------
    firstNewToken <- (,,) <$> (QC.arbitrary :: QC.Gen LedgerApiV2.CurrencySymbol ) <*> (QC.arbitrary  :: QC.Gen LedgerApiV2.TokenName) <*> pure (0 :: Integer)
    !_ <- debugTraceIfM swTrace ("Generated new token: " ++ show firstNewToken)
    ------------------
    -- Then, generate the rest of the tokens (if any)
    restTokens <- if numToAdd > 1
        then ControlMonad.replicateM (numToAdd - 1) $ do
            useExisting <- QC.arbitrary
            if useExisting && not (null originalTokens)
                then do
                    (cs, tn, _) <- QC.elements originalTokens
                    !_ <- debugTraceIfM swTrace ("Selected existing token: " ++ show (cs, tn, 0::Integer))
                    return (cs, tn, 0)
                else do
                    additionalNewToken <- (,,) <$> QC.arbitrary <*> QC.arbitrary <*> pure 0
                    !_ <- debugTraceIfM swTrace ("Generated additional new token: " ++ show additionalNewToken)
                    return additionalNewToken
        else return []
    ------------------
    let tokensToAdd = firstNewToken : restTokens
    ------------------
    -- -- Ensure at least one token is added
    -- tokensToAdd <-
    --     if null tokensToAdd'
    --         then do
    --             amount <- QC.choose (1, maxAmount) -- Assign an amount for the new token
    --             newToken <- (,,) <$> QC.arbitrary <*> QC.arbitrary <*> pure amount
    --             !_ <- debugTraceIfM swTrace ("Generated new token (as list was empty): " ++ show newToken)
    --             return [newToken]
    --         else return (reverse tokensToAdd')
    ------------------
    let originalPriceMap = [((cs, tn), p) | (cs, tn, p) <- tokenPrices]
    tokenPricesToAdd <- ControlMonad.mapM (\(cs, tn, _) -> do
            let existingPrice = lookup (cs, tn) originalPriceMap
            case existingPrice of
                Just price -> do
                    !_ <- debugTraceIfM swTrace ("Using existing price for token " ++ show (cs, tn) ++ ": " ++ show price)
                    return (cs, tn, price)
                Nothing -> do
                    newPrice <- QC.choose (1, 1000)
                    !_ <- debugTraceIfM swTrace ("Assigned new price for token " ++ show (cs, tn) ++ ": " ++ show newPrice)
                    return (cs, tn, newPrice)
        ) tokensToAdd
    ------------------
    -- SFI_common = floor(CVRT / (K * (Price_C + Price_D)))
    ------------------
    let totalPrice = sum $ map (\(_, _, p) -> p) tokenPricesToAdd
    !_ <- debugTraceIfM swTrace ("Total price: " ++ show totalPrice)
    let sfiCommon =  cvrt `div` ( k * totalPrice)
    !_ <- debugTraceIfM swTrace ("SFI Common: " ++ show sfiCommon)
    ------------------
    -- Scaling Factor for Token i (SF_i) = SFI_i * K
    -- Add Quantity per IU for Token i (AQ_IU_i) = (SF_i * 100) / TIU
    -- Add Quantity Total for Token i (AQ_T_i) = (AQ_IU_i * TIU) / 100
    -- Aggregate Value of Added Token i (AV_i) = RQ_T_i * Price_i
    -- Simplified Aggregate Value of Added Token i (AV_i) = SF_i * Price_i
    ------------------
    let tokensToAddWithAddQtyPerIU = map (\(cs, tn, _)  ->
                let
                    -- Calculate Scaling Factor (SF)
                    asf = sfiCommon * k
                    -- Calculate Add Quantity per IU (AQ_IU)
                    aq_iu = (asf * 100) `div` totalFTMinted
                    -- !_ <- debugTraceIfM swTrace ("Token " ++ show (cs, tn) ++ " - ASF: " ++ show asf ++ ", AQ_IU: " ++ show aq_iu) $
                in (cs, tn, aq_iu)
            ) tokensToAdd
    ------------------
    let updatedTokenPrices = foldr (\(cs, tn, price) acc ->
            if any (\(cs', tn', _) -> cs == cs' && tn == tn') tokenPrices
                then acc  -- Skip if the price is already present
                else (cs, tn, price) : acc
            ) tokenPrices tokenPricesToAdd
    ------------------
    return (tokensToAddWithAddQtyPerIU, updatedTokenPrices)

--------------------------------------------------------------------------------

genADAToAdd :: Bool -> Integer -> Integer -> Integer -> Integer -> QC.Gen (Maybe (LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer))
genADAToAdd swTrace cvrt cvat totalFTMinted k = do
    ------------------
    -- Scaling Factor Index for Token ADA (SFI_ADA) = Balance / K = (CVRT - CVAT) / K
    -- Scaling Factor for Token ADA (SF_ADA) = SFI_ADA * K
    -- Add Quantity per IU for Token ADA (AQ_IU_ADA) = (SF_ADA * 100) / TIU
    -- Add Quantity Total for Token ADA (AQ_T_ADA) = (AQ_IU_ADA * TIU) / 100
    -- Aggregate Value of Added Token ADA (AV_ADA) = AQ_T_ADA * Price_ADA = AQ_T_ADA * 1 = AQ_T_ADA
    ------------------
    let balance = cvrt - cvat
    !_ <- debugTraceIfM swTrace ("-- \nBalance: " ++ show balance)
    ------------------
    if balance <= 0
    then
        return Nothing
    else do
        -- Calculate Scaling Factor Index (SFI)
        -- SFI_ADA = Balance / K
        let sfi_ADA = balance `div` k
        !_ <- debugTraceIfM swTrace ("SFI_ADA: " ++ show sfi_ADA)
        -- Calculate Scaling Factor (SF)
        -- SF_ADA = 11 * K = 9,504
        let sf_ADA = sfi_ADA * k
        !_ <- debugTraceIfM swTrace ("SF_ADA: " ++ show sf_ADA)
        -- Calculate Add Quantity per IU (AQ_IU)
        -- AQ_IU_ADA = (SF_C * 100) / TIU
        let aq_iu_ADA = (sf_ADA * 100) `div` totalFTMinted
        !_ <- debugTraceIfM swTrace ("AQ_IU_ADA: " ++ show aq_iu_ADA)
        ------------------
        -- AV_ADA = SF_ADA  = ((AQ_IU_i * TIU) / 100) * 1
        let adaValue = aq_iu_ADA * totalFTMinted `div` 100
        let balance_final = cvrt - (cvat+adaValue)
        !_ <- debugTraceIfM swTrace ("Final Balance: " ++ show balance_final)
        ------------------
        let adaToken = (LedgerValue.adaSymbol, LedgerValue.adaToken, aq_iu_ADA)
        ------------------
        return $ Just adaToken

--------------------------------------------------------------------------------

calculateCVT :: Bool -> [T.InvestUnitToken] -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> Integer -> QC.Gen Integer
calculateCVT _swTrace tokens prices totalFTMinted = do

    let priceMap = [((cs, tn), p) | (cs, tn, p) <- prices]
        cvt = sum [((amt * totalFTMinted) `div` 100) * DataMaybe.fromMaybe 0 (lookup (cs, tn) priceMap) | (cs, tn, amt) <- tokens]
    -- !_ <- debugTraceIfM swTrace ("CVT: " ++ show cvt)
    return cvt
    -- !_ <- debugTraceIfM swTrace ("CVT calculation - Tokens: " ++ show tokens ++ ", Prices: " ++ show prices ++ ", Total FT Minted: " ++ show totalFTMinted ++ ", CVT: " ++ show cvt) cvt

--------------------------------------------------------------------------------

genReIndexParamsWithoutDivisibility :: Bool -> Integer -> LedgerApiV2.TxOut -> [LedgerApiV2.TxOut] -> Integer -> T.InvestUnit -> [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> QC.Gen ReIndexParams
genReIndexParamsWithoutDivisibility swTrace maxQtyTokensToAddOrRemove input_Fund_UTxO input_FundHolding_UTxOs totalFTMinted input_InvestUnit@(T.InvestUnit tokens) tokenPrices = do
    tokensToRemove <- genTokensToRemoveWithoutDivisibility maxQtyTokensToAddOrRemove tokens 
    valueToRemove <- calculateCVT swTrace tokensToRemove tokenPrices totalFTMinted
    tokensToAdd <- genTokensToAddWithoutDivisibility maxQtyTokensToAddOrRemove tokens valueToRemove
    updatedTokenPrices <- updateTokenPrices tokenPrices tokensToAdd
    return $ ReIndexParams
        input_Fund_UTxO
        input_FundHolding_UTxOs
        totalFTMinted
        input_InvestUnit
        (T.InvestUnit tokensToAdd)
        (T.InvestUnit tokensToRemove)
        (T.InvestUnit updatedTokenPrices)

genTokensToRemoveWithoutDivisibility :: Integer -> [T.InvestUnitToken] -> QC.Gen [T.InvestUnitToken]
genTokensToRemoveWithoutDivisibility maxQtyTokensToAddOrRemove tokens = do
    numToRemove <- QC.choose (1, P.min (length tokens - 1) (fromIntegral maxQtyTokensToAddOrRemove))
    tokensToRemove <- QC.sublistOf tokens >>= \sublist -> return (take numToRemove sublist)
    ControlMonad.forM tokensToRemove $ \(cs, tn, maxAmount) -> do
        amount <- QC.choose (1, maxAmount)
        return (cs, tn, amount)

genTokensToAddWithoutDivisibility :: Integer ->  [T.InvestUnitToken] -> Integer -> QC.Gen [T.InvestUnitToken]
genTokensToAddWithoutDivisibility maxQtyTokensToAddOrRemove originalTokens valueToRemove = do
    numToAdd <- QC.choose (1, P.min (length originalTokens + 2) (fromIntegral maxQtyTokensToAddOrRemove))  -- Allow for new tokens
    tokensToAdd <- ControlMonad.replicateM numToAdd $ do
        useExisting <- QC.arbitrary
        if useExisting && not (null originalTokens)
        then QC.elements originalTokens
        else (,,) <$> QC.arbitrary <*> QC.arbitrary <*> pure 0

    -- Distribute valueToRemove among the new tokens
    amounts <- distributeValue valueToRemove (length tokensToAdd)
    return $ zipWith (\(cs, tn, _) amount -> (cs, tn, amount)) tokensToAdd amounts


distributeValue :: Integer -> Int -> QC.Gen [Integer]
distributeValue total n = do
    rawValues <- ControlMonad.replicateM (n-1) $ QC.choose (1, 100)
    let sumRaw = sum rawValues
        scaledValues = map (\x -> x * total `div` sumRaw) rawValues
        remainingValue = total - sum scaledValues
    return $ scaledValues ++ [remainingValue]

genTokenPrices :: [T.InvestUnitToken] -> QC.Gen [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
genTokenPrices = mapM (\(cs, tn, _) -> do
        price <- QC.choose (1, 1000)
        return (cs, tn, price)
    )

updateTokenPrices :: [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)] -> [T.InvestUnitToken] -> QC.Gen [(LedgerApiV2.CurrencySymbol, LedgerApiV2.TokenName, Integer)]
updateTokenPrices existingPrices newTokens = do
    let existingPriceMap = DataList.nubBy (\(cs1, tn1, _) (cs2, tn2, _) -> cs1 == cs2 && tn1 == tn2) existingPrices
        priceMap = [((cs, tn), p) | (cs, tn, p) <- existingPriceMap]
    newPrices <- ControlMonad.forM newTokens $ \(cs, tn, _) -> do
        case lookup (cs, tn) priceMap of
            Just price -> return (cs, tn, price)
            Nothing -> do
                price <- QC.choose (1, 1000)
                return (cs, tn, price)
    return $ DataList.nubBy (\(cs1, tn1, _) (cs2, tn2, _) -> cs1 == cs2 && tn1 == tn2) (existingPrices ++ newPrices)

--------------------------------------------------------------------------------

