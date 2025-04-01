{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------3
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------3

module Protocol.Fund.Helpers where

--------------------------------------------------------------------------------2
-- Import Externos
--------------------------------------------------------------------------------2

import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import PlutusTx.Prelude hiding (unless)
import qualified PlutusTx.Ratio as TxRatio

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Generic.Types as T
import qualified Ledger.Value as LedgerValue
import qualified PlutusTx.AssocMap as TxAssocMap
import qualified Protocol.Fund.Holding.Types as FundHoldingT
import qualified Protocol.Fund.Types as T
import qualified Protocol.Types as T

--------------------------------------------------------------------------------2
-- Modulo
--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_NormalChanges #-}
mkUpdated_Fund_Datum_With_NormalChanges :: T.FundDatumType -> [T.WalletPaymentPKH] -> LedgerApiV2.CurrencySymbol -> Integer -> T.FundDatumType
mkUpdated_Fund_Datum_With_NormalChanges !fundDatum_In !admins !tokenAdminPolicy_CS !maxDepositAndWithdraw =
    fundDatum_In {T.fdAdmins = admins, T.fdTokenAdminPolicy_CS = tokenAdminPolicy_CS, T.fdMaxDepositAndWithdraw = maxDepositAndWithdraw}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_MinADAChanged #-}
mkUpdated_Fund_Datum_With_MinADAChanged :: T.FundDatumType -> Integer -> T.FundDatumType
mkUpdated_Fund_Datum_With_MinADAChanged !fundDatum_In !newMinADA = fundDatum_In {T.fdMinADA = newMinADA}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_ClosedAt #-}
mkUpdated_Fund_Datum_With_ClosedAt :: T.FundDatumType -> LedgerApiV2.POSIXTime -> T.FundDatumType
mkUpdated_Fund_Datum_With_ClosedAt !fundDatum_In !closedAt = fundDatum_In {T.fdClosedAt = Just closedAt}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_HoldingAdded #-}
mkUpdated_Fund_Datum_With_HoldingAdded :: T.FundDatumType -> T.FundDatumType
mkUpdated_Fund_Datum_With_HoldingAdded !fundDatum_In =
    fundDatum_In {T.fdHoldingsCount = T.fdHoldingsCount fundDatum_In + 1, T.fdHoldingsIndex = T.fdHoldingsIndex fundDatum_In + 1}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_Fund_Datum_With_HoldingDeleted #-}
mkUpdated_Fund_Datum_With_HoldingDeleted :: T.FundDatumType -> T.FundDatumType
mkUpdated_Fund_Datum_With_HoldingDeleted !fundDatum_In =
    fundDatum_In {T.fdHoldingsCount = T.fdHoldingsCount fundDatum_In - 1}

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Deposit #-}
mkUpdated_FundHolding_Datum_With_Deposit :: FundHoldingT.FundHoldingDatumType -> Integer -> Integer -> Integer -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Deposit !fundHoldingDatum_In !mintingFT !_deliverFT !commissionsPayed !commissions_FT_Release_PerMonth_1e6 =
    fundHoldingDatum_In
        { FundHoldingT.hdSubTotal_FT_Minted_Accumulated = FundHoldingT.hdSubTotal_FT_Minted_Accumulated fundHoldingDatum_In + mintingFT
        , FundHoldingT.hdSubTotal_FT_Minted = FundHoldingT.hdSubTotal_FT_Minted fundHoldingDatum_In + mintingFT
        , FundHoldingT.hdSubTotal_FT_Commissions = FundHoldingT.hdSubTotal_FT_Commissions fundHoldingDatum_In + commissionsPayed
        , FundHoldingT.hdSubTotal_FT_Commissions_Total = FundHoldingT.hdSubTotal_FT_Commissions_Total fundHoldingDatum_In + commissionsPayed
        , FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 = FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 fundHoldingDatum_In + commissions_FT_Release_PerMonth_1e6
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Withdraw #-}
mkUpdated_FundHolding_Datum_With_Withdraw :: FundHoldingT.FundHoldingDatumType -> Integer -> Integer -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Withdraw !fundHoldingDatum_In !withdraw !commissionsForUserFTToGetBack !commissions_FT_Release_PerMonth_1e6 =
    fundHoldingDatum_In
        { FundHoldingT.hdSubTotal_FT_Minted = FundHoldingT.hdSubTotal_FT_Minted fundHoldingDatum_In - withdraw - commissionsForUserFTToGetBack
        , FundHoldingT.hdSubTotal_FT_Commissions = FundHoldingT.hdSubTotal_FT_Commissions fundHoldingDatum_In - commissionsForUserFTToGetBack
        , FundHoldingT.hdSubTotal_FT_Commissions_Total = FundHoldingT.hdSubTotal_FT_Commissions_Total fundHoldingDatum_In - commissionsForUserFTToGetBack
        , FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 = FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 fundHoldingDatum_In - commissions_FT_Release_PerMonth_1e6
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission #-}
mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission :: FundHoldingT.FundHoldingDatumType -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Collect_Protocol_Commission !fundHoldingDatum_In !withdraw =
    fundHoldingDatum_In
        { FundHoldingT.hdSubTotal_FT_Commissions = FundHoldingT.hdSubTotal_FT_Commissions fundHoldingDatum_In - withdraw
        , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol = FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol fundHoldingDatum_In + withdraw
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission #-}
mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission :: FundHoldingT.FundHoldingDatumType -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Collect_Managers_Commission !fundHoldingDatum_In !withdraw =
    fundHoldingDatum_In
        { FundHoldingT.hdSubTotal_FT_Commissions = FundHoldingT.hdSubTotal_FT_Commissions fundHoldingDatum_In - withdraw
        , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers = FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers fundHoldingDatum_In + withdraw
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission #-}
mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission :: FundHoldingT.FundHoldingDatumType -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_Collect_Delegators_Commission !fundHoldingDatum_In !withdraw =
    fundHoldingDatum_In
        { FundHoldingT.hdSubTotal_FT_Commissions = FundHoldingT.hdSubTotal_FT_Commissions fundHoldingDatum_In - withdraw
        , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators = FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators fundHoldingDatum_In + withdraw
        }

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_FundHolding_Datum_With_MinADAChanged #-}
mkUpdated_FundHolding_Datum_With_MinADAChanged :: FundHoldingT.FundHoldingDatumType -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_With_MinADAChanged !fundHoldingDatum_In !newMinADA = fundHoldingDatum_In {FundHoldingT.hdMinADA = newMinADA}

--------------------------------------------------------------------------------

{-# INLINEABLE mkUpdated_FundHolding_Datum_AfterBalancing #-}
mkUpdated_FundHolding_Datum_AfterBalancing :: FundHoldingT.FundHoldingDatumType -> Integer ->  Integer ->  Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> FundHoldingT.FundHoldingDatumType
mkUpdated_FundHolding_Datum_AfterBalancing !fundHoldingDatum_In !newMinted !newMintedAcum !newCommissions !newCommissionsTotal !newCommissions_FT_Release_PerMonth_1e6 !newCollectProtocol !newCollectManagers !newCollectDelegators =
    fundHoldingDatum_In
        { FundHoldingT.hdSubTotal_FT_Commissions = newCommissions
        , FundHoldingT.hdSubTotal_FT_Commissions_Total = newCommissionsTotal
        , FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 = newCommissions_FT_Release_PerMonth_1e6
        , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Protocol = newCollectProtocol
        , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Managers = newCollectManagers
        , FundHoldingT.hdSubTotal_FT_Commissions_Collected_Delegators = newCollectDelegators
        , FundHoldingT.hdSubTotal_FT_Minted = newMinted
        , FundHoldingT.hdSubTotal_FT_Minted_Accumulated = newMintedAcum
        }

--------------------------------------------------------------------------------2

{-# INLINEABLE getRemainingMonths #-}
getRemainingMonths :: LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer
getRemainingMonths deadline date =
    let
        !msPerMonth = 2_592_000_000 :: Integer -- 1000 * 60 * 60 * 24 * 30
        !msRemaining = LedgerApiV2.getPOSIXTime $ deadline - date
        !monthsRemaining = msRemaining `divide` msPerMonth
    in
        if monthsRemaining < 0 then 0 else monthsRemaining

--------------------------------------------------------------------------------2

{-# INLINEABLE calculateDepositCommissionsUsingMonths #-}
calculateDepositCommissionsUsingMonths :: [Integer] -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonths commissions_Table_Numerator_1e6 deadline date deposit
    | deposit < 0 = traceError "Deposit cannot be negative"
    | otherwise = (userFT, commissionsFT, commissions_FT_Release_PerMonth_1e6)
    where
        ------------------
        -- las comisiones son por en realidad por año y en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
        -- la formula de comisiones es = (1 - comisiones por periodo en porcentaje del 0 al 1) ^ (periodos restantes + 1)
        -- el periodo aqui lo voy a calcular en meses
        -- eso significa que al valor de commission_PerYear_InBPx1e3 tengo que dividirlo por
        -- 10e2 para pasarlo a bp
        -- 100 para pasarlo a porcentaje normal del 1 al 100
        -- 100 para pasarlo a porcentaje del 0 al 1
        -- 12 para pasarlo a meses
        -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120_000_000
        -- commissionesPerMonthPct0to1 = TxRatio.unsafeRatio commission_PerYear_InBPx1e3 den
        -- pero en lugar de calcular el rational y leugo volver a separarlo en num y den para llamar a la potencia
        -- voy a calcular primero el 1 - commissionesPerMonthPct0to1 de esta forma:
        -- commissionesToUse = TxRatio.unsafeRatio (den - commission_PerYear_InBPx1e3) den
        -- pero este Rational tampoco hace falta crearlo, puedo usar directamente esos num y den para llamar a la potencia
        -- commissionsAcumulated = OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den (daysRemaining + 1)
        ------------------
        !monthsRemaining = getRemainingMonths deadline date
        ------------------
        -- !den = 120_000_000
        -- !commissionsAcumulated = OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den (monthsRemaining + 1)
        -- !userFT = TxRatio.truncate (commissionsAcumulated * TxRatio.fromInteger deposit)
        ------------------
        -- Estoy usando una tabla de comisiones pre calculada
        -- la tabla tiene en cada posicion el valor de las comisiones acumuladas para ese cantidad de mes restantes
        -- ademas esta guardada como enteros x 1e6, por lo que para obtener el valor real hay que dividir por 1e6
        -- aprovechando que es entero, primero calculo la multiplicacion por deposit, que es entero, y luego lo divido por 1e6
        ------------------
        !commissionsAcumulated_Numerator_1e6 = commissions_Table_Numerator_1e6 !! (monthsRemaining + 1)
        ------------------
        {--
        The calculation is indeed rounding down (truncating) the result.
        This approach favors the protocol over the users.
        Users will always receive either the exact calculated amount of tokens or slightly less, never more.
        Any fractional parts are effectively kept by the protocol.
        Over many transactions, this could accumulate to a non-trivial amount in favor of the protocol.
        --}
        ------------------
        !userFT = TxRatio.truncate (TxRatio.unsafeRatio (commissionsAcumulated_Numerator_1e6 * deposit) 1_000_000)
        ------------------
        !commissionsFT = deposit - userFT
        ------------------
        -- !commissions_FT_Release_PerMonth
        --     | monthsRemaining == 0 = TxRatio.fromInteger 0
        --     | otherwise = TxRatio.unsafeRatio commissionsFT monthsRemaining
        -- !commissions_FT_Release_PerMonth_1e6 = OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissions_FT_Release_PerMonth
        ------------------
        !commissions_FT_Release_PerMonth_1e6 = calculateRelease monthsRemaining commissionsFT

--------------------------------------------------------------------------------2

{-# INLINEABLE calculateRelease #-}
calculateRelease :: Integer -> Integer -> Integer
calculateRelease monthsRemaining commissionsFT = commissions_FT_Release_PerMonth_1e6
    where
        -- !monthsRemaining = getRemainingMonths deadline date
        ------------------
        !commissions_FT_Release_PerMonth
            | monthsRemaining == 0 = TxRatio.fromInteger 0
            | otherwise = TxRatio.unsafeRatio commissionsFT monthsRemaining
        !commissions_FT_Release_PerMonth_1e6 = OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissions_FT_Release_PerMonth

--------------------------------------------------------------------------------2

-- NOTE: este metdoo ya no se usa onchain, se dividio en dos y se usan el de bajo, pero si se usa en testings
{-# INLINEABLE calculateWithdrawCommissionsUsingMonths #-}
calculateWithdrawCommissionsUsingMonths :: [Integer] -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> Integer -> (Integer, Integer, Integer)
calculateWithdrawCommissionsUsingMonths commissions_Table_Numerator_1e6 deadline date withdraw investUnit_Granularity
    | withdraw < 0 = traceError "Withdraw cannot be negative"
    | otherwise = (commissionsForUserFTToGetBack, withdrawPlusCommissionsGetBack, commissions_FT_Release_PerMonth_1e6)
    where
        -- las comisiones son por en realidad por año y en basic points BP multiplicados por 1e3 o lo que es igual 10e2 = 1_000
        -- la formula de comisiones es = (1 - comisiones por periodo en porcentaje del 0 al 1) ^ (periodos restantes + 1)
        -- el periodo aqui lo voy a calcular en meses
        -- eso significa que al valor de commission_PerYear_InBPx1e3 tengo que dividirlo por
        -- 10e2 para pasarlo a bp
        -- 100 para pasarlo a porcentaje normal del 1 al 100
        -- 100 para pasarlo a porcentaje del 0 al 1
        -- 12 para pasarlo a meses
        -- defino den = 1e3 * 100 * 100 * 12 = 1000 * 100 * 100 * 12 = 120_000_000
        -- commissionesPerMonthPct0to1 = TxRatio.unsafeRatio commission_PerYear_InBPx1e3 den
        -- pero en lugar de calcular el rational y leugo volver a separarlo en num y den para llamar a la potencia
        -- voy a calcular primero el 1 - commissionesPerMonthPct0to1 de esta forma:
        -- commissionesToUse = TxRatio.unsafeRatio (den - commission_PerYear_InBPx1e3) den
        -- pero este Rational tampoco hace falta crearlo, puedo usar directamente esos num y den para llamar a la potencia
        -- commissionsAcumulated = OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den (daysRemaining + 1)
        ------------------
        !monthsRemaining = getRemainingMonths deadline date
        ------------------
        -- !den = 120_000_000
        -- !commissionsAcumulatedNotIncludingThisPeriod = OnChainHelpers.powRational (den - commission_PerYear_InBPx1e3) den monthsRemaining
        -- !userFT'forCalculationsOfCommissionsToGetBack = TxRatio.truncate (commissionsAcumulatedNotIncludingThisPeriod * TxRatio.fromInteger withdraw)
        ------------------
        -- Estoy usando una tabla de comisiones pre calculada
        -- la tabla tiene en cada posicion el valor de las comisiones acumuladas para ese cantidad de mes restantes
        -- ademas esta guardada como enteros x 1e6, por lo que para obtener el valor real hay que dividir por 1e6
        -- aprovechando que es entero, primero calculo la multiplicacion por withdraw, que es entero, y luego lo divido por 1e6
        ------------------
        !commissionsAcumulatedNotIncludingThisPeriod_Numerator_1e6 = commissions_Table_Numerator_1e6 !! monthsRemaining
        !userFT'forCalculationsOfCommissionsToGetBack = TxRatio.truncate (TxRatio.unsafeRatio (commissionsAcumulatedNotIncludingThisPeriod_Numerator_1e6 * withdraw) 1_000_000)
        ------------------
        -- Ajustar para que sea múltiplo de la granularidad
        !userFT'forCalculationsOfCommissionsToGetBackAdjusted = (userFT'forCalculationsOfCommissionsToGetBack `divide` investUnit_Granularity) * investUnit_Granularity
        -- Recalcular commissionsForUserFTToGetBack basado en el valor ajustado
        !commissionsForUserFTToGetBack = withdraw - userFT'forCalculationsOfCommissionsToGetBackAdjusted
        !withdrawPlusCommissionsGetBack = withdraw + commissionsForUserFTToGetBack
        -- !commissions_FT_Release_PerMonth
        --     | monthsRemaining == 0 = TxRatio.fromInteger 0
        --     | otherwise = TxRatio.unsafeRatio commissionsForUserFTToGetBack monthsRemaining
        -- !commissions_FT_Release_PerMonth_1e6 = OnChainHelpers.setAndLoosePrecision1e6GetOnlyNumerator commissions_FT_Release_PerMonth
        ------------------
        !commissions_FT_Release_PerMonth_1e6 = calculateRelease monthsRemaining commissionsForUserFTToGetBack

--------------------------------------------------------------------------------2

{-# INLINEABLE calculateWithdrawCommissionsAvailable #-}
calculateWithdrawCommissionsAvailable :: [Integer] -> Integer -> Integer -> Integer -> Integer
calculateWithdrawCommissionsAvailable commissions_Table_Numerator_1e6 monthsRemaining withdraw investUnit_Granularity
    | withdraw < 0 = traceError "Withdraw cannot be negative"
    | otherwise = commissionsForUserFTToGetBack
    where
        -- !monthsRemaining = getRemainingMonths deadline date
        ------------------
        !commissionsAcumulatedNotIncludingThisPeriod_Numerator_1e6 = commissions_Table_Numerator_1e6 !! monthsRemaining
        !userFT'forCalculationsOfCommissionsToGetBack = TxRatio.truncate (TxRatio.unsafeRatio (commissionsAcumulatedNotIncludingThisPeriod_Numerator_1e6 * withdraw) 1_000_000)
        ------------------
        -- Ajustar para que sea múltiplo de la granularidad
        !userFT'forCalculationsOfCommissionsToGetBackAdjusted = (userFT'forCalculationsOfCommissionsToGetBack `divide` investUnit_Granularity) * investUnit_Granularity
        -- Recalcular commissionsForUserFTToGetBack basado en el valor ajustado
        !commissionsForUserFTToGetBack = withdraw - userFT'forCalculationsOfCommissionsToGetBackAdjusted

--------------------------------------------------------------------------------2

{-# INLINEABLE getCommissionsAvailable #-}
getCommissionsAvailable :: LedgerApiV2.POSIXTime -> FundHoldingT.FundHoldingDatumType -> Integer -> Integer -> LedgerApiV2.POSIXTime -> Integer
getCommissionsAvailable deadline fundHoldingDatum_In shareBPx1e2 taken date =
    let
        !monthsRemainingRational = getRemainingMonths deadline date
        !totalCommisions = FundHoldingT.hdSubTotal_FT_Commissions_Total fundHoldingDatum_In
        !release_PerMonth = TxRatio.unsafeRatio (FundHoldingT.hdSubTotal_FT_Commissions_Release_PerMonth_1e6 fundHoldingDatum_In) 1_000_000
        !commisionsReady = TxRatio.fromInteger totalCommisions - (TxRatio.fromInteger monthsRemainingRational * release_PerMonth)
        -- shareBPx1e2 = shareBP * 100
        -- 1BP to decimal 1/10_000
        -- 1BPx1e2 to decimal 1/(10_000 * 100) = 1/1_000_000
        !shareDecimal = TxRatio.unsafeRatio shareBPx1e2 1_000_000
        !commisionsReady_share = commisionsReady * shareDecimal
    in
        TxRatio.truncate commisionsReady_share - taken

--------------------------------------------------------------------------------2

{-
Function: isCorrectAmount

Purpose:
Validates deposit or withdrawal amounts based on predefined rules and invest unit granularity.

Parameters:
- amount: The deposit or withdrawal amount to validate.

Checks:
1. Amount is positive.
2. Amount doesn't exceed maximum allowed for deposits/withdrawals.
3. Amount is divisible by the smallest unit of granularity in the invest unit.

Example:
For an invest unit with tokens [455, 1000] (representing 4.55 and 10.00):
- Valid amount: 200
    Because:
    (200 * 455) /100 = 910 (whole number)
    (200 * 1000) /100 = 2000 (whole number)
- Invalid amount: 157
    Because:
    (157 * 455) /100 = 714.35 (fractional, not allowed)
    (157 * 1000) /100 = 1570 (whole number, but the other token fails)

The function getDecimalsInInvestUnit would return 100 for this invest unit,
because the smallest fraction is 0.01 (represented as 1 in the 455).
isCorrectAmount then checks if the amount is divisible by 100, ensuring
it can be evenly distributed across all tokens without creating fractions.

In this case:
200 is valid because 200 % 100 == 0
157 is invalid because 157 % 100 != 0
-}
------------------
{-# INLINEABLE isCorrectAmount #-}
isCorrectAmount :: Integer -> Integer -> Integer -> Bool
isCorrectAmount !amount !max' !investUnit_Granularity' =
    amount
        > 0
        && (amount <= max')
        && amount
        `remainder` investUnit_Granularity'
        == 0

------------------
{-# INLINEABLE isCorrectCommissionsAmount #-}
isCorrectCommissionsAmount :: Integer -> Integer -> Integer -> Bool
isCorrectCommissionsAmount !amount !max' !investUnit_Granularity' =
    amount
        >= 0
        && (amount <= max')
        && amount
        `remainder` investUnit_Granularity'
        == 0

------------------
{-
Function: getDecimalsInInvestUnit

Purpose:
Determines the smallest fraction (highest precision) used in the invest unit.

Parameters:
- tokens: List of InvestUnitTokens to analyze.

Process:
1. Iterates through all tokens in the invest unit.
2. For each token, checks if its amount (which is multiplied by 100) is divisible by 100, 10, or neither.
3. Returns the smallest divisor found:
- 100 if any token has a fraction (two decimal places)
- 10 if the smallest fraction is 0.1 (one decimal place)
- 1 if all tokens are whole numbers (no decimals)

Importance:
This function is crucial for ensuring that deposit/withdrawal amounts are compatible
with the invest unit's highest precision, which in turn guarantees exact divisions
in token amount calculations.

Examples:

1. Two decimal places:
Invest unit tokens: [455, 1000, 10000]
- 455 represents 4.55 (two decimal places)
- 1000 represents 10.00 (whole number)
- 10000 represents 100.00 (whole number)
Result: Returns 100
Explanation: The smallest fraction is 0.01 (in 4.55), so amounts must be divisible by 100.

2. One decimal place:
Invest unit tokens: [150, 1000, 2050]
- 150 represents 1.5 (one decimal place)
- 1000 represents 10.0 (whole number)
- 2050 represents 20.5 (one decimal place)
Result: Returns 10
Explanation: The smallest fraction is 0.1, so amounts must be divisible by 10.

3. No decimal places:
Invest unit tokens: [100, 1000, 5000]
- 100 represents 1 (whole number)
- 1000 represents 10 (whole number)
- 5000 represents 50 (whole number)
Result: Returns 1
Explanation: All numbers are whole, so amounts only need to be divisible by 1.

4. Mixed precision:
Invest unit tokens: [455, 150, 1000]
- 455 represents 4.55 (two decimal places)
- 150 represents 1.5 (one decimal place)
- 1000 represents 10 (whole number)
Result: Returns 100
Explanation: The highest precision is two decimal places (0.01 in 4.55),
                so amounts must be divisible by 100 to work with all tokens.

Usage in isCorrectAmount:
If getDecimalsInInvestUnit returns 100, then isCorrectAmount will check if
the deposit/withdrawal amount is divisible by 100. This ensures that the amount
can be evenly distributed across all tokens, including those with the highest precision.

For example, with the result 100:
- 200 is a valid amount (200 % 100 == 0)
- 1100 is a valid amount (1100 % 100 == 0)
- 155 is not a valid amount (155 % 100 != 0)
-}
------------------
{-# INLINEABLE getDecimalsInInvestUnit #-}
getDecimalsInInvestUnit :: [T.InvestUnitToken] -> Integer
getDecimalsInInvestUnit !tokens = go tokens 1
    where
        ------------------
        go [] !acc = acc
        go ((_, _, !amount) : xs) acc =
            let
                !accNew = max acc (dividedBy amount)
            in
                go xs accNew
        ------------------
        dividedBy !amount
            | amount `remainder` 100 == 0 = 1
            | amount `remainder` 10 == 0 = 10
            | otherwise = 100

------------------
{-
Overall Function: createValue_WithTokensFrom_InvestUnit_Plus_FundHolding_Value

Purpose:
Calculates the new value for a FundHolding after a deposit or withdrawal operation,
considering the current value and the invest unit definition.

Key Points:
1. Invest Unit amounts are stored multiplied by 100 to handle two decimal places.
Example: 4.55 tokens are stored as 455 in the invest unit.
2. Processes each currency symbol (CS) and its associated tokens.
3. Respects the granularity defined in the invest unit.
4. Uses a round-up approach for robustness, despite guaranteed exactness due to granularity checks.

Process:
1. Converts input Value to a list of currency symbols and token maps.
2. Updates or adds tokens based on the invest unit and deposit/withdrawal amount.
3. Ensures accurate token amount calculations.
4. Returns a new Value with updated token amounts.

Example:
For an invest unit with token A = 455 (4.55) and a deposit of 1000:
New amount of token A = (455 * 1000) / 100 = 4550
-}
{-# INLINEABLE createValue_WithTokensFrom_InvestUnit_Plus_FundHolding_Value #-}
createValue_WithTokensFrom_InvestUnit_Plus_FundHolding_Value :: LedgerValue.Value -> [T.InvestUnitToken] -> Integer -> Bool -> LedgerValue.Value
createValue_WithTokensFrom_InvestUnit_Plus_FundHolding_Value (LedgerValue.Value !mp) investUnitTokens !amount !swRoundUp =
    LedgerValue.Value mapCSResult
    where
        !listMapCS = TxAssocMap.toList mp
        !listTokens = investUnitTokens
        !mapCSResult = TxAssocMap.fromList (updateListMapCS listTokens listMapCS)
        ------------------
        updateListMapCS [] !restListMapCS = restListMapCS
        updateListMapCS ((!cs, !tn, !amt) : restTokens) restListMapCS =
            let
                !(tokensFromSameCS, restTokensWithoutCS) = getOthersTokensFromSameCSAndDeleteFromList cs restTokens [] []
                !(mapFromSameCS, restMapWithoutCS) = getMapFromSameCSAndDeleteFromList cs restListMapCS []
                !mapFromSameCSWithTokensAdded = addTokensInMap cs mapFromSameCS ((tn, amt) : tokensFromSameCS)
                !resultMap = mapFromSameCSWithTokensAdded : updateListMapCS restTokensWithoutCS restMapWithoutCS
            in
                resultMap
        ------------------
        getOthersTokensFromSameCSAndDeleteFromList _ [] !accListFromSame !accListOthers = (accListFromSame, accListOthers)
        getOthersTokensFromSameCSAndDeleteFromList !cs ((!cs', !tn', !amt') : restTokens) accListFromSame accListOthers
            | cs == cs' = getOthersTokensFromSameCSAndDeleteFromList cs restTokens ((tn', amt') : accListFromSame) accListOthers
            | otherwise = getOthersTokensFromSameCSAndDeleteFromList cs restTokens accListFromSame ((cs', tn', amt') : accListOthers)
        ------------------
        getMapFromSameCSAndDeleteFromList _ [] !accListMapOthers = (Nothing, accListMapOthers)
        getMapFromSameCSAndDeleteFromList !cs ((!cs', !mapTN) : restMap) accListOthers
            | cs == cs' = (Just mapTN, restMap ++ accListOthers)
            | otherwise = getMapFromSameCSAndDeleteFromList cs restMap ((cs', mapTN) : accListOthers)
        ------------------
        addTokensInMap !cs Nothing !tokens = addTokensInMap' cs TxAssocMap.empty tokens
        addTokensInMap !cs (Just !mapTN) !tokens = addTokensInMap' cs mapTN tokens
        ------------------
        {-
            Function: addTokensInMap'

            Purpose:
            Adds or updates token amounts in a map based on invest unit definition and deposit/withdrawal amount.

            Parameters:
            - cs: Currency Symbol
            - mapTN: Existing map of TokenName to amounts
            - listTokensToAddInMap: List of tokens from the invest unit to process

            Process:
            1. Iterates through each token in the invest unit.
            2. Calculates new amount: (invest_unit_amount * deposit_amount) / 100
            Note: invest_unit_amount is already multiplied by 100, so this division brings it back to the correct scale.
            3. Updates the map with new amounts, adding new entries or updating existing ones.

            Note on Calculation:
            Despite granularity check ensuring exact divisions, we still use multiply_By_Scaled_1e2_And_RoundUp
            with round-up behavior as a safety measure

            Example:
            For an invest unit token of 455 (4.55) and a deposit of 1000:
            New amount = (455 * 1000) / 100 = 4550
            This would typically be exact, but the function would round up if there were any remainder.
            -}
        addTokensInMap' :: LedgerApiV2.CurrencySymbol -> LedgerApiV2.Map LedgerApiV2.TokenName Integer -> [(LedgerApiV2.TokenName, Integer)] -> (LedgerApiV2.CurrencySymbol, LedgerApiV2.Map LedgerApiV2.TokenName Integer)
        addTokensInMap' !cs !mapTN [] = (cs, mapTN)
        addTokensInMap' !cs !mapTN !listTokensToAddInMap =
            ( cs
            , foldl
                ( \acc (!tn', !amt') ->
                    let
                        !roundedAmt =
                            -- NOTE: en depositos esta bien que redondee para arriba, en retiros redondea para abajo
                            if swRoundUp
                                then OnChainHelpers.multiply_By_Scaled_1e2_And_RoundUp amount amt'
                                else OnChainHelpers.multiply_By_Scaled_1e2_And_RoundDown amount amt'
                    in
                        mapElement acc tn' roundedAmt
                )
                mapTN
                listTokensToAddInMap
            )
        mapElement !acc !tn !amt =
            case TxAssocMap.lookup tn acc of
                Nothing -> TxAssocMap.insert tn amt acc
                Just !amt' -> TxAssocMap.insert tn (amt + amt') acc
