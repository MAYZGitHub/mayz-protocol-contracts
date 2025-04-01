
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

--------------------------------------------------------------------------------2
{- HLINT ignore "Use camelCase"          -}
{- HLINT ignore "Reduce duplication"          -}
--------------------------------------------------------------------------------2

module Helpers.Helpers where

--------------------------------------------------------------------------------2
-- External Imports
--------------------------------------------------------------------------------2

import qualified Ledger.Value           as LedgerValue
import qualified Plutonomy
import qualified Plutus.V2.Ledger.Api   as LedgerApiV2
import           PlutusTx
import qualified PlutusTx.Code          as PlutusTxCode
import           PlutusTx.Prelude       hiding (unless)

--------------------------------------------------------------------------------2
-- Import Internos
--------------------------------------------------------------------------------2

import qualified Generic.DeployHelpers  as DeployHelpers
import qualified Generic.OnChainHelpers as OnChainHelpers
import qualified Protocol.Fund.Helpers  as FundHelpers
import qualified Protocol.Types         as T

--------------------------------------------------------------------------------2

calculateDepositCommissionsUsingMonthsWrapper :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->  (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonthsWrapper commission_PerYear_InBPx1e3 deadline date deposit  = do
    let
        !commission_PerYear_InBPx1e3' =  PlutusTx.unsafeFromBuiltinData commission_PerYear_InBPx1e3
        !deadline' =  PlutusTx.unsafeFromBuiltinData deadline
        !date' =  PlutusTx.unsafeFromBuiltinData date
        !deposit' =  PlutusTx.unsafeFromBuiltinData deposit
        !res = FundHelpers.calculateDepositCommissionsUsingMonths commission_PerYear_InBPx1e3' deadline' date' deposit'
    res

calculateWithdrawCommissionsUsingMonthsWrapper :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->  (Integer, Integer, Integer)
calculateWithdrawCommissionsUsingMonthsWrapper commission_PerYear_InBPx1e3 deadline date withdraw  = do
    let
        !commission_PerYear_InBPx1e3' =  PlutusTx.unsafeFromBuiltinData commission_PerYear_InBPx1e3
        !deadline' =  PlutusTx.unsafeFromBuiltinData deadline
        !date' =  PlutusTx.unsafeFromBuiltinData date
        !withdraw' =  PlutusTx.unsafeFromBuiltinData withdraw
        !investUnit_Granularity = 1
        !res = FundHelpers.calculateWithdrawCommissionsUsingMonths commission_PerYear_InBPx1e3' deadline' date' withdraw' investUnit_Granularity
    res

calculateDepositCommissionsUsingMonthsBuiltinDataCodeOptimized :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> PlutusTxCode.CompiledCode (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonthsBuiltinDataCodeOptimized commission_PerYear_InBPx1e3 deadline date deposit =
    Plutonomy.optimizeUPLC $ calculateDepositCommissionsUsingMonthsBuiltinDataCode commission_PerYear_InBPx1e3 deadline date deposit


calculateDepositCommissionsUsingMonthsBuiltinDataCode ::Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> PlutusTxCode.CompiledCode (Integer, Integer, Integer)
calculateDepositCommissionsUsingMonthsBuiltinDataCode commission_PerYear_InBPx1e3 deadline date deposit =
    $$(PlutusTx.compile [|| calculateDepositCommissionsUsingMonthsWrapper ||])
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData commission_PerYear_InBPx1e3)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime deadline)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime date)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData deposit)

calculateWithdrawCommissionsUsingMonthsBuiltinDataCode ::Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> PlutusTxCode.CompiledCode (Integer, Integer, Integer)
calculateWithdrawCommissionsUsingMonthsBuiltinDataCode commission_PerYear_InBPx1e3 deadline date withdraw =
    $$(PlutusTx.compile [|| calculateWithdrawCommissionsUsingMonthsWrapper ||])
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData commission_PerYear_InBPx1e3)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime deadline)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData $ LedgerApiV2.getPOSIXTime date)
            `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData withdraw)

--------------------------------------------------------------------------------

powRationalWrapper :: BuiltinData -> BuiltinData ->  BuiltinData -> Rational
powRationalWrapper !num !dem !pot  = do
    let
        !num' =  PlutusTx.unsafeFromBuiltinData num
        !dem' =  PlutusTx.unsafeFromBuiltinData dem
        !pot' =  PlutusTx.unsafeFromBuiltinData pot
        !res = OnChainHelpers.powRational num' dem' pot'
    res

powRationalWrapperBuiltinDataCodeOptimized :: Integer -> Integer -> Integer -> PlutusTxCode.CompiledCode Rational
powRationalWrapperBuiltinDataCodeOptimized num dem pot =
    Plutonomy.optimizeUPLC $ powRationalWrapperBuiltinDataCode num dem pot

powRationalWrapperBuiltinDataCode :: Integer -> Integer -> Integer -> PlutusTxCode.CompiledCode Rational
powRationalWrapperBuiltinDataCode num dem pot =
    Plutonomy.optimizeUPLC $    $$(PlutusTx.compile [|| powRationalWrapper ||])
        `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData num)
        `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData dem)
        `PlutusTx.applyCode` PlutusTx.liftCode (DeployHelpers.intToBuiltinData pot)


--------------------------------------------------------------------------------

-- ESTO ES LO QUE QUIERO OPTIMIZAR
testDepositWrapper :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData ->  Bool
testDepositWrapper commission_PerYear_InBPx1e3 deadline date deposit  fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit  = do
    let
        !commission_PerYear_InBPx1e3' =  PlutusTx.unsafeFromBuiltinData commission_PerYear_InBPx1e3
        !deadline' =  PlutusTx.unsafeFromBuiltinData deadline
        !date' =  PlutusTx.unsafeFromBuiltinData date
        !deposit' =  PlutusTx.unsafeFromBuiltinData deposit
        !fundFT_AC' =  PlutusTx.unsafeFromBuiltinData fundFT_AC
        !valueOf_FundHoldingDatum_In' =  PlutusTx.unsafeFromBuiltinData valueOf_FundHoldingDatum_In
        !valueOf_FundHoldingDatum_Out' =  PlutusTx.unsafeFromBuiltinData valueOf_FundHoldingDatum_Out
        !investUnit' =  PlutusTx.unsafeFromBuiltinData investUnit
        !res = testDeposit commission_PerYear_InBPx1e3' deadline' date' deposit' fundFT_AC' valueOf_FundHoldingDatum_In' valueOf_FundHoldingDatum_Out' investUnit'
    res

testDepositBuiltinDataCodeOptimized :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> LedgerValue.AssetClass -> LedgerValue.Value -> LedgerValue.Value ->  T.InvestUnit -> PlutusTxCode.CompiledCode Bool
testDepositBuiltinDataCodeOptimized commission_PerYear_InBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit =
    Plutonomy.optimizeUPLC $ testDepositBuiltinDataCode commission_PerYear_InBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit

testDepositBuiltinDataCode :: Integer -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer -> LedgerValue.AssetClass -> LedgerValue.Value -> LedgerValue.Value ->  T.InvestUnit -> PlutusTxCode.CompiledCode Bool
testDepositBuiltinDataCode commission_PerYear_InBPx1e3 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit =
           $$(PlutusTx.compile [|| testDepositWrapper ||])
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData commission_PerYear_InBPx1e3)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData deadline)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData date)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData deposit)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData fundFT_AC)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData valueOf_FundHoldingDatum_In)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData valueOf_FundHoldingDatum_Out)
                    `PlutusTx.applyCode` PlutusTx.liftCode (LedgerApiV2.toBuiltinData investUnit)

------------------

testDeposit :: [Integer] -> LedgerApiV2.POSIXTime -> LedgerApiV2.POSIXTime -> Integer ->  LedgerValue.AssetClass -> LedgerValue.Value -> LedgerValue.Value -> T.InvestUnit ->   Bool
testDeposit commissions_Table_Numerator_1e6 deadline date deposit fundFT_AC valueOf_FundHoldingDatum_In valueOf_FundHoldingDatum_Out investUnit =
    traceIfFalse "wrong output values" (isCorrect_Output_FundHolding_Value valueFor_FundHoldingDatum_Control_With_Tokens_And_FT valueOf_FundHoldingDatum_Out)
    where
    ------------------
        !(_, commissionsFT, _) = FundHelpers.calculateDepositCommissionsUsingMonths commissions_Table_Numerator_1e6 deadline date deposit
    ------------------
        !valueOf_TokensForDeposit_Plus_FundHolding_Value = FundHelpers.createValue_WithTokensFrom_InvestUnit_Plus_FundHolding_Value valueOf_FundHoldingDatum_In (T.iuValues investUnit) deposit True
    ------------------
        !valueFor_FT_Commissions = LedgerValue.assetClassValue fundFT_AC commissionsFT
    ------------------
        !valueFor_FundHoldingDatum_Control_With_Tokens_And_FT = valueOf_TokensForDeposit_Plus_FundHolding_Value <> valueFor_FT_Commissions -- valueOf_FundHoldingDatum_In -- <> valueOf_TokensForDeposit <> valueFor_FT_Commissions
    ------------------
        isCorrect_Output_FundHolding_Value :: LedgerValue.Value -> LedgerValue.Value -> Bool
        isCorrect_Output_FundHolding_Value valueFor_FundHoldingDatum_Control valueOf_FundHoldingDatum_Out' =
            valueOf_FundHoldingDatum_Out' `OnChainHelpers.isEqValue` valueFor_FundHoldingDatum_Control

------------------
