# MAYZ Protocol Smart Contracts - Technical Specification

## Table of Contents
- [MAYZ Protocol Smart Contracts - Technical Specification](#mayz-protocol-smart-contracts---technical-specification)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Validators](#validators)
  - [Minting Policies](#minting-policies)
  - [Interactions Between Contracts](#interactions-between-contracts)
  - [Version Management and Dependencies](#version-management-and-dependencies)
  - [Security Considerations](#security-considerations)
  - [Common Types and Helper Structures](#common-types-and-helper-structures)
    - [Basic Types](#basic-types)
    - [Core Asset Identifiers](#core-asset-identifiers)
    - [MinMaxDef Type](#minmaxdef-type)
    - [Investment Unit Type](#investment-unit-type)
    - [Oracle Data Types](#oracle-data-types)
    - [FundCategory Type](#fundcategory-type)
  - [Contract Components](#contract-components)
    - [Protocol Contract](#protocol-contract)
      - [Protocol Policy Parameters](#protocol-policy-parameters)
      - [Protocol Validator Parameters](#protocol-validator-parameters)
      - [Protocol Datum](#protocol-datum)
      - [Protocol Policy Redeemer](#protocol-policy-redeemer)
        - [MintID](#mintid)
      - [Protocol Validator Redeemers](#protocol-validator-redeemers)
        - [DatumUpdate](#datumupdate)
        - [UpdateMinADA](#updateminada)
        - [Emergency](#emergency)
    - [Fund Contract](#fund-contract)
      - [Fund Policy Parameters](#fund-policy-parameters)
      - [Fund Validator Parameters](#fund-validator-parameters)
      - [Fund Datum](#fund-datum)
      - [Fund Policy Redeemers](#fund-policy-redeemers)
        - [MintID](#mintid-1)
        - [BurnID](#burnid)
        - [MintFT](#mintft)
        - [BurnFT](#burnft)
      - [Fund Validator Redeemers](#fund-validator-redeemers)
        - [DatumUpdate](#datumupdate-1)
        - [UpdateMinADA](#updateminada-1)
        - [FundHoldingAdd](#fundholdingadd)
        - [FundHoldingDelete](#fundholdingdelete)
        - [Emergency](#emergency-1)
        - [Finish](#finish)
        - [Delete](#delete)
    - [InvestUnit Contract](#investunit-contract)
      - [InvestUnit Validator Parameters](#investunit-validator-parameters)
      - [InvestUnit Datum](#investunit-datum)
      - [InvestUnit Validator Redeemers](#investunit-validator-redeemers)
        - [UpdateMinADA](#updateminada-2)
        - [ReIndexing](#reindexing)
        - [Emergency](#emergency-2)
        - [Delete](#delete-1)
      - [Validation Functions](#validation-functions)
    - [FundHolding Contract](#fundholding-contract)
      - [Fund Holding Policy Parameters](#fund-holding-policy-parameters)
      - [Fund Holding Validator Parameters](#fund-holding-validator-parameters)
      - [Fund Holding Datum](#fund-holding-datum)
      - [Fund Holding Policy Redeemers](#fund-holding-policy-redeemers)
        - [MintID](#mintid-2)
        - [BurnID](#burnid-1)
      - [Fund Holding Validator Redeemers](#fund-holding-validator-redeemers)
        - [UpdateMinADA](#updateminada-3)
        - [Deposit](#deposit)
        - [Withdraw](#withdraw)
        - [Collect\_Protocol\_Commission](#collect_protocol_commission)
        - [Collect\_Managers\_Commission](#collect_managers_commission)
        - [Collect\_Delegators\_Commission](#collect_delegators_commission)
        - [ReIndexing](#reindexing-1)
        - [BalanceAssets](#balanceassets)
        - [Delete](#delete-2)
        - [Emergency](#emergency-3)
    - [SwapOffer Contract](#swapoffer-contract)
      - [SwapOffer Policy Parameters](#swapoffer-policy-parameters)
      - [SwapOffer Validator Parameters](#swapoffer-validator-parameters)
      - [SwapOffer Datum](#swapoffer-datum)
      - [SwapOffer Policy Redeemers](#swapoffer-policy-redeemers)
        - [MintID](#mintid-3)
        - [BurnID](#burnid-2)
      - [SwapOffer Validator Redeemers](#swapoffer-validator-redeemers)
        - [UpdateStatus](#updatestatus)
        - [UpdateAskedCommissionRate](#updateaskedcommissionrate)
        - [UpdateSellRestrictions](#updatesellrestrictions)
        - [UpdateMinADA](#updateminada-4)
        - [Deposit](#deposit-1)
        - [Withdraw](#withdraw-1)
        - [SwapFTxADA](#swapftxada)
        - [SwapADAxFT](#swapadaxft)
        - [Delete](#delete-3)
        - [Emergency](#emergency-4)
      - [Validation Functions](#validation-functions-1)
    - [Delegation Contract](#delegation-contract)
      - [Delegation Policy Parameters](#delegation-policy-parameters)
      - [Delegation Validator Parameters](#delegation-validator-parameters)
      - [Delegation Datum Fields](#delegation-datum-fields)
      - [Delegation Policy Redeemers](#delegation-policy-redeemers)
        - [MintID](#mintid-4)
        - [BurnID](#burnid-3)
      - [Delegation Validator Redeemers](#delegation-validator-redeemers)
        - [UpdateMinADA](#updateminada-5)
        - [Deposit](#deposit-2)
        - [Withdraw](#withdraw-2)
        - [Delete](#delete-4)
    - [Script Contract](#script-contract)
      - [Script Policy Parameters](#script-policy-parameters)
      - [Script Validator Parameters](#script-validator-parameters)
      - [Script Datum](#script-datum)
      - [Script Policy Redeemers](#script-policy-redeemers)
        - [MintID](#mintid-5)
        - [BurnID](#burnid-4)
      - [Script Validator Redeemer](#script-validator-redeemer)
        - [Delete](#delete-5)
  - [Main Validation Patterns by Script](#main-validation-patterns-by-script)
    - [Protocol Script](#protocol-script)
    - [Fund Script](#fund-script)
    - [FundHolding Script](#fundholding-script)
    - [InvestUnit Script](#investunit-script)
    - [SwapOffer Script](#swapoffer-script)
    - [Common Validation Patterns](#common-validation-patterns)
  - [Transaction Specifications](#transaction-specifications)
    - [Core Protocol Management](#core-protocol-management)
      - [Protocol Management](#protocol-management)
        - [Protocol\_Create\_Tx](#protocol_create_tx)
        - [Protocol\_DatumUpdate\_Tx](#protocol_datumupdate_tx)
    - [Fund Lifecycle](#fund-lifecycle)
      - [Fund Management](#fund-management)
        - [Fund\_Create\_Tx](#fund_create_tx)
        - [Fund\_DatumUpdate\_Tx](#fund_datumupdate_tx)
        - [Fund\_Finish\_Tx](#fund_finish_tx)
        - [Fund\_Delete\_Tx](#fund_delete_tx)
      - [Reindexing Operations](#reindexing-operations)
        - [Fund\_ReIndexing\_Tx](#fund_reindexing_tx)
      - [Fund Holdings Management](#fund-holdings-management)
        - [FundHolding\_Create\_Tx](#fundholding_create_tx)
        - [FundHolding\_Delete\_Tx](#fundholding_delete_tx)
    - [Investment Operations](#investment-operations)
      - [Deposits \& Withdrawals](#deposits--withdrawals)
        - [Fund\_Deposit\_Tx](#fund_deposit_tx)
        - [Fund\_Withdraw\_Tx](#fund_withdraw_tx)
      - [Commission Collection](#commission-collection)
        - [FundHolding\_Collect\_Protocol\_Commission\_Tx](#fundholding_collect_protocol_commission_tx)
        - [FundHolding\_Collect\_Managers\_Commission\_Tx](#fundholding_collect_managers_commission_tx)
        - [FundHolding\_Collect\_Delegators\_Commission\_Tx](#fundholding_collect_delegators_commission_tx)
    - [Trading Operations](#trading-operations)
      - [Swap Offer Management](#swap-offer-management)
        - [SwapOffer\_Create\_Tx](#swapoffer_create_tx)
        - [SwapOffer\_UpdateStatus\_Tx](#swapoffer_updatestatus_tx)
        - [SwapOffer\_UpdateAskedCommissionRate\_Tx](#swapoffer_updateaskedcommissionrate_tx)
        - [SwapOffer\_UpdateSellRestrictions\_Tx](#swapoffer_updatesellrestrictions_tx)
        - [SwapOffer\_Delete\_Tx](#swapoffer_delete_tx)
      - [Swap Executions](#swap-executions)
        - [SwapOffer\_Deposit\_Tx](#swapoffer_deposit_tx)
        - [SwapOffer\_Withdraw\_Tx](#swapoffer_withdraw_tx)
        - [SwapOffer\_SwapFTxADA\_Tx](#swapoffer_swapftxada_tx)
        - [SwapOffer\_SwapADAxFT\_Tx](#swapoffer_swapadaxft_tx)
    - [System Maintenance](#system-maintenance)
      - [Balance Operations](#balance-operations)
        - [FundHolding\_BalanceAssets\_Tx](#fundholding_balanceassets_tx)
      - [MinADA Updates](#minada-updates)
        - [Protocol\_UpdateMinADA\_Tx](#protocol_updateminada_tx)
        - [Fund\_UpdateMinADA\_Tx](#fund_updateminada_tx)
        - [FundHolding\_UpdateMinADA\_Tx](#fundholding_updateminada_tx)
        - [InvestUnit\_UpdateMinADA\_Tx](#investunit_updateminada_tx)
        - [SwapOffer\_UpdateMinADA\_Tx](#swapoffer_updateminada_tx)
      - [Script Management](#script-management)
        - [Scritp\_Add\_Tx](#scritp_add_tx)
        - [Scritp\_Remove\_Tx](#scritp_remove_tx)
    - [Emergency \& Recovery Operations](#emergency--recovery-operations)
        - [Protocol\_Emergency\_Tx](#protocol_emergency_tx)
        - [Fund\_Emergency\_Tx](#fund_emergency_tx)
        - [FundHolding\_Emergency\_Tx](#fundholding_emergency_tx)
        - [SwapOffer\_Emergency\_Tx](#swapoffer_emergency_tx)
        - [InvestUnit\_Emergency\_Tx](#investunit_emergency_tx)

## Introduction

This document provides a comprehensive overview of the smart contracts that power the MAYZ Protocol. Each contract plays a crucial role in the ecosystem, enabling the creation and management of decentralized investment funds, facilitating token swaps, and ensuring the overall security and efficiency of the protocol.

For more detailed information on each contract's implementation, please refer to the source code and accompanying comments in the `src/Protocol/` directory.

## Validators

Our protocol utilizes advanced smart contracts for secure and efficient operations:
- **Protocol Contract**: Manages global protocol parameters and upgrades.
- **Fund Contract**: Manages the creation, operation, and liquidation of funds.
- **Fund Holding Contract**: Handles individual fund holdings and optimizes concurrency.
- **Invest Unit Contract**: Manages representation and valuation of fund compositions.
- **SwapOffer Contract**: Facilitates ADA and Fund Token exchanges.
- **Delegation Contract**: Manages MAYZ token delegations to funds.
- **Script Contract**: Oversees deployed scripts on-chain.

## Minting Policies

The MAYZ Protocol employs several minting policies to manage various tokens within the ecosystem:
- **Protocol ID Policy**: Mints the unique Protocol ID token
- **Fund ID Policy**: Mints Fund ID and Invest Unit ID tokens for each fund
- **Fund Holding ID Policy**: Mints Fund Holding ID tokens for each fund holding
- **Fund Token (FT) Policy**: Manages the minting and burning of fund-specific tokens
- **Swap Offer ID Policy**: Mints Swap Offer ID tokens
- **Delegation ID Policy**: Mints Delegation ID tokens
- **Script ID Policy**: Mints Script ID tokens for deployed scripts

Each policy ensures the uniqueness and integrity of its respective tokens within the protocol.

## Interactions Between Contracts
The MAYZ Protocol's smart contracts interact in complex ways to provide a seamless and secure user experience:
- The Protocol Contract sets global parameters used by all other contracts.
- Fund Contracts create and manage funds, interacting with Fund Holding and Invest Unit contracts.
- Fund Holding Contracts manage individual holdings, coordinating with the Fund Contract for deposits, withdrawals, and re-indexing.
- The Invest Unit Contract works closely with Fund and Fund Holding contracts during re-indexing operations.
- The Swap Offer contract interacts with Fund contracts to facilitate token exchanges.
- The Delegation Contract interacts with Fund contracts for commission distributions.
- The Script Contract manages the lifecycle of all deployed scripts, including the other contracts.

These interactions are carefully designed to maintain the integrity and security of the entire protocol.

## Version Management and Dependencies 
The MAYZ Protocol implements a hierarchical version management system that reflects the dependencies between contracts. Changes in core contracts trigger cascading updates in dependent contracts to maintain compatibility and consistency:

- Protocol Contract changes affect all contracts
- Fund Block changes affect Fund, Fund Holding, and Invest Unit contracts, plus dependent Swap/Delegation contracts
- Swap and Delegation contracts can be updated independently

Each contract follows semantic versioning principles while respecting these dependencies. For in-depth details about our versioning strategy, dependency management, and update procedures, see our [Dependencies and Versioning](./VERSIONING.md) documentation.

## Security Considerations
The MAYZ Protocol implements several security measures across its smart contracts:
- **Access Control**: Admin functions are protected by signature checks or admin tokens.
- **Oracle Integration**: Price data for token swaps and re-indexing is provided by a secure, multi-source oracle system.
- **Commission Calculations**: High-precision arithmetic is used to prevent rounding errors in commission calculations.
- **Re-indexing Safeguards**: The re-indexing process includes checks to ensure the total fund value remains constant.
- **Multi-UTXO Design**: Fund holdings are spread across multiple UTXOs to enhance concurrency and resist certain types of attacks.
- **Emergency Mechanisms**: The Protocol Contract includes provisions for handling emergency situations.

Regular security audits and open-source development practices contribute to the ongoing security efforts of the protocol.

## Common Types and Helper Structures

### Basic Types

```haskell
-- Core type aliases
type CS = LedgerApiV2.CurrencySymbol    -- Policy ID
type TN = LedgerApiV2.TokenName         -- Token Name
type WalletPaymentPKH = LedgerApiV2.PubKeyHash  -- Payment Public Key Hash


-- Status constants

swapOffer_Status_Open :: Integer 
swapOffer_Status_Open = 1

swapOffer_Status_Closed :: Integer
swapOffer_Status_Closed = 2

swapOffer_AllowSell :: Integer
swapOffer_AllowSell = 1

swapOffer_NotAllowSell :: Integer
swapOffer_NotAllowSell = 0

-- System constants

validTxTimeRange :: LedgerApiV2.POSIXTime
validTxTimeRange = 900_000 -- 15 * 60 * 1000 = 15 minutos

```

### Core Asset Identifiers

```haskell
-- Protocol Token Identifiers
protocolID_TN :: TN 
protocolID_TN = "ProtocolID"    -- Unique NFT for Protocol Datum UTXO

protocolTokenEmergencyAdmin_TN :: T.TN
protocolTokenEmergencyAdmin_TN = "EmergencyAdmin"

protocolTokenAdmin_TN :: TN
protocolTokenAdmin_TN = "Admin"  -- Admin token for Protocol access

-- Fund Token Identifiers  
fundID_TN :: TN
fundID_TN = "FundID"            -- NFT for Fund Datum UTXO

fundHoldingID_TN_basename :: BuiltinByteString 
fundHoldingID_TN_basename = "FundHoldingID"  -- Base for indexed holding NFTs

fundTokenAdmin_TN :: T.TN
fundTokenAdmin_TN = "Admin"

-- Other Tokens
investUnitID_TN :: TN
investUnitID_TN = "IUID"        -- NFT for InvestUnit Datum

swapOfferID_TN :: TN 
swapOfferID_TN = "SwapOfferID" -- NFT for SwapOffer Datum

delegationID_TN :: TN
delegationID_TN = "DelegationID" -- NFT for Delegation Datum

```

### MinMaxDef Type

```haskell
-- For values requiring min/max/default bounds
data MinMaxDef a = MinMaxDef {
    mmdMin :: a,       -- Minimum allowed value
    mmdMax :: a,       -- Maximum allowed value  
    mmdDef :: a        -- Default value
}

-- Helper functions
isInRange :: (Ord a) => MinMaxDef a -> a -> Bool
isInRange mmd value = mmdMin mmd <= value && value <= mmdMax mmd

isValidMinMaxDef :: (Ord a) => MinMaxDef a -> Bool 
isValidMinMaxDef mmd = 
    mmdMin mmd <= mmdMax mmd && 
    mmdMin mmd <= mmdDef mmd && 
    mmdDef mmd <= mmdMax mmd
```

### Investment Unit Type

```haskell
-- Core structure for token compositions
newtype InvestUnit = InvestUnit {
    iuValues :: [(CS, TN, Integer)]  -- List of token configs
    -- CurrencySymbol: Policy ID of token
    -- TokenName: Token name  
    -- Integer: Amount * 100 for 2 decimal precision
}

-- Used for value operations and re-indexing
type InvestUnitToken = (CS, TN, Integer)
```

### Oracle Data Types

```haskell
-- For re-indexing price data
data OracleReIdx_Data = OracleReIdx_Data {
    oridTokensPriceADA :: InvestUnit,  -- Token prices in ADA
    oridTime :: POSIXTime              -- Price timestamp
}

-- For swap offer price data  
data Oracle_Data = Oracle_Data {
    odFTPriceADA1xe6 :: InvestUnit,  -- FT prices in ADA * 1e6
    odTime :: POSIXTime              -- Price timestamp  
}
```

### FundCategory Type

```haskell
-- Defines fund types and requirements
data FundCategory = FundCategory {
    fcCategoryNumber :: Integer,  -- Sequential ID starting at 0
    fcRequiredMAYZ :: Integer,    -- MAYZ tokens needed for fund
    fcMaxUI :: Integer            -- Max Investment Unit value
}

instance Ord FundCategory where
    compare :: FundCategory -> FundCategory -> Ordering
    compare fc1 fc2 = compare (fcCategoryNumber fc1) (fcCategoryNumber fc2)
```

## Contract Components

### Protocol Contract
Core contract managing global protocol parameters and administrative functions.

#### Protocol Policy Parameters
```haskell
data PolicyParams = PolicyParams {
    ppProtocolPolicyID_TxOutRef :: TxOutRef  
    -- TxOutRef consumed during policy execution 
    -- Ensures single protocol instance by requiring specific UTXO
}
```

#### Protocol Validator Parameters
```haskell
data ValidatorParams = ValidatorParams {
    vpProtocolPolicyID_CS :: CS,          -- Protocol policy ID
    vpTokenEmergencyAdminPolicy_CS :: CS  -- Emergency admin token policy
}
```

#### Protocol Datum
```haskell
data ProtocolDatumType = ProtocolDatumType {
   -- Version Control
    pdProtocolVersion :: Integer,                -- Current version number
    
    -- Script Management
    pdScriptPolicyID_CS :: CS,                   -- Script policy ID
    pdScriptValidator_Hash :: ValidatorHash,      -- Script validator hash
    
    -- Oracle Configuration
    pdOraclePaymentPubKey :: PaymentPubKey,      -- Oracle's verification key
    pdOracleData_Valid_Time :: POSIXTime,        -- Time window for oracle data validity
    
    -- Access Control
    pdAdmins :: [WalletPaymentPKH],              -- Protocol admin PKHs
    pdDelegatorsAdmins :: [WalletPaymentPKH],    -- Delegator admin PKHs
    pdTokenAdminPolicy_CS :: CS,                 -- Admin token policy ID
    
    -- Fund Configuration
    pdFundCategories :: [FundCategory],           -- Available fund categories
    pdFundLifeTime :: MinMaxDef POSIXTime,        -- Fund duration constraints
    
    -- Required Stakes
    pdTokenMAYZ_AC :: AssetClass,                 -- MAYZ token identifier
    pdRequiredMAYZForSwapOffer :: Integer,        -- MAYZ needed for swap offers
    pdRequiredMAYZForBuyOrder :: Integer,         -- MAYZ needed for buy orders
    
    -- Commission Configuration (basis points x 1000)
    pdCommissionFund_PerYear_InBPx1e3 :: MinMaxDef Integer,     -- Fund commission range
    pdCommissionSwapOffer_InBPx1e3 :: MinMaxDef Integer,        -- Swap offer commission range
    pdCommissionBuyOrder_InBPx1e3 :: MinMaxDef Integer,         -- Buy order commission range
    
    -- Commission Distribution (basis points x 100)
    -- Must sum to 1,000,000 = 100%
    pdShare_InBPx1e2_Protocol :: Integer,     -- Protocol's share
    pdShare_InBPx1e2_Managers :: Integer,     -- Fund managers' share
    pdShare_InBPx1e2_Delegators :: Integer,   -- Delegators' share  
    
    -- Operation Limits
    pdMaxDepositAndWithdraw :: Integer,       -- Maximum single transaction amount

    -- Minimum ADA
    pdMinADA :: Integer                       -- Minimum ADA used in UTXO
}

newtype ValidatorDatum = ProtocolDatum ProtocolDatumType

PlutusTx.makeIsDataIndexed ''ProtocolDatumType [('ProtocolDatumType, 0)]
PlutusTx.makeIsDataIndexed ''ValidatorDatum [('ProtocolDatum, 0)]

```

#### Protocol Policy Redeemer

##### MintID
```haskell
-- There is no redeemer for this policy, because it executes only once to mint protocol ID NFT
-- Requires consumption of specific TxOutRef
```

#### Protocol Validator Redeemers

```haskell
data ValidatorRedeemer =
    -- Regular parameter updates
    ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    -- MinADA updates
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    -- Emergency updates
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType


PlutusTx.makeIsDataIndexed ''ValidatorRedeemer
    [ ('ValidatorRedeemerDatumUpdate, 0)
    , ('ValidatorRedeemerUpdateMinADA, 1)
    , ('ValidatorRedeemerEmergency, 2)
    ]

```

##### DatumUpdate
```haskell
data ValidatorRedeemerDatumUpdateType = ValidatorRedeemerDatumUpdateType
-- Used to update protocol parameters
-- Requires admin signature or admin token
-- Can modify almost all datum fields with some exeptions

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDatumUpdateType [('ValidatorRedeemerDatumUpdateType, 0)]
```

##### UpdateMinADA
```haskell
data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType
-- Updates minimum ADA used
-- Requires admin signature or admin token
-- Must ensure new value covers protocol operation costs

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]
```

##### Emergency
```haskell
data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType
-- Allows emergency updates with special admin token
-- Bypasses normal validation checks
-- Emergency token must be in output #0

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerEmergencyType [('ValidatorRedeemerEmergencyType, 0)]
```

### Fund Contract
Core contract managing investment fund lifecycle and parameters.

#### Fund Policy Parameters
```haskell
data PolicyParams = PolicyParams {
    ppProtocolPolicyID_CS :: CS,             -- Protocol policy ID
    ppFundPolicy_TxOutRef :: TxOutRef,       -- Ensures unique fund creation
    ppFundValidator_Hash :: ValidatorHash     -- Fund validator hash for outputs
}
```

#### Fund Validator Parameters
```haskell
data ValidatorParams = ValidatorParams {
    vpProtocolPolicyID_CS :: CS,                -- Protocol policy ID
    vpTokenEmergencyAdminPolicy_CS :: CS        -- Emergency admin token policy
}
```

#### Fund Datum
```haskell
data FundDatumType = FundDatumType {
    -- Version Control
    fdFundVersion :: Integer,                     

    -- Core Fund Info
    fdFundPolicy_CS :: CS,                        -- Fund policy ID
    fdFundFT_TN :: TN,                           -- Fund Token name
    fdFundValidator_Hash :: ValidatorHash,        -- Fund validator hash
    
    -- Component References
    fdFundHoldingPolicyID_CS :: CS,              -- Fund Holding policy ID
    fdFundHoldingValidator_Hash :: ValidatorHash, -- Fund Holding validator hash
    fdInvestUnitValidator_Hash :: ValidatorHash,  -- InvestUnit validator hash
    
    -- Access Control
    fdAdmins :: [WalletPaymentPKH],              -- Fund administrators
    fdTokenAdminPolicy_CS :: CS,                 -- Admin token policy
    
    -- Fund Properties
    fdFundCategoryNumber :: Integer,              -- Category identifier
    
    -- Time Constraints
    fdBeginAt :: POSIXTime,                      -- Fund start time
    fdDeadline :: POSIXTime,                     -- Fund end time
    fdClosedAt :: Maybe POSIXTime,               -- Early closure timestamp
    
    -- Commission Configuration
    fdCommission_PerYear_InBPx1e3 :: Integer,    -- Annual rate in BPx1000
    fdCommissions_Table_Numerator_1e6 :: [Integer], -- Pre-calculated values
    
    -- Holdings Management
    fdHoldingsCount :: Integer,                  -- Current holdings count
    fdHoldingsIndex :: Integer,                  -- Next holding index
    fdMaxDepositAndWithdraw :: Integer,          -- Max transaction size
    
    -- Token Requirements and used
    fdTokenMAYZ_AC :: AssetClass,                -- MAYZ token identifier
    fdRequiredMAYZ :: Integer,                   -- Required MAYZ stake used
    fdMinADA :: Integer                          -- Minimum ADA used in UTXO
}

newtype ValidatorDatum = FundDatum FundDatumType

PlutusTx.makeIsDataIndexed ''FundDatumType [('FundDatumType, 0)]
PlutusTx.makeIsDataIndexed ''ValidatorDatum [('FundDatum, 0)]

```

#### Fund Policy Redeemers
```haskell
data PolicyRedeemer =
    PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType
    | PolicyRedeemerMintFT PolicyRedeemerMintFTType
    | PolicyRedeemerBurnFT PolicyRedeemerBurnFTType

PlutusTx.makeIsDataIndexed ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 0)
    , ('PolicyRedeemerBurnID, 1)
    , ('PolicyRedeemerMintFT, 2)
    , ('PolicyRedeemerBurnFT, 3)
    ]
```

##### MintID
```haskell
data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType
-- Used to mint Fund ID and InvestUnit ID NFTs during fund creation
-- Requires valid Protocol Datum reference
-- Must mint correct IDs to correct validator addresses
-- Must verify fund parameters against protocol constraints

PlutusTx.makeIsDataIndexed ''PolicyRedeemerMintIDType [('PolicyRedeemerMintIDType, 0)]
```

##### BurnID
```haskell
data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType
-- Used when deleting fund
-- Burns Fund ID and InvestUnit ID NFTs
-- Requires FundHoldingsCount == 0
-- Must run with Fund Validator (ValidatorRedeemerDelete)

PlutusTx.makeIsDataIndexed ''PolicyRedeemerBurnIDType [('PolicyRedeemerBurnIDType, 0)]
```

##### MintFT
```haskell
data PolicyRedeemerMintFTType = PolicyRedeemerMintFTType
-- Used when minting Fund Tokens during deposits
-- Must run with FundHolding Validator (ValidatorRedeemerDeposit)
-- Amount determined by deposit size and commission calculations

PlutusTx.makeIsDataIndexed ''PolicyRedeemerMintFTType [('PolicyRedeemerMintFTType, 0)]
```

##### BurnFT
```haskell
data PolicyRedeemerBurnFTType = PolicyRedeemerBurnFTType
-- Used when burning Fund Tokens during withdrawals
-- Must run with FundHolding Validator (ValidatorRedeemerWithdraw)
-- Amount includes withdrawal plus any commission returns

PlutusTx.makeIsDataIndexed ''PolicyRedeemerBurnFTType [('PolicyRedeemerBurnFTType, 0)]
```

#### Fund Validator Redeemers
```haskell
data ValidatorRedeemer =
    ValidatorRedeemerDatumUpdate ValidatorRedeemerDatumUpdateType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerFundHoldingAdd ValidatorRedeemerFundHoldingAddType
    | ValidatorRedeemerFundHoldingDelete ValidatorRedeemerFundHoldingDeleteType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    | ValidatorRedeemerFinish ValidatorRedeemerFinishType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType

PlutusTx.makeIsDataIndexed ''ValidatorRedeemer
    [ ('ValidatorRedeemerDatumUpdate, 0)
    , ('ValidatorRedeemerUpdateMinADA, 1)
    , ('ValidatorRedeemerFundHoldingAdd, 2)
    , ('ValidatorRedeemerFundHoldingDelete, 3)
    , ('ValidatorRedeemerEmergency, 4)
    , ('ValidatorRedeemerFinish, 5)
    , ('ValidatorRedeemerDelete, 6)
    ]
```

##### DatumUpdate
```haskell
data ValidatorRedeemerDatumUpdateType = ValidatorRedeemerDatumUpdateType
-- Updates admin list and token admin policy
-- Requires current admin signature or admin token
-- Cannot modify other critical parameters

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDatumUpdateType
    [('ValidatorRedeemerDatumUpdateType, 0)]

```

##### UpdateMinADA
```haskell
data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType
-- Updates minimum ADA used
-- Requires fund admin signature or admin token
-- Must ensure new value covers fund operations

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerUpdateMinADAType
    [('ValidatorRedeemerUpdateMinADAType, 0)]
```

##### FundHoldingAdd
```haskell
data ValidatorRedeemerFundHoldingAddType = ValidatorRedeemerFundHoldingAddType
-- Creates new fund holding UTXO
-- Requires fund admin signature or admin token
-- Must mint correct holding ID token
-- Increments holdings count and index
-- Fund must be open

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundHoldingAddType
    [('ValidatorRedeemerFundHoldingAddType, 0)]
```

##### FundHoldingDelete
```haskell
data ValidatorRedeemerFundHoldingDeleteType = ValidatorRedeemerFundHoldingDeleteType
-- Removes fund holding UTXO
-- Requires fund admin signature or admin token
-- Must burn correct holding ID token
-- Decrements holdings count
-- Holding must have zero balances

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFundHoldingDeleteType
    [('ValidatorRedeemerFundHoldingDeleteType, 0)]
```

##### Emergency
```haskell
data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType
-- Allows emergency updates with special admin token
-- Bypasses normal validation checks
-- Emergency token must be in output #0

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]
```

##### Finish
```haskell
data ValidatorRedeemerFinishType = ValidatorRedeemerFinishType {
    rfDate :: POSIXTime    -- Timestamp for fund closure
}
-- Sets closedAt field to close fund
-- Requires fund admin signature or admin token
-- Must be before deadline
-- Prevents new deposits

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerFinishType
    [('ValidatorRedeemerFinishType, 0)]
```

##### Delete
```haskell
data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType
-- Removes fund completely
-- Requires fund admin signature or admin token
-- Must have zero holdings (holdingsCount == 0)
-- Burns Fund ID and InvestUnit ID tokens

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDeleteType
    [('ValidatorRedeemerDeleteType, 0)]
```

### InvestUnit Contract

Core contract managing fund token composition and reindexing operations.

#### InvestUnit Validator Parameters
```haskell
data ValidatorParams = ValidatorParams {
    vpProtocolPolicyID_CS :: CS,                -- Protocol policy ID
    vpTokenEmergencyAdminPolicy_CS :: CS        -- Emergency token policy
}
```

#### InvestUnit Datum
```haskell
data InvestUnitDatumType = InvestUnitDatumType {
    -- Version Control
    iuVersion :: Integer,                       -- Contract version
    
    -- Fund Reference
    iudFundPolicy_CS :: CS,                     -- Associated fund policy ID
    
    -- Token Composition
    iudInvestUnit :: T.InvestUnit,              -- Current token basket
    -- InvestUnit represents fund composition
    -- All amounts stored as Integer * 100 for 2 decimal places
    -- Must maintain divisibility properties
    
    -- Minimum ADA
    iudMinADA :: Integer                         -- Minimum ADA required
}

newtype ValidatorDatum = InvestUnitDatum InvestUnitDatumType

PlutusTx.makeIsDataIndexed
    ''InvestUnitDatumType
    [ ('InvestUnitDatumType, 0)
    ]

PlutusTx.makeIsDataIndexed
    ''ValidatorDatum
    [ ('InvestUnitDatum, 0)
    ]

```

#### InvestUnit Validator Redeemers
```haskell
data ValidatorRedeemer =
    ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerReIndexing ValidatorRedeemerReIndexingType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerReIndexing, 0)
    , ('ValidatorRedeemerUpdateMinADA, 1)
    , ('ValidatorRedeemerEmergency, 2)
    , ('ValidatorRedeemerDelete, 3)
    ]

```

##### UpdateMinADA
```haskell
data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType
-- Updates minimum ADA used
-- Requires fund admin signature or admin token
-- Must ensure new value covers InvestUnit operations
-- Updates minADA field in datum

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

```

##### ReIndexing
```haskell
data ValidatorRedeemerReIndexingType = ValidatorRedeemerReIndexingType {
    riuriTokensToAdd :: T.InvestUnit,               -- Tokens to add
    riuriTokensToRemove :: T.InvestUnit,            -- Tokens to remove
    riuriOracleReIdx_Data :: T.OracleReIdx_Data,    -- Oracle price data
    riuriOracleSignature :: Signature               -- Oracle signature
}
-- Updates fund composition during reindexing
-- Must run with FundHolding Validator (ValidatorRedeemerReIndexing)
-- Requires fund admin signature or admin token
-- Verifies oracle price data and signature
-- Maintains value equivalence using oracle prices
-- Ensures token divisibility properties
-- Updates invest unit composition in datum

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerReIndexingType
    [ ('ValidatorRedeemerReIndexingType, 0)
    ]
```

##### Emergency
```haskell
data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType
-- Allows emergency updates with special admin token
-- Bypasses normal validation checks
-- Emergency token must be in output #0

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]
```

##### Delete
```haskell
data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType
-- Removes InvestUnit when deleting fund
-- Must run with Fund Validator (ValidatorRedeemerDelete)
-- Requires fund admin signature or admin token
-- Must burn InvestUnit ID token

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDeleteType
    [('ValidatorRedeemerDeleteType, 0)]

```

#### Validation Functions
```haskell
-- Price validation helpers
isCorrect_Oracle_Signature :: BuiltinByteString -> PaymentPubKey -> Signature -> Bool
-- Verifies oracle signature on price data
-- Uses Ed25519 signature verification

isCorrect_Oracle_InRangeTime :: TxInfo -> POSIXTime -> POSIXTime -> Bool
-- Validates oracle data timestamp
-- Ensures price data is within validity window

-- Token balance helpers  
getDecimalsInInvestUnit :: [InvestUnitToken] -> Integer
-- Calculates maximum decimal precision in unit
-- Used to ensure token divisibility
```

### FundHolding Contract
Core contract managing individual fund holdings, optimizing for concurrency and commission handling.

#### Fund Holding Policy Parameters
```haskell
data PolicyParams = PolicyParams {
    ppFundPolicy_CS :: CS    -- Fund policy ID that can mint holding tokens
}
```

#### Fund Holding Validator Parameters
```haskell
data ValidatorParams = ValidatorParams {
    vpProtocolPolicyID_CS :: CS,                 -- Protocol policy ID
    vpFundPolicy_CS :: CS,                       -- Fund policy ID 
    vpTokenEmergencyAdminPolicy_CS :: CS         -- Emergency token policy
}
```

#### Fund Holding Datum
```haskell
data FundHoldingDatumType = FundHoldingDatumType {
    -- Version Control
    hdVersion :: Integer,                       -- Contract version
    
    -- Identification
    hdFundHolding_Index :: Integer,             -- Unique holding identifier
    
    -- Fund Token Accounting
    hdSubTotal_FT_Minted_Accumulated :: Integer,  -- Total FTs ever minted
    hdSubTotal_FT_Minted :: Integer,              -- Current FTs in circulation
    
    -- Commission Tracking
    hdSubTotal_FT_Commissions :: Integer,        -- Available commissions
    hdSubTotal_FT_Commissions_Total :: Integer   -- Total commissions, for calcs, is available plus collected
    hdSubTotal_FT_Commissions_Release_PerMonth_1e6 :: Integer,  -- Monthly release
    
    -- Commission Collections
    hdSubTotal_FT_Commissions_Collected_Protocol :: Integer,    -- Protocol total
    hdSubTotal_FT_Commissions_Collected_Managers :: Integer,    -- Managers total
    hdSubTotal_FT_Commissions_Collected_Delegators :: Integer,  -- Delegators total
    
    --- Minimum ADA
    hdMinADA :: Integer                         -- Minimum ADA used in UTXO
}

newtype ValidatorDatum = FundHoldingDatum FundHoldingDatumType

PlutusTx.makeIsDataIndexed
    ''FundHoldingDatumType
    [ ('FundHoldingDatumType, 0)
    ]

PlutusTx.makeIsDataIndexed
    ''ValidatorDatum
    [ ('FundHoldingDatum, 0)
    ]

```

#### Fund Holding Policy Redeemers
```haskell
data PolicyRedeemer =
    PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 0)
    , ('PolicyRedeemerBurnID, 1)
    ]
```

##### MintID
```haskell
data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType
-- Creates new fund holding ID NFT
-- Must run with Fund Validator (ValidatorRedeemerFundHoldingAdd)
-- Token name is fundHoldingID_TN_basename + index
-- Verifies correct holding index from fund datum

PlutusTx.makeIsDataIndexed ''PolicyRedeemerMintIDType [('PolicyRedeemerMintIDType, 0)]

```

##### BurnID
```haskell
data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType
-- Burns fund holding ID NFT
-- Must run with Fund Validator (ValidatorRedeemerFundHoldingDelete)
-- Must run with FundHolding Validator (ValidatorRedeemerDelete)
-- Verifies holding has zero balances

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [ ('PolicyRedeemerBurnIDType, 0)
    ]
```

#### Fund Holding Validator Redeemers
```haskell
data ValidatorRedeemer =
    ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerCollect_Protocol_Commission ValidatorRedeemerCollect_Protocol_CommissionType
    | ValidatorRedeemerCollect_Managers_Commission ValidatorRedeemerCollect_Managers_CommissionType
    | ValidatorRedeemerCollect_Delegators_Commission ValidatorRedeemerCollect_Delegators_CommissionType
    | ValidatorRedeemerReIndexing ValidatorRedeemerReIndexingType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    | ValidatorRedeemerBalanceAssets ValidatorRedeemerBalanceAssetsType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateMinADA, 0)
    , ('ValidatorRedeemerDeposit, 1)
    , ('ValidatorRedeemerWithdraw, 2)
    , ('ValidatorRedeemerCollect_Protocol_Commission, 3)
    , ('ValidatorRedeemerCollect_Managers_Commission, 4)
    , ('ValidatorRedeemerCollect_Delegators_Commission, 5)
    , ('ValidatorRedeemerReIndexing, 6)
    , ('ValidatorRedeemerBalanceAssets, 7)
    , ('ValidatorRedeemerEmergency, 8)
    , ('ValidatorRedeemerDelete, 9)
    ]
```

##### UpdateMinADA
```haskell
data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType
-- Updates minimum ADA used
-- Requires fund admin signature or admin token
-- Must ensure new value covers holding operations

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]
```

##### Deposit
```haskell
data ValidatorRedeemerDepositType = ValidatorRedeemerDepositType {
    rdDate :: POSIXTime,    -- Deposit timestamp
    rdAmount :: Integer     -- Deposit amount
}
-- Process user deposits
-- Must run with Fund Policy (PolicyRedeemerMintFT)
-- Calculates and reserves commissions
-- Updates minted and commission totals
-- Amount must respect token divisibility rules

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDepositType
    [ ('ValidatorRedeemerDepositType, 0)
    ]
```

##### Withdraw 
```haskell
data ValidatorRedeemerWithdrawType = ValidatorRedeemerWithdrawType {
    rwDate :: POSIXTime,               -- Withdrawal timestamp
    rwAmount :: Integer,               -- Base withdrawal amount
    rwAmountPlusComissions :: Integer  -- Total including commission refund
}
-- Process withdrawals and commission refunds
-- Must run with Fund Policy (PolicyRedeemerBurnFT)
-- Calculates available commission returns
-- Updates minted and commission totals
-- Amount must respect token divisibility rules

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerWithdrawType
    [ ('ValidatorRedeemerWithdrawType, 0)
    ]
```

##### Collect_Protocol_Commission
```haskell
data ValidatorRedeemerCollect_Protocol_CommissionType = ValidatorRedeemerCollect_Protocol_CommissionType {
    rcpcDate :: POSIXTime,    -- Collection timestamp
    rcpcAmount :: Integer     -- Commission amount
}
-- Collects protocol's commission share
-- Requires protocol admin signature or admin token
-- Verifies commission availability
-- Updates commission collection records

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_Protocol_CommissionType
    [ ('ValidatorRedeemerCollect_Protocol_CommissionType, 0)
    ]
```

##### Collect_Managers_Commission 
```haskell
data ValidatorRedeemerCollect_Managers_CommissionType = ValidatorRedeemerCollect_Managers_CommissionType {
    rcdcDate :: POSIXTime,    -- Collection timestamp
    rcdcAmount :: Integer     -- Commission amount
}
-- Collects fund managers' commission share
-- Requires fund admin signature or admin token
-- Verifies commission availability
-- Updates commission collection records

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_Managers_CommissionType
    [ ('ValidatorRedeemerCollect_Managers_CommissionType, 0)
    ]
```

##### Collect_Delegators_Commission
```haskell
data ValidatorRedeemerCollect_Delegators_CommissionType = ValidatorRedeemerCollect_Delegators_CommissionType {
    rcmcDate :: POSIXTime,    -- Collection timestamp
    rcmcAmount :: Integer     -- Commission amount
}
-- Collects delegators' commission share
-- Requires delegators admin signature
-- Verifies commission availability
-- Updates commission collection records

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_Delegators_CommissionType
    [ ('ValidatorRedeemerCollect_Delegators_CommissionType, 0)
    ]
```

##### ReIndexing
```haskell
data ValidatorRedeemerReIndexingType = ValidatorRedeemerReIndexingType {
    rriTokensToAdd :: T.InvestUnit,     -- New tokens to add 
    rriTokensToRemove :: T.InvestUnit   -- Tokens to remove
}
-- Modifies fund composition during reindexing
-- Must run with InvestUnit Validator (ValidatorRedeemerReIndexing)
-- Maintains total value using oracle prices
-- Updates token balances

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerCollect_Delegators_CommissionType
    [ ('ValidatorRedeemerCollect_Delegators_CommissionType, 0)
    ]

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerReIndexingType
    [ ('ValidatorRedeemerReIndexingType, 0)
    ]
```

##### BalanceAssets
```haskell
data ValidatorRedeemerBalanceAssetsType = ValidatorRedeemerBalanceAssetsType {
    rbCommissionsFT :: [Integer]    -- Commission movements between holdings
}
-- Redistributes assets between holdings
-- Requires fund admin signature or admin token
-- Maintains total balances
-- Updates commission records proportionally

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerBalanceAssetsType
    [ ('ValidatorRedeemerBalanceAssetsType, 0)
    ]

```

##### Delete
```haskell
data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType
-- Removes fund holding
-- Must run with Fund Validator (ValidatorRedeemerFundHoldingDelete)
-- Must run with FundHolding Policy (PolicyRedeemerBurnID)
-- Verifies zero balances
-- Burns holding ID NFT

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerDeleteType
    [ ('ValidatorRedeemerDeleteType, 0)
    ]
```

##### Emergency
```haskell
data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType
-- Allows emergency updates with special admin token
-- Bypasses normal validation checks
-- Emergency token must be in output #0

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]

```

### SwapOffer Contract

Core contract facilitating the exchange of Fund Tokens (FT) and ADA using oracle prices.

#### SwapOffer Policy Parameters
```haskell
data PolicyParams = PolicyParams {
    ppProtocolPolicyID_CS :: CS,                -- Protocol policy ID
    ppSwapOffer_Validator_Hash :: ValidatorHash  -- SwapOffer validator hash
}
```

#### SwapOffer Validator Parameters
```haskell
data ValidatorParams = ValidatorParams {
    vpProtocolPolicyID_CS :: CS,                -- Protocol policy ID
    vpTokenEmergencyAdminPolicy_CS :: CS        -- Emergency token policy
}
```

#### SwapOffer Datum
```haskell
data SwapOffer_DatumType = SwapOffer_DatumType {
    -- Version Control
    sodVersion :: Integer,                      -- Contract version
    
    -- Policy References
    sodSwapOfferPolicyID_CS :: CS,              -- SwapOffer policy ID
    sodFundPolicy_CS :: CS,                     -- Fund policy ID for FTs
    
    -- Seller Information
    sodSellerPaymentPKH :: WalletPaymentPKH,    -- Seller's payment key hash
    sodSellerStakePKH :: Maybe WalletPaymentPKH, -- Optional stake key hash
    
    -- Commission and Balances
    sodAskedCommission_InBPx1e3 :: Integer,     -- Seller's rate in BPx1000
    sodAmount_FT_Available :: Integer,           -- Available Fund Tokens
    sodAmount_ADA_Available :: Integer,          -- Available ADA
    
    -- Earnings Tracking
    sodTotal_FT_Earned :: Integer,              -- FT commissions earned
    sodTotal_ADA_Earned :: Integer,             -- ADA commissions earned
    
    -- Trading Options
    sodOrder_AllowSellFT :: Integer,            -- Allow FT sales (1=yes, 0=no)
    sodOrder_AllowSellADA :: Integer,           -- Allow ADA sales (1=yes, 0=no)
    sodOrder_Status :: Integer,                 -- 1=open, 2=closed
    
    -- Token Requirements and used
    sodTokenMAYZ_AC :: AssetClass,              -- MAYZ token identifier
    sodRequiredMAYZ :: Integer,                 -- Required MAYZ stake used
    sodMinADA :: Integer                        -- Minimum ADA used in UTXO
}

newtype ValidatorDatum = SwapOffer_Datum SwapOffer_DatumType

PlutusTx.makeIsDataIndexed ''SwapOffer_DatumType [('SwapOffer_DatumType, 0)]
PlutusTx.makeIsDataIndexed ''ValidatorDatum [('SwapOffer_Datum, 0)]

```

#### SwapOffer Policy Redeemers
```haskell
data PolicyRedeemer =
    PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 1)
    , ('PolicyRedeemerBurnID, 2)
    ]
```

##### MintID
```haskell
data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType
-- Used to mint SwapOffer ID NFT during creation
-- Requires valid Protocol and Fund Datum references
-- Verifies required MAYZ tokens are locked
-- Validates commission rate against protocol limits

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerMintIDType
    [('PolicyRedeemerMintIDType, 0)]
```

##### BurnID
```haskell
data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType
-- Used when deleting swap offer
-- Must run with SwapOffer Validator (ValidatorRedeemerDelete)
-- Requires zero available balances
-- Returns locked MAYZ tokens to seller

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [('PolicyRedeemerBurnIDType, 0)]
```

#### SwapOffer Validator Redeemers
```haskell
data ValidatorRedeemer =
    ValidatorRedeemerUpdateStatus ValidatorRedeemerUpdateStatusType
    | ValidatorRedeemerUpdateAskedCommissionRate ValidatorRedeemerUpdateAskedCommissionRateType
    | ValidatorRedeemerUpdateSellRestrictions ValidatorRedeemerUpdateSellRestrictionsType
    | ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType
    | ValidatorRedeemerSwapFTxADA ValidatorRedeemerSwapFTxADAType
    | ValidatorRedeemerSwapADAxFT ValidatorRedeemerSwapADAxFTType
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType
    | ValidatorRedeemerEmergency ValidatorRedeemerEmergencyType

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateStatus, 0)
    , ('ValidatorRedeemerUpdateAskedCommissionRate, 1)
    , ('ValidatorRedeemerUpdateSellRestrictions, 2)
    , ('ValidatorRedeemerUpdateMinADA, 3)
    , ('ValidatorRedeemerDeposit, 4)
    , ('ValidatorRedeemerWithdraw, 5)
    , ('ValidatorRedeemerSwapFTxADA, 6)
    , ('ValidatorRedeemerSwapADAxFT, 7)
    , ('ValidatorRedeemerDelete, 8)
    , ('ValidatorRedeemerEmergency, 9)
    ]
```

##### UpdateStatus
```haskell
data ValidatorRedeemerUpdateStatusType = ValidatorRedeemerUpdateStatusType {
    rusNewStatus :: Integer     -- New status (1=open, 2=closed)
}
-- Changes offer status
-- Requires seller signature
-- Updates order_Status field

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateStatusType [('ValidatorRedeemerUpdateStatusType, 0)]

```

##### UpdateAskedCommissionRate
```haskell
data ValidatorRedeemerUpdateAskedCommissionRateType = ValidatorRedeemerUpdateAskedCommissionRateType {
    rucrNewCommissionRate :: Integer    -- New rate in BPx1000
}
-- Updates commission rate
-- Requires seller signature
-- Must be within protocol limits
-- Updates askedCommission_InBPx1e3 field

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateAskedCommissionRateType [('ValidatorRedeemerUpdateAskedCommissionRateType, 0)]

```

##### UpdateSellRestrictions
```haskell
data ValidatorRedeemerUpdateSellRestrictionsType = ValidatorRedeemerUpdateSellRestrictionsType {
    rusrAllowSellFT :: Integer,   -- Allow FT sales (1/0)
    rusrAllowSellADA :: Integer   -- Allow ADA sales (1/0)
}
-- Updates trading restrictions
-- Requires seller signature
-- Updates order_AllowSellFT and order_AllowSellADA fields

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateSellRestrictionsType [('ValidatorRedeemerUpdateSellRestrictionsType, 0)]

```

##### UpdateMinADA
```haskell
data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType
-- Updates minimum ADA used
-- Requires seller signature
-- Must ensure new value covers operations

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

```

##### Deposit
```haskell
data ValidatorRedeemerDepositType = ValidatorRedeemerDepositType {
    rdNewDeposit_FT :: Integer,    -- FT amount to add
    rdNewDeposit_ADA :: Integer    -- ADA amount to add
}
-- Adds tokens to swap offer
-- Requires seller signature
-- Updates available amounts

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDepositType [('ValidatorRedeemerDepositType, 0)]

```

##### Withdraw
```haskell
data ValidatorRedeemerWithdrawType = ValidatorRedeemerWithdrawType {
    rwNewWithdraw_FT :: Integer,    -- FT amount to withdraw
    rwNewWithdraw_ADA :: Integer    -- ADA amount to withdraw
}
-- Removes tokens from swap offer
-- Requires seller signature
-- Updates available amounts

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerWithdrawType [('ValidatorRedeemerWithdrawType, 0)]

```

##### SwapFTxADA
```haskell
data ValidatorRedeemerSwapFTxADAType = ValidatorRedeemerSwapFTxADAType {
    rsfxaAmount_FT :: Integer,                  -- FT amount to swap
    rsfxaAmount_ADA :: Integer,                 -- ADA amount to receive
    rsfxaCommission_ADA :: Integer,             -- Commission in ADA
    rsfxaOracle_Data :: T.Oracle_Data,          -- Oracle price data
    rsfxaOracle_Signature :: Signature          -- Oracle signature
}
-- Swaps FT for ADA using oracle price
-- Verifies oracle data and signature
-- Validates price calculation
-- Calculates and collects commission
-- Updates available amounts and earnings
-- Requires order to be open and ADA selling allowed

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerSwapFTxADAType [('ValidatorRedeemerSwapFTxADAType, 0)]

```

##### SwapADAxFT
```haskell
data ValidatorRedeemerSwapADAxFTType = ValidatorRedeemerSwapADAxFTType {
    rsaxfAmount_ADA :: Integer,                 -- ADA amount to swap
    rsaxfAmount_FT :: Integer,                  -- FT amount to receive
    rsaxfCommission_FT :: Integer,              -- Commission in FT
    rsaxfOracle_Data :: T.Oracle_Data,          -- Oracle price data
    rsaxfOracle_Signature :: Signature          -- Oracle signature
}
-- Swaps ADA for FT using oracle price
-- Verifies oracle data and signature
-- Validates price calculation
-- Calculates and collects commission
-- Updates available amounts and earnings
-- Requires order to be open and FT selling allowed

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerSwapADAxFTType [('ValidatorRedeemerSwapADAxFTType, 0)]

```

##### Delete
```haskell
data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType
-- Removes swap offer
-- Requires seller signature
-- Must have zero balances
-- Must burn SwapOffer ID token
-- Returns MAYZ tokens to seller

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDeleteType [('ValidatorRedeemerDeleteType, 0)]

```

##### Emergency
```haskell
data ValidatorRedeemerEmergencyType = ValidatorRedeemerEmergencyType
-- Allows emergency updates with special admin token
-- Bypasses normal validation checks
-- Emergency token must be in output #0

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemerEmergencyType
    [('ValidatorRedeemerEmergencyType, 0)]
```

#### Validation Functions
```haskell
isCorrect_Conversion :: Oracle_Data -> Integer -> Integer -> Bool
-- Validates swap amounts using oracle price
-- Amount_Out = (Amount_In * Price) / Scale

isCorrect_Commission :: Integer -> Integer -> Bool
-- Validates commission calculation
-- Commission = (Amount * CommissionRate) / BPScale
```

### Delegation Contract

#### Delegation Policy Parameters
```haskell
data PolicyParams = PolicyParams {
    ppProtocolPolicyID_CS :: CS,                -- Protocol policy ID
    ppDelegation_Validator_Hash :: ValidatorHash -- Delegation validator hash
}
```

#### Delegation Validator Parameters
```haskell
data ValidatorParams = ValidatorParams {
    vpProtocolPolicyID_CS :: CS                 -- Protocol policy ID
}
```

#### Delegation Datum Fields
```haskell
data Delegation_DatumType = Delegation_DatumType {
    ddVersion :: Integer,
    ddDelegationPolicyID_CS :: CS,
    ddFundPolicy_CS :: CS,
    ddDelegatorPaymentPKH :: WalletPaymentPKH,
    ddDelegatorStakePKH :: Maybe WalletPaymentPKH,
    ddTokenMAYZ_AC :: AssetClass,
    ddDelegated_MAYZ :: Integer,
    ddMinADA :: Integer
}

newtype ValidatorDatum = Delegation_Datum Delegation_DatumType

PlutusTx.makeIsDataIndexed ''Delegation_DatumType [('Delegation_DatumType, 0)]
PlutusTx.makeIsDataIndexed ''ValidatorDatum [('Delegation_Datum, 0)]
```

#### Delegation Policy Redeemers
```haskell
data PolicyRedeemer = 
    PolicyRedeemerMintID PolicyRedeemerMintIDType
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 1)
    , ('PolicyRedeemerBurnID, 2)
    ]
```

##### MintID
```haskell
data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType
-- Used to mint Delegation ID NFT during creation
-- Requires valid Protocol Datum reference
-- Must mint ID to correct validator address
-- Locks MAYZ tokens and sets delegation amount
```

##### BurnID  
```haskell
data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType
-- Used when deleting delegation
-- Burns Delegation ID NFT
-- Returns locked MAYZ to delegator
-- Must run with Delegation Validator (ValidatorRedeemerDelete)
```

#### Delegation Validator Redeemers
```haskell
data ValidatorRedeemer =
    ValidatorRedeemerUpdateMinADA ValidatorRedeemerUpdateMinADAType
    | ValidatorRedeemerDeposit ValidatorRedeemerDepositType
    | ValidatorRedeemerWithdraw ValidatorRedeemerWithdrawType 
    | ValidatorRedeemerDelete ValidatorRedeemerDeleteType

PlutusTx.makeIsDataIndexed
    ''ValidatorRedeemer
    [ ('ValidatorRedeemerUpdateMinADA, 0)
    , ('ValidatorRedeemerDeposit, 1)
    , ('ValidatorRedeemerWithdraw, 2)
    , ('ValidatorRedeemerDelete, 3)
    ]

```

##### UpdateMinADA
```haskell
data ValidatorRedeemerUpdateMinADAType = ValidatorRedeemerUpdateMinADAType
-- Updates minimum ADA used
-- Only delegator can update
-- Must ensure new value covers delegation operations

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerUpdateMinADAType [('ValidatorRedeemerUpdateMinADAType, 0)]

```

##### Deposit
```haskell
data ValidatorRedeemerDepositType = ValidatorRedeemerDepositType
    { vrdDelegated_MAYZ_Change :: Integer
    } 
-- Adds MAYZ to delegation
-- Only delegator can deposit
-- Updates delegation amount

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDepositType [('ValidatorRedeemerDepositType, 0)]

```

##### Withdraw
```haskell  
data ValidatorRedeemerWithdrawType = ValidatorRedeemerWithdrawType
    { vrdwDelegated_MAYZ_Change :: Integer        
    }
-- Removes MAYZ from delegation  
-- Only delegator can withdraw
-- Updates delegation amount

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerWithdrawType [('ValidatorRedeemerWithdrawType, 0)]

```

##### Delete
```haskell
data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType
-- Removes delegation completely
-- Only delegator can delete  
-- Must burn Delegation ID NFT
-- Returns all MAYZ to delegator

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDeleteType [('ValidatorRedeemerDeleteType, 0)]

```

### Script Contract


#### Script Policy Parameters
```haskell
newtype PolicyParams = PolicyParams {
    ppProtocolPolicyID_CS :: CS,                -- Protocol policy ID
}
```

#### Script Validator Parameters
```haskell
data ValidatorParams = ValidatorParams {
    vpProtocolPolicyID_CS :: CS                 -- Protocol policy ID
    vpScriptPolicyID_CS :: CS                 -- Script policy ID
}
```

#### Script Datum
```haskell
data ScriptDatumType = ScriptDatumType {
    sdVersion :: Integer,                       -- Contract version
    sdScriptPolicyID_CS :: CS,                  -- Script policy ID
    sdScriptValidator_Hash :: ValidatorHash,     -- Script validator hash
    sdAdminPaymentPKH :: Maybe WalletPaymentPKH, -- Optional admin payment key hash
    sdAdminStakePKH :: Maybe WalletPaymentPKH,  -- Optional admin stake key hash
    sdMinADA :: Integer                         -- Minimum ADA requirement
}

newtype ValidatorDatum = ScriptDatum ScriptDatumType

PlutusTx.makeIsDataIndexed ''ScriptDatumType [('ScriptDatumType, 0)]
PlutusTx.makeIsDataIndexed ''ValidatorDatum [('ScriptDatum, 0)]

```

#### Script Policy Redeemers
```haskell
data PolicyRedeemer =
    PolicyRedeemerMintID PolicyRedeemerMintIDType  
    | PolicyRedeemerBurnID PolicyRedeemerBurnIDType

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemer
    [ ('PolicyRedeemerMintID, 1)
    , ('PolicyRedeemerBurnID, 2)
    ]

```

##### MintID
```haskell
data PolicyRedeemerMintIDType = PolicyRedeemerMintIDType
-- Used to mint Script ID NFT during registration
-- Sets script parameters like hash and admin info
-- Locks minimum required ADA in script address  

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerMintIDType
    [('PolicyRedeemerMintIDType, 0)]
```

##### BurnID
```haskell
data PolicyRedeemerBurnIDType = PolicyRedeemerBurnIDType  
-- Used when deleting script registration
-- Burns Script ID NFT 
-- Returns locked ADA to admin
-- Must run with Script Validator (ValidatorRedeemerDelete)

PlutusTx.makeIsDataIndexed
    ''PolicyRedeemerBurnIDType
    [('PolicyRedeemerBurnIDType, 0)]

```

#### Script Validator Redeemer  
```haskell
data ValidatorRedeemerDelete = ValidatorRedeemerDelete 
type ValidatorRedeemer = ValidatorRedeemerDelete

PlutusTx.makeIsDataIndexed ''ValidatorRedeemerDelete [('ValidatorRedeemerDelete, 0)]

```

##### Delete
```haskell
data ValidatorRedeemerDeleteType = ValidatorRedeemerDeleteType
-- Removes script registration  
-- Must be signed by admin if specified
-- Burns Script ID NFT
-- Returns locked ADA to admin
```

## Main Validation Patterns by Script

### Protocol Script
- Admin Authentication
  - Valid admin signature or admin token present
  - Emergency token validation for bypasses
- Parameter Validation
  - Commission rates within bounds
  - Share percentages sum to 100%
  - Fund categories properly structured
  - Time parameters logical
- Token Management
  - Single Protocol ID NFT
  - Proper validator addressing
  - Min ADA requirements

### Fund Script
- Admin Authentication
  - Fund admin signature or admin token
  - Protocol admin overlays
- State Management
  - Open/closed status checks
  - Timeline validations
  - Holdings count tracking
- Asset Management
  - MAYZ token requirements
  - Commission calculations
  - Value preservation
- Token Management
  - Fund ID and InvestUnit ID minting
  - FT minting/burning with holdings

### FundHolding Script
- Balance Management
  - Asset availability checks
  - Commission tracking
  - Monthly release calculations
- Operation Validation
  - Deposit/withdraw limits
  - Commission collection rights
  - Asset reindexing rules
- Token Management
  - Holding ID linking
  - FT balance tracking

### InvestUnit Script
- Composition Management
  - Token ratio validation
  - Divisibility rules
  - Reindexing value preservation
- Oracle Integration
  - Price data verification
  - Signature validation
  - Timestamp checks

### SwapOffer Script
- Trade Management
  - Status checks
  - Sell restrictions
  - Price calculations
- Oracle Integration
  - Price validation
  - Commission calculations
  - Time window checks
- Balance Management
  - Available amounts
  - Commission tracking
  - MAYZ requirements

### Common Validation Patterns
- Admin Rights
  - Signature verification
  - Token possession checks
  - Emergency bypasses
- Asset Operations
  - Balance sufficiency
  - Value preservation
  - Min ADA requirements
- Token Management
  - NFT uniqueness
  - Proper minting/burning
  - Validator addressing
- Time Management
  - Window validations
  - Deadline checks
  - Oracle freshness
- Commission Handling
  - Rate calculations
  - Share distribution
  - Release schedules

## Transaction Specifications

### Core Protocol Management

#### Protocol Management

##### Protocol_Create_Tx
Purpose: Initialize protocol with core parameters and settings
Signers: [Creator]
Inputs:
1. Protocol Policy TxOutRef: Required for one-time policy execution
Outputs:
1. New Protocol Datum UTXO + Protocol ID + Min ADA
Minting/Burning:
- Protocol ID: +1
Scripts & Redeemers:
1. Protocol Policy: MintID
Validations:
1. Protocol Policy: MintID
   1.1. Valid Protocol Policy TxOutRef consumed
   1.2. Minting exactly one Protocol ID token
   1.3. Protocol Datum parameters within valid ranges
   1.4. Share percentages sum to 100%
   1.5. Minimum ADA requirements met
   1.6. Protocol ID sent to correct validator address

##### Protocol_DatumUpdate_Tx
Purpose: Update protocol parameters within allowed ranges
Signers: [Protocol Admin or Admin Token Holder]
Inputs:
1. Protocol Datum UTXO + Protocol ID
Outputs:
1. Updated Protocol Datum UTXO + Protocol ID
Scripts & Redeemers:
1. Protocol Validator: DatumUpdate
Validations:
1. Protocol Validator: DatumUpdate
   1.1. Signed by admin or has admin token
   1.2. Valid parameter ranges maintained
   1.3. Share percentages still sum to 100%
   1.4. Protocol ID preserved
   1.5. Min ADA maintained

### Fund Lifecycle  

#### Fund Management

##### Fund_Create_Tx
Purpose: Create new investment fund with initial parameters
Signers: [Fund Creator with Required MAYZ]
Inputs:
1. Required MAYZ tokens
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
Outputs:
1. Fund Datum UTXO + Fund ID + Required MAYZ + Min ADA
2. InvestUnit Datum UTXO + InvestUnit ID + Min ADA
Minting/Burning:
- Fund ID: +1
- InvestUnit ID: +1
Scripts & Redeemers:
1. Fund Policy: MintID
2. Fund Validator: None (initial creation)
Validations:
1. Fund Policy: MintID
   1.1. Valid Protocol reference input
   1.2. Fund category exists and valid
   1.3. Required MAYZ amount locked
   1.4. Valid fund lifetime parameters
   1.5. Commission rates within protocol limits
   1.6. Correct IDs minted to proper addresses
   1.7. Initial invest unit composition valid

##### Fund_DatumUpdate_Tx
Purpose: Update fund parameters within allowed modifications
Signers: [Fund Admin or Admin Token Holder]
Inputs:
1. Fund Datum UTXO + Fund ID
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
Outputs:
1. Updated Fund Datum UTXO + Fund ID
Scripts & Redeemers:
1. Fund Validator: DatumUpdate
Validations:
1. Fund Validator: DatumUpdate
   1.1. Signed by fund admin or has admin token
   1.2. Only modifiable parameters changed (admins, token admin policy)
   1.3. Fund ID preserved
   1.4. Required MAYZ maintained
   1.5. Min ADA maintained

##### Fund_Finish_Tx
Purpose: Close fund for new deposits before deadline
Signers: [Fund Admin or Admin Token Holder]
Inputs:
1. Fund Datum UTXO + Fund ID
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
Outputs:
1. Updated Fund Datum UTXO + Fund ID (with closedAt set)
Scripts & Redeemers:
1. Fund Validator: Finish
Validations:
1. Fund Validator: Finish
   1.1. Signed by fund admin or has admin token
   1.2. Current time before deadline
   1.3. Fund not already closed
   1.4. Valid closure timestamp
   1.5. Fund ID and MAYZ maintained

##### Fund_Delete_Tx
Purpose: Remove fund completely after all holdings cleared
Signers: [Fund Admin or Admin Token Holder]
Inputs:
1. Fund Datum UTXO + Fund ID + MAYZ
2. InvestUnit Datum UTXO + InvestUnit ID
Outputs:
1. MAYZ returned to admin
Minting/Burning:
- Fund ID: -1
- InvestUnit ID: -1
Scripts & Redeemers:
1. Fund Policy: BurnID
2. Fund Validator: Delete
3. InvestUnit Validator: Delete
Validations:
1. Fund Policy: BurnID
   1.1. Burning correct Fund ID and InvestUnit ID
2. Fund Validator: Delete
   2.1. Signed by fund admin or has admin token
   2.2. Holdings count is zero
   2.3. MAYZ tokens properly returned
3. InvestUnit Validator: Delete
   3.1. Proper InvestUnit cleanup

#### Reindexing Operations

##### Fund_ReIndexing_Tx
Purpose: Modify fund token composition while maintaining value
Signers: [Fund Admin or Admin Token Holder]
Inputs:
1. FundHolding Datum UTXO + FundHolding ID + Current Assets
2. InvestUnit Datum UTXO + InvestUnit ID
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
2. Fund Datum UTXO + Fund ID
Outputs:
1. Updated FundHolding Datum UTXO + FundHolding ID + New Assets
2. Updated InvestUnit Datum UTXO + InvestUnit ID
Scripts & Redeemers:
1. FundHolding Validator: ReIndexing
2. InvestUnit Validator: ReIndexing
Validations:
1. FundHolding Validator: ReIndexing
   1.1. Signed by fund admin
   1.2. Valid oracle data and signature
   1.3. Equivalent value maintained using oracle prices
   1.4. Asset divisibility rules respected
2. InvestUnit Validator: ReIndexing
   2.1. Token changes match FundHolding changes
   2.2. New composition maintains valid ratios
   2.3. Oracle price verification
   2.4. Value equivalence verified

#### Fund Holdings Management

##### FundHolding_Create_Tx
Purpose: Create new holding instance for fund
Signers: [Fund Admin or Admin Token Holder]
Inputs:
1. Fund Datum UTXO + Fund ID
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
Outputs:
1. Updated Fund Datum UTXO + Fund ID (incremented holdings)
2. New FundHolding Datum UTXO + FundHolding ID + Min ADA
Minting/Burning:
- FundHolding ID: +1
Scripts & Redeemers:
1. Fund Validator: FundHoldingAdd
2. FundHolding Policy: MintID
Validations:
1. Fund Validator: FundHoldingAdd
   1.1. Signed by fund admin or has admin token
   1.2. Fund is open
   1.3. Holding index sequential
   1.4. Holdings count updated
2. FundHolding Policy: MintID
   2.1. Correct ID minting
   2.2. Proper validator addressing
   2.3. Initial state valid

##### FundHolding_Delete_Tx
Purpose: Remove empty fund holding
Signers: [Fund Admin or Admin Token Holder]
Inputs:
1. Fund Datum UTXO + Fund ID
2. FundHolding Datum UTXO + FundHolding ID
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
Outputs:
1. Updated Fund Datum UTXO + Fund ID (decremented holdings)
Minting/Burning:
- FundHolding ID: -1
Scripts & Redeemers:
1. Fund Validator: FundHoldingDelete
2. FundHolding Policy: BurnID
3. FundHolding Validator: Delete
Validations:
1. Fund Validator: FundHoldingDelete
   1.1. Signed by fund admin or has admin token
   1.2. Holdings count updated
2. FundHolding Policy: BurnID
   2.1. Proper ID burning
3. FundHolding Validator: Delete
   3.1. Zero balances (FT, commissions)
   3.2. Proper cleanup

### Investment Operations

#### Deposits & Withdrawals

##### Fund_Deposit_Tx
Purpose: Process user deposit into fund
Signers: [Depositor]
Inputs:
1. FundHolding Datum UTXO + FundHolding ID + Current Assets
2. Deposit Assets
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
2. Fund Datum UTXO + Fund ID
3. InvestUnit Datum UTXO + InvestUnit ID
Outputs:
1. Updated FundHolding Datum UTXO + FundHolding ID + Combined Assets + New FTs
Minting/Burning:
- Fund Tokens (FT): +deposit_amount
Scripts & Redeemers:
1. Fund Policy: MintFT
2. FundHolding Validator: Deposit
Validations:
1. Fund Policy: MintFT
   1.1. Correct amount based on deposit
   1.2. Only with valid FundHolding deposit
2. FundHolding Validator: Deposit
   2.1. Fund is open (not expired/closed)
   2.2. Deposit amount within limits
   2.3. Assets match InvestUnit composition
   2.4. Commission calculations correct
   2.5. FT minting amount verified

##### Fund_Withdraw_Tx
Purpose: Process user withdrawal from fund
Signers: [FT Holder]
Inputs:
1. FundHolding Datum UTXO + FundHolding ID + Current Assets + User's FTs
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
2. Fund Datum UTXO + Fund ID
3. InvestUnit Datum UTXO + InvestUnit ID
Outputs:
1. Updated FundHolding Datum UTXO + FundHolding ID + Remaining Assets
2. Withdrawn Assets to User
Minting/Burning:
- Fund Tokens (FT): -withdraw_amount
Scripts & Redeemers:
1. Fund Policy: BurnFT
2. FundHolding Validator: Withdraw
Validations:
1. Fund Policy: BurnFT
   1.1. Correct amount including commission returns
   1.2. Only with valid FundHolding withdrawal
2. FundHolding Validator: Withdraw
   2.1. Valid withdrawal amount
   2.2. Commission return calculations correct
   2.3. Assets returned match InvestUnit composition
   2.4. Sufficient balances available

#### Commission Collection

##### FundHolding_Collect_Protocol_Commission_Tx
Purpose: Collect protocol's share of commissions
Signers: [Protocol Admin or Admin Token Holder]
Inputs:
1. FundHolding Datum UTXO + FundHolding ID + Commissions
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
2. Fund Datum UTXO + Fund ID
Outputs:
1. Updated FundHolding Datum UTXO + FundHolding ID + Remaining Commissions
2. Collected Commissions to Protocol Admin
Scripts & Redeemers:
1. FundHolding Validator: Collect_Protocol_Commission
Validations:
1. FundHolding Validator: Collect_Protocol_Commission
   1.1. Signed by protocol admin
   1.2. Commission amount available
   1.3. Correct share percentage
   1.4. Monthly release limits respected
   1.5. Commission records updated

##### FundHolding_Collect_Managers_Commission_Tx
Purpose: Collect fund managers' share of commissions
Signers: [Fund Admin or Admin Token Holder]
Inputs:
1. FundHolding Datum UTXO + FundHolding ID
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
2. Fund Datum UTXO + Fund ID
Outputs:
1. Updated FundHolding Datum UTXO + FundHolding ID
2. Commissions to Fund Managers
Scripts & Redeemers:
1. FundHolding Validator: Collect_Managers_Commission
Validations:
1. FundHolding Validator: Collect_Managers_Commission
   1.1. Signed by fund admin
   1.2. Commission amount available
   1.3. Correct share percentage
   1.4. Monthly release limits
   1.5. Commission records updated

##### FundHolding_Collect_Delegators_Commission_Tx
Purpose: Collect delegators' share of commissions
Signers: [Delegators Admin]
Inputs:
1. FundHolding Datum UTXO + FundHolding ID
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
2. Fund Datum UTXO + Fund ID
Outputs:
1. Updated FundHolding Datum UTXO + FundHolding ID
2. Commissions to Delegators
Scripts & Redeemers:
1. FundHolding Validator: Collect_Delegators_Commission
Validations:
1. FundHolding Validator: Collect_Delegators_Commission
   1.1. Signed by delegators admin
   1.2. Commission amount available
   1.3. Correct share percentage
   1.4. Monthly release limits
   1.5. Commission records updated

### Trading Operations

#### Swap Offer Management

##### SwapOffer_Create_Tx
Purpose: Create new swap offer for FT/ADA trading
Signers: [Swap Offer Creator with Required MAYZ]
Inputs:
1. Required MAYZ tokens
2. Initial FT/ADA for liquidity
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
2. Fund Datum UTXO + Fund ID
Outputs:
1. SwapOffer Datum UTXO + SwapOffer ID + Required MAYZ + Initial Liquidity + Min ADA
Minting/Burning:
- SwapOffer ID: +1
Scripts & Redeemers:
1. SwapOffer Policy: MintID
Validations:
1. SwapOffer Policy: MintID
   1.1. Required MAYZ locked
   1.2. Commission rate within protocol limits
   1.3. Valid sell restrictions
   1.4. Proper SwapOffer ID minting
   1.5. Correct validator address

##### SwapOffer_UpdateStatus_Tx
Purpose: Change swap offer status (open/closed)
Signers: [SwapOffer Owner]
Inputs:
1. SwapOffer Datum UTXO + SwapOffer ID
Outputs:
1. Updated SwapOffer Datum UTXO + SwapOffer ID
Scripts & Redeemers:
1. SwapOffer Validator: UpdateStatus
Validations:
1. SwapOffer Validator: UpdateStatus
   1.1. Signed by owner
   1.2. Valid status value
   1.3. Assets preserved

##### SwapOffer_UpdateAskedCommissionRate_Tx
Purpose: Update swap offer commission rate
Signers: [SwapOffer Owner]
Inputs:
1. SwapOffer Datum UTXO + SwapOffer ID
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
Outputs:
1. Updated SwapOffer Datum UTXO + SwapOffer ID
Scripts & Redeemers:
1. SwapOffer Validator: UpdateAskedCommissionRate
Validations:
1. SwapOffer Validator: UpdateAskedCommissionRate
   1.1. Signed by owner
   1.2. Rate within protocol limits
   1.3. Assets preserved

##### SwapOffer_UpdateSellRestrictions_Tx
Purpose: Update swap offer trading restrictions
Signers: [SwapOffer Owner]
Inputs:
1. SwapOffer Datum UTXO + SwapOffer ID
Outputs:
1. Updated SwapOffer Datum UTXO + SwapOffer ID
Scripts & Redeemers:
1. SwapOffer Validator: UpdateSellRestrictions
Validations:
1. SwapOffer Validator: UpdateSellRestrictions
   1.1. Signed by owner
   1.2. Valid restriction values
   1.3. Assets preserved

##### SwapOffer_Delete_Tx
Purpose: Remove swap offer completely
Signers: [SwapOffer Owner]
Inputs:
1. SwapOffer Datum UTXO + SwapOffer ID + MAYZ
Outputs:
1. MAYZ and remaining assets to owner
Minting/Burning:
- SwapOffer ID: -1
Scripts & Redeemers:
1. SwapOffer Policy: BurnID
2. SwapOffer Validator: Delete
Validations:
1. SwapOffer Policy: BurnID
   1.1. Burning correct SwapOffer ID
2. SwapOffer Validator: Delete
   2.1. Signed by owner
   2.2. All assets returned
   2.3. MAYZ properly returned

#### Swap Executions

##### SwapOffer_Deposit_Tx 
Purpose: Add liquidity to swap offer
Signers: [SwapOffer Owner]
Inputs:
1. SwapOffer Datum UTXO + SwapOffer ID
2. Additional FT/ADA
Outputs:
1. Updated SwapOffer Datum UTXO + SwapOffer ID + Added Assets
Scripts & Redeemers:
1. SwapOffer Validator: Deposit
Validations:
1. SwapOffer Validator: Deposit
   1.1. Signed by owner
   1.2. Valid deposit amounts
   1.3. Balances updated correctly

##### SwapOffer_Withdraw_Tx
Purpose: Remove liquidity from swap offer
Signers: [SwapOffer Owner]
Inputs:
1. SwapOffer Datum UTXO + SwapOffer ID
Outputs:
1. Updated SwapOffer Datum UTXO + SwapOffer ID - Withdrawn Assets
2. Assets to Owner
Scripts & Redeemers:
1. SwapOffer Validator: Withdraw
Validations:
1. SwapOffer Validator: Withdraw
   1.1. Signed by owner
   1.2. Sufficient balances
   1.3. Amounts properly deducted

##### SwapOffer_SwapFTxADA_Tx
Purpose: Execute FT to ADA swap using oracle price
Signers: [Trader]
Inputs:
1. SwapOffer Datum UTXO + SwapOffer ID + Available FTs
2. Trader's FTs
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
2. Fund Datum UTXO + Fund ID
Outputs:
1. Updated SwapOffer Datum UTXO + SwapOffer ID + Updated Balances
2. ADA to Trader
Scripts & Redeemers:
1. SwapOffer Validator: SwapFTxADA
Validations:
1. SwapOffer Validator: SwapFTxADA
   1.1. Offer status is open
   1.2. ADA selling allowed
   1.3. Valid oracle data and signature
   1.4. Price calculation correct
   1.5. Commission calculation correct
   1.6. Sufficient balances available
   1.7. Within time window

##### SwapOffer_SwapADAxFT_Tx

### System Maintenance

#### Balance Operations

##### FundHolding_BalanceAssets_Tx
Purpose: Redistribute assets between fund holdings
Signers: [Fund Admin or Admin Token Holder]
Inputs:
1. Multiple FundHolding Datum UTXOs + FundHolding IDs
Reference Inputs:
1. Fund Datum UTXO + Fund ID
Outputs:
1. Updated FundHolding Datum UTXOs + FundHolding IDs
Scripts & Redeemers:
1. FundHolding Validator: BalanceAssets
Validations:
1. FundHolding Validator: BalanceAssets
   1.1. Signed by fund admin
   1.2. Total assets preserved
   1.3. Commission ratios maintained
   1.4. Valid redistribution amounts

#### MinADA Updates

##### Protocol_UpdateMinADA_Tx
Purpose: Update protocol minimum ADA used in the UTXO
Signers: [Protocol Admin or Admin Token Holder]
Inputs:
1. Protocol Datum UTXO + Protocol ID
Outputs:
1. Updated Protocol Datum UTXO + Protocol ID + Updated MinADA
Scripts & Redeemers:
1. Protocol Validator: UpdateMinADA
Validations:
1. Protocol Validator: UpdateMinADA
   1.1. Signed by admin or has admin token
   1.2. New value sufficient
   1.3. ADA amount adjusted

##### Fund_UpdateMinADA_Tx
Purpose: Update fund minimum ADA used in the UTXO
Signers: [Fund Admin or Admin Token Holder]
Inputs:
1. Fund Datum UTXO + Fund ID
Outputs:
1. Updated Fund Datum UTXO + Fund ID + Updated MinADA
Scripts & Redeemers:
1. Fund Validator: UpdateMinADA
Validations:
1. Fund Validator: UpdateMinADA
   1.1. Signed by admin or has admin token
   1.2. New value sufficient
   1.3. ADA amount adjusted

##### FundHolding_UpdateMinADA_Tx
Purpose: Update fundHolding minimum ADA used in the UTXO
Signers: [FundHolding Admin or Admin Token Holder]
Inputs:
1. FundHolding Datum UTXO + FundHolding ID
Outputs:
1. Updated FundHolding Datum UTXO + FundHolding ID + Updated MinADA
Scripts & Redeemers:
1. FundHolding Validator: UpdateMinADA
Validations:
1. FundHolding Validator: UpdateMinADA
   1.1. Signed by admin or has admin token
   1.2. New value sufficient
   1.3. ADA amount adjusted

##### InvestUnit_UpdateMinADA_Tx
Purpose: Update investUnit minimum ADA used in the UTXO
Signers: [InvestUnit Admin or Admin Token Holder]
Inputs:
1. InvestUnit Datum UTXO + InvestUnit ID
Outputs:
1. Updated InvestUnit Datum UTXO + InvestUnit ID + Updated MinADA
Scripts & Redeemers:
1. InvestUnit Validator: UpdateMinADA
Validations:
1. InvestUnit Validator: UpdateMinADA
   1.1. Signed by admin or has admin token
   1.2. New value sufficient
   1.3. ADA amount adjusted

##### SwapOffer_UpdateMinADA_Tx
Purpose: Update swapOffer minimum ADA used in the UTXO
Signers: [SwapOffer Admin or Admin Token Holder]
Inputs:
1. SwapOffer Datum UTXO + SwapOffer ID
Outputs:
1. Updated SwapOffer Datum UTXO + SwapOffer ID + Updated MinADA
Scripts & Redeemers:
1. SwapOffer Validator: UpdateMinADA
Validations:
1. SwapOffer Validator: UpdateMinADA
   1.1. Signed by admin or has admin token
   1.2. New value sufficient
   1.3. ADA amount adjusted

#### Script Management

##### Scritp_Add_Tx
Purpose: Register new script in protocol
Signers: [Protocol Admin or Admin Token Holder]
Inputs:
1. Min ADA for script
Reference Inputs:
1. Protocol Datum UTXO + Protocol ID
Outputs:
1. Script Datum UTXO + Script ID + Min ADA
Minting/Burning:
- Script ID: +1
Scripts & Redeemers:
1. Script Policy: MintID
Validations:
1. Script Policy: MintID
   1.1. Proper script parameters
   1.2. Valid admin info
   1.3. Sufficient Min ADA
   1.4. Correct validator addressing

##### Scritp_Remove_Tx
Purpose: Remove registered script
Signers: [Script Admin if specified]
Inputs:
1. Script Datum UTXO + Script ID
Outputs:
1. Min ADA returned to admin
Minting/Burning:
- Script ID: -1
Scripts & Redeemers:
1. Script Policy: BurnID
2. Script Validator: Delete
Validations:
1. Script Policy: BurnID
   1.1. Burning correct Script ID
2. Script Validator: Delete
   2.1. Signed by specified admin
   2.2. Proper cleanup

### Emergency & Recovery Operations 

##### Protocol_Emergency_Tx
Purpose: Emergency intervention in protocol parameters and state
Signers: [Emergency Admin Token Holder]
Inputs:
1. Protocol Datum UTXO + Protocol ID + Original State
Reference Inputs: None (bypasses normal reference requirements)
Outputs:
1. Protocol Datum UTXO + Protocol ID + Modified State
2. Emergency Admin Token UTXO (Must be output #0)
Scripts & Redeemers:
1. Protocol Validator: Emergency
Validations:
1. Protocol Validator: Emergency
   1.1. Emergency admin token present in output #0 (tokenEmergencyAdmin_TN)
   1.2. Protocol ID maintained in output
   1.3. Min ADA requirements still met
   1.4. All other validations bypassed
Key Properties:
- Can modify any protocol parameters
- Bypasses admin signature requirements
- Bypasses share percentage validations
- Bypasses parameter range checks
- Maintains only basic token and ADA requirements

##### Fund_Emergency_Tx
Purpose: Emergency intervention in fund state
Signers: [Emergency Admin Token Holder]
Inputs:
1. Fund Datum UTXO + Fund ID + Original State
Reference Inputs: None (bypasses normal reference requirements)
Outputs:
1. Fund Datum UTXO + Fund ID + Modified State
2. Emergency Admin Token UTXO (Must be output #0)
Scripts & Redeemers:
1. Fund Validator: Emergency
Validations:
1. Fund Validator: Emergency
   1.1. Emergency admin token present in output #0
   1.2. Fund ID maintained in output
   1.3. Min ADA requirements still met
   1.4. All other validations bypassed
Key Properties:
- Can modify any fund parameters
- Can change fund state regardless of timeline
- Bypasses admin signature requirements
- Bypasses holding count checks
- Maintains only basic token and ADA requirements

##### FundHolding_Emergency_Tx
Purpose: Emergency intervention in fund holding state
Signers: [Emergency Admin Token Holder]
Inputs:
1. FundHolding Datum UTXO + FundHolding ID + Original State + Assets
Reference Inputs: None (bypasses normal reference requirements)
Outputs:
1. FundHolding Datum UTXO + FundHolding ID + Modified State + Modified Assets
2. Emergency Admin Token UTXO (Must be output #0)
3. Optional: Assets moved to specified address
Scripts & Redeemers:
1. FundHolding Validator: Emergency
Validations:
1. FundHolding Validator: Emergency
   1.1. Emergency admin token present in output #0
   1.2. FundHolding ID maintained in output
   1.3. Min ADA requirements still met
   1.4. All other validations bypassed
Key Properties:
- Can modify any holding parameters
- Can move assets without normal restrictions
- Bypasses commission calculations
- Bypasses balance checks
- Can modify commission records
- Maintains only basic token and ADA requirements

##### SwapOffer_Emergency_Tx
Purpose: Emergency intervention in swap offer state
Signers: [Emergency Admin Token Holder]
Inputs:
1. SwapOffer Datum UTXO + SwapOffer ID + Original State + Assets
Reference Inputs: None (bypasses normal reference requirements)
Outputs:
1. SwapOffer Datum UTXO + SwapOffer ID + Modified State + Modified Assets
2. Emergency Admin Token UTXO (Must be output #0)
3. Optional: Assets moved to specified address
Scripts & Redeemers:
1. SwapOffer Validator: Emergency
Validations:
1. SwapOffer Validator: Emergency
   1.1. Emergency admin token present in output #0
   1.2. SwapOffer ID maintained in output
   1.3. Min ADA requirements still met
   1.4. All other validations bypassed
Key Properties:
- Can modify any swap offer parameters
- Can move assets without owner permission
- Bypasses status checks
- Bypasses commission calculations
- Can force close offering
- Maintains only basic token and ADA requirements

##### InvestUnit_Emergency_Tx
Purpose: Emergency intervention in invest unit composition
Signers: [Emergency Admin Token Holder]
Inputs:
1. InvestUnit Datum UTXO + InvestUnit ID + Original State
Reference Inputs: None (bypasses normal reference requirements)
Outputs:
1. InvestUnit Datum UTXO + InvestUnit ID + Modified State
2. Emergency Admin Token UTXO (Must be output #0)
Scripts & Redeemers:
1. InvestUnit Validator: Emergency
Validations:
1. InvestUnit Validator: Emergency
   1.1. Emergency admin token present in output #0
   1.2. InvestUnit ID maintained in output
   1.3. Min ADA requirements still met
   1.4. All other validations bypassed
Key Properties:
- Can modify token composition directly
- Bypasses oracle price checks
- Bypasses value preservation rules
- Bypasses divisibility requirements
- Maintains only basic token and ADA requirements

