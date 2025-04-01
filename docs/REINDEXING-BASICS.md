# MAYZ Protocol: Fund Re-indexing and Balancing System

## Table of Contents
- [MAYZ Protocol: Fund Re-indexing and Balancing System](#mayz-protocol-fund-re-indexing-and-balancing-system)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Fund Basics](#fund-basics)
  - [Investment Unit (IU)](#investment-unit-iu)
  - [Fund Holdings and UTXOs](#fund-holdings-and-utxos)
  - [Re-indexing (Reidx) Process Overview](#re-indexing-reidx-process-overview)
    - [Reidx Flexibility:](#reidx-flexibility)
  - [Balancing Process Overview](#balancing-process-overview)
  - [Datum Fields and Potential Discrepancies](#datum-fields-and-potential-discrepancies)
  - [Withdrawals and Commissions](#withdrawals-and-commissions)
  - [Risks and Complex Situations](#risks-and-complex-situations)
    - [Reidx Limitations:](#reidx-limitations)
    - [Balancing Challenges:](#balancing-challenges)
    - [Commission Synchronization:](#commission-synchronization)
  - [Technical Challenges and Design Decisions](#technical-challenges-and-design-decisions)
    - [Ideal vs. Practical Approach:](#ideal-vs-practical-approach)
    - [Resource Optimization:](#resource-optimization)
    - [Fund Manager Considerations:](#fund-manager-considerations)
  - [Conclusion](#conclusion)

## Introduction

The MAYZ Protocol is an innovative decentralized finance (DeFi) system built on the Cardano blockchain, designed to facilitate the creation and management of investment funds. This document provides a comprehensive overview of the re-indexing and balancing processes within the MAYZ Protocol's Fund system, including their implications, limitations, and potential risks.

## Fund Basics

MAYZ Protocol funds operate through deposits and withdrawals. When users deposit tokens into a fund, they receive Fund Tokens (FTs) in return. These FTs represent the user's share in the fund and can be later redeemed for the underlying tokens.

Example:
- Action: User Deposits
- Tokens Deposited: 100 TokenA, 200 TokenB
- FTs Received: 100 FTs minted and given to user
(TODO: cuando se depositan se pagan comisiones por el tiempo de vida que le queda al fondo, y parte de ellas pueden ser recuperadas si se hace un wthdraw y se queman los tokens FT del usuario)

FTs serve as a standardized unit of account within the fund, allowing users to easily track their investment and perform operations like withdrawals or transfers.

## Investment Unit (IU)

The Investment Unit (IU) is a fundamental concept in the MAYZ Protocol, defining the token composition of each fund. 

Key Characteristics:
- Can have up to two decimal places for each token amount
- Impacts deposit and withdrawal operations

Examples of IUs:
1. 100 TokenA, 200 TokenB
2. 10.5 TokenA, 20.0 TokenB
3. 1.05 TokenA, 2.00 TokenB

Impact on Deposits and Withdrawals:
- No decimals: Any whole number of IUs
- One decimal: Multiples of 10 IUs
- Two decimals: Multiples of 100 IUs

Examples:
1. IU: 10.5 TokenA, 20.0 TokenB
   - Deposit: 315 TokenA (30 IUs), 600 TokenB (30 IUs)
   - Withdraw: 210 TokenA (20 IUs), 400 TokenB (20 IUs)

2. IU: 1.05 TokenA, 2.00 TokenB
   - Deposit: 210 TokenA (200 IUs), 400 TokenB (200 IUs)
   - Withdraw: 315 TokenA (300 IUs), 600 TokenB (300 IUs)

## Fund Holdings and UTXOs

Each fund in the MAYZ Protocol consists of:
- An Investment Unit (IU) defining the fund's composition
- Multiple Fund Holdings UTXOs where actual tokens are held

Key points:
- Multiple UTXOs allow for concurrency in user operations
- Each UTXO contains a portion of the fund's total assets
- Calculating fund totals requires summing subTotals from all UTXOs
- Operating on many UTXOs is resource-intensive and limited by transaction constraints

Challenges:
- Re-indexing operations become complex with multiple UTXOs
- Changing the IU requires withdrawing and depositing tokens in Fund Holdings
- Resource limitations may prevent affecting all UTXOs in a single transaction
- Unbalanced UTXOs may result from partial re-indexing operations

These challenges necessitate the balancing process and introduce the concept of "unbalanced" Fund Holdings.

## Re-indexing (Reidx) Process Overview

The re-indexing process allows for changes in the IU composition. It's performed on a single Fund Holding UTXO but affects the entire fund.

Reidx Constraints:
- The new IU must not have more than two decimal places per token
- The total value of the fund in ADA must remain unchanged (verified using an oracle for token prices)
- For each token (including ADA), the following must be true:
  (token_amount * total_IUs_deposited) must be divisible by 100

### Reidx Flexibility:
1. Less restrictive IU:
   Current IU: 100 TokenA, 200 TokenB
   Total IUs: 1,234,567
   Possible reidx: Add 1 TokenC (results in 1,234,567 TokenC total added)

2. More restrictive IU:
   Current IU: 1.05 TokenA, 2.00 TokenB
   Total IUs: 1,200,000 (must be multiple of 100 due to IU decimals)
   Possible reidx: Add 0.73 TokenC (results in 876,000 TokenC total)

In the second case, because the total IUs are a multiple of 100, we have more flexibility and granularity in choosing the amount of new token to add.

For a detailed explanation of the re-indexing methodology, please refer to the separate [Re-indexing Methodology](./REINDEXING-DEEP.md) document.

## Balancing Process Overview

Balancing is necessary after reidx to redistribute tokens across all Fund Holding UTXOs.

Key Points:
- Must operate on two UTXOs at a time.
- Modifies the hdSubTotal_FT_Commissions and hdSubTotal_FT_Commissions_Release_PerMonth_1e6 fields in the Fund Holding datum to reflect proportional changes.
- Can move tokens between UTXOs, but the first input must always have non-zero deposits to calculate the changeRatio to be used.
- Maintains minimum ADA as specified in each datum.

## Datum Fields and Potential Discrepancies

The Fund Holding datum contains several important fields:

1. hdSubTotal_FT_Minted
2. hdSubTotal_FT_Minted_Accumulated
3. hdSubTotal_FT_Commissions
4. hdSubTotal_FT_Commissions_Release_PerMonth_1e6

After balancing, fields 1 (hdSubTotal_FT_Minted) and 2 (hdSubTotal_FT_Minted_Accumulated) may become out of sync with the actual token amounts in the UTXO. Fields 3 (hdSubTotal_FT_Commissions) and 4 (hdSubTotal_FT_Commissions_Release_PerMonth_1e6) will be updated to reflect the proportional changes made during balancing, ensuring the correct synchronization of commissions and release values.

## Withdrawals and Commissions

When a user withdraws:
- They receive underlying tokens proportional to the FTs they're returning
- They also receive a portion of the accumulated commissions. These commissions were paid by the users themselves when making deposits, based on the remaining life of the fund at the time of deposit. When withdrawing, they recover part of these commissions, related to the remaining life of the fund at the time of withdrawal.
- If there are decimals in the IU, the commission return is limited to multiples of 10 or 100 FTs
- Users can opt to receive fewer commissions than they're entitled to, to avoid blocking the withdrawal

Example:
User withdraws 1000 FTs
Entitled to 56 FTs worth of commissions
IU has one decimal and limit the amount to be multiple of 10.
User can choose to receive 0, 10, 20, 30, 40, or 50 FTs worth of commissions

In this case, the user is limited to a maximum of 50 FTs worth of commissions due to the IU decimal constraint.

## Risks and Complex Situations

### Reidx Limitations:
- If a new token is added during reidx, all UTXOs may become unusable for complete withdrawals

Example:
Initial IU: 100 TokenA, 200 TokenB
After reidx (adding TokenC and removing some TokenB):
- UTXO1 (used for reidx): 1000 TokenA, 1500 TokenB, 500 TokenC
- UTXO2: 1000 TokenA, 2000 TokenB, 0 TokenC

Both UTXOs are now problematic for complete withdrawals.

### Balancing Challenges:
- Large number of UTXOs might make balancing transactions too large for the network
- Uneven distribution of tokens might make perfect balancing between fewer UTXOs impossible in a single transaction

Due to these challenges, the MAYZ Protocol has adopted a pragmatic balancing approach:

1. The chosen methodology does not aim for perfect balancing where all UTXOs have correct datums and values. Such an approach would be infeasible due to the reasons mentioned above.

2. The current balancing process operates on two UTXOs at a time, allowing free movement of underlying tokens between them, provided the total token amount remains consistent across inputs and outputs. Additionally, it enables the transfer of commission FTs, with proportional updates to both the commission and release per month values.
   
3. The first UTXO must have non-zero deposits, as the changeRatio is calculated based on its values. 
   
4. The total of tokens must remain constant between inputs and outputs.
   
It's crucial to note that while individual datum values may become out of sync with their UTXO's actual token holdings, these values serve important purposes:

1. Statistical tracking: The datum values provide historical information about the fund's operations.

2. Aggregate accuracy: The sum of these values across all fund holdings remains meaningful and accurate at the fund level.

For example, to determine the total minted tokens of a fund, one must sum the 'hdSubTotal_FT_Minted' fields across all fund holdings. This sum remains accurate even if individual values may be negative due to withdrawals exceeding deposits in a particular UTXO.

This approach strikes a balance between operational feasibility and maintaining essential fund-level accuracy, albeit at the cost of some UTXO-level discrepancies.

### Commission Synchronization:
The calculation of available commissions is critical:

```
commisionsReady = totalComisions - (monthsRemaining * releasePerMonth)
```

If the commission FTs (totalComisions) and the releasePerMonth become out of sync, it could lead to:
- Incorrect commission payments
- Possibility of withdrawing more commissions than actually available

Example:
5 months remaining:

- Initial state:
UTXO1: 100 commission FTs, releasePerMonth 10 FTs/month

```
commisionsReady1 = 100 - (5 * 10) = 50
```

UTXO2: 50 commission FTs, releasePerMonth 5 FTs/month

```
commisionsReady2 = 50 - (5 * 5) = 25
```

Total commissions ready: 50 + 25 = 75 FTs

- If a withdrawal in UTXO2 of 50 FT occurs that reduces the releasePerMonth to 0:
  
```
commisionsReady2 = 0 - (5 * 0) = 0
```

Total commissions ready: 50 + 0 = 50 FTs


- Now with incorrect balancing (move 90 FT and not update the releasePerMonth):
  
UTXO1: 100 - 90 = 10 commission FTs, releasePerMonth 10 FTs/month

```
commisionsReady1 = 10 - (5 * 10) = -40
```

These 10 commissions cannot be collected due to negative commisionsReady.

UTXO2: 50 + 90 = 140 commission FTs, releasePerMonth 5 FTs/month

```
commisionsReady2 = 140 - (5 * 5) = 115
```

- If a withdrawal in UTXO2 of 50 FT occurs that reduces the releasePerMonth to 0:
  
```
commisionsReady2 = 90 - (5 * 0) = 90
```

90 FTs become available, which is significantly more than the original 50 FTs without the incorrect balancing.

This discrepancy demonstrates the importance of keeping commission FTs and rates in sync across UTXOs.

- Now with correct balancing (move 90 FT and update the releasePerMonth):
  
UTXO1: 100 - 90 = 10 commission FTs, releasePerMonth = 10 - 9 = 1 FTs/month

```
commisionsReady1 = 10 - (5 * 1) = 5   0
```

UTXO2: 140 commission FTs, releasePerMonth (5 + 9) 14 FTs/month

```
commisionsReady2 = 140 - (5 * 14) = 70
```

Total commissions ready: 5 + 70 = 75 FTs

- If a withdrawal in UTXO2 of 50 FT occurs that reduces the releasePerMonth to 9:
  
```
commisionsReady2 = 90 - (5 * 9) = 45
```

Total commissions ready: 5 + 45 = 50 FTs

## Technical Challenges and Design Decisions

Creating a fully functional system without re-indexing blockages, where no fund manager has absolute control over users' funds, presents significant technical challenges. 

### Ideal vs. Practical Approach:
Ideally, re-indexing would be performed in a single operation, leaving all UTXOs balanced with perfectly calculated new IUs. However, this is technically challenging due to network resource limitations.

### Resource Optimization:
When consuming multiple UTXOs from the same validator in a single transaction, the validator executes repeatedly for each input, multiplying resource usage. A potential optimization is to verify all inputs and outputs in the first execution, and in subsequent executions, only check that the first execution occurred. This approach reduces redundant executions but introduces complexity and risks that require thorough study and testing.

### Fund Manager Considerations:
Fund Manager Considerations:
- Re-indexing funds with millions of tokens is challenging and capital-intensive for managers
- The system allows for re-indexing with two decimals in the investment unit to provide more flexibility

## Conclusion

The MAYZ Protocol's Fund system provides flexibility through its re-indexing and balancing mechanisms. However, these processes introduce complexities and potential risks that need careful management. The system's design choices, such as limiting reidx to a single UTXO and maintaining strict rules for commission FTs, aim to balance flexibility with security and consistency.

By prioritizing user control and system functionality, the MAYZ Protocol strives to create a robust, decentralized investment fund platform. Ongoing monitoring and potential refinements will be crucial to address identified risks and edge cases, ensuring the system's long-term viability and user satisfaction.