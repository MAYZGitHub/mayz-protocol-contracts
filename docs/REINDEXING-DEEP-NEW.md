
# 🧠 MAYZ Protocol – Reindexing: Concepts, Constraints & Strategies

This document outlines the technical and operational complexities behind the **reindexing process** in MAYZ Protocol — how it interacts with Investment Units (UIs), token precision, liquidity, and fund holdings.

---

## 🧩 What is an Investment Unit (UI)?

An **Investment Unit (UI)** is a fixed basket of tokens with specific proportions.

- Users do **not** interact directly with tokens.
- Instead, they interact with **amounts of UI**.
- When users **deposit or withdraw**, they are essentially moving **`X` amount of UI**, and the protocol handles the underlying tokens automatically.

```
Real tokens moved = number_of_UIs × composition_of_each_UI
```

This means that **any change to the UI composition** (which is what reindexing does) must be compatible with the total number of UIs in existence.

---

## 🔁 What is Reindexing?

Reindexing is the process that:

- **Modifies the composition of the UI** (changing which tokens and in what proportion).
- **Updates the actual token holdings** in the Fund Holdings (FHs) to match the new UI.

In other words:

> Reindexing changes **what each UI means**, and ensures the real tokens across the system are updated to reflect this new definition.

---

## 🔁 Deposits and Withdrawals in Context

When a user deposits or withdraws:

- They are not moving "UI" directly.
- Instead, they are depositing or withdrawing the **full token composition** of one or more UIs.

So, for any operation to be valid:

```
token_amount_per_UI × number_of_UIs = integer
```

📌 Because on Cardano, **tokens must be whole numbers** — decimals are only for display.

---

## 🧮 Why Allow Decimals in the UI?

Even though tokens are stored as integers, MAYZ allows **up to 2 decimal places** in the token values defined in a UI, to:

- Facilitate reindexing without requiring huge token movements.
- Provide more **granular control** over proportions.

---

## ⚠️ Why Only 2 Decimal Places?

Allowing more decimal places introduces stricter limitations.

- If you allowed up to 6 decimals, to preserve integer compatibility you'd need to deposit or withdraw **1,000,000 UIs** to cancel out the fractional part.
- With only 2 decimals, you only need **multiples of 100 UIs**, which is much more practical.

📌 Example:

- UI defines `0.01` of a token.
- You want to withdraw `1 token`.
- You need to withdraw `100 UI` → `0.01 × 100 = 1.00` ✅

---

## 🎯 Ideal Case: No Decimals at All

The ideal setup is that **each UI has only whole numbers** for all tokens.

Advantages:

- All deposits and withdrawals will automatically yield integer totals.
- No need to adjust or validate precision.
- Safer and easier to manage on-chain.

---

## 💡 UI Design Tip: Use Large Quantities Per Token

Why?

If the **minimum editable value per token** is `x_min`, and the token amount in the UI is small:

- A token with 10 units in the UI and `x_min = 1.00` → **10% minimum change**
- A token with 1,000,000 units and `x_min = 1.00` → **0.0001% minimum change**

Larger token amounts in the UI give:

- More precise reindexing.
- Lower operational cost.
- Smoother adjustments without breaking proportion.

---

## ⚠️ Reindexing Complexities

### 1. 🔀 Fund Holdings (FHs)

Each fund can have **one or multiple Fund Holdings** — smart contracts that hold the actual tokens.

- Reindexing **only operates on one FH at a time**.
- If a specific FH doesn’t have enough of the needed tokens, the reindex will **fail**, even if other FHs have enough.

#### Why have multiple FHs?

✅ To **enable concurrent deposits and withdrawals** by multiple users at the same time, without conflicts or race conditions.

#### Why is it a problem for reindexing?

❌ If tokens are scattered, the active FH used in the reindex might not have all needed tokens. Reindex fails unless tokens are moved.

📌 **Recommendation**:
- Funds that **prioritize user concurrency** should use **multiple FHs**, and implement **token rebalancing** between them before and after reindex.
- Funds that **reindex frequently** may prefer having a **single FH**, simplifying token access during reindexing.

---

### 2. 💧 Liquidity

- Any change in the UI is **multiplied by the total number of UIs minted**.
- So, even a small change in the UI can require large liquidity if many UIs exist.

#### Why we introduced decimals in UI

To **allow changes that move small amounts of tokens** during reindex, MAYZ allows up to **2 decimal places** in the token quantities defined in a UI.

- Without decimals, a small change would require handling very large token amounts.
- With 2 decimals, reindexing can operate with fractional changes, reducing required liquidity per reindex operation.

---

### 3. 📐 Decimal Precision & Minimum Editable Value

Since **Cardano tokens are always integers**, the product:

```
token_amount_per_UI × number_of_UI
```

must always be an integer.

That means: if you define a token with `0.01` in the UI, and there are 101 UIs, the result `0.01 × 101 = 1.01` is invalid. It must be a whole number.

#### ✅ Minimum change allowed in reindexing:

This is **the minimum valid number that can be used in editing the UI** during a reindex operation.

```
x_min = 1 / gcd(100, N)
```

Where:

- `N` is the total number of UIs minted.
- `gcd` is the greatest common divisor.

#### Example:

- If `N = 1245`, `gcd(100, 1245) = 5`, so `x_min = 1 / 5 = 0.20`.
- Any change smaller than 0.20 in that case would break integer compatibility.

---

### 🔁 How to Improve Precision: Adjusting `N`

To reduce `x_min` and allow finer adjustments, we can adjust `N` to improve divisibility.

#### Example:

- `N = 67,548,864,421` → `gcd(100, N) = 1` → `x_min = 1.00` (very coarse)
- To allow `x_min = 0.01`, we need `gcd(100, N) = 100` → i.e., `N` must be divisible by 100.

So:

- Subtract `21` → `N = 67,548,864,400`
- Or add `79` → `N = 67,548,864,500`

This means:
- Deposit or withdraw 21 or 79 UIs to reach a better value of `N` where `x_min = 0.01`.

---

### 4. 💰 Pricing Logic

Reindexing must always respect value equivalence:

```
Total value withdrawn ≤ Total value deposited
```

This ensures users do not receive more tokens than they contribute. If a reindex would result in a value mismatch, the operation must be rejected.

---

## ✅ Summary

| Topic           | Complexity                                                               | Recommendation / Solution                                     |
|----------------|---------------------------------------------------------------------------|----------------------------------------------------------------|
| Fund Holdings   | Reindex uses only one FH; token availability may be fragmented           | Use 1 FH for simplicity, or balance tokens between multiple FHs |
| Liquidity       | Changes multiply with number of UIs, requiring large token amounts       | Use decimals to reduce token impact per UI                     |
| Decimal Rules   | Tokens are integers; changes must produce whole numbers                  | Ensure `x × N` is integer. Use `x_min = 1 / gcd(100, N)`       |
| Improving x_min | `x_min` depends on N; too large = coarse edits only                      | Adjust N to a multiple of 100                                  |
| Pricing         | Cannot withdraw more value than deposited                                | Validate all token price deltas                                |

---

For protocol devs and UI designers: reindexing is a low-level operation with high impact. Handling these constraints correctly ensures robust and predictable asset management in MAYZ.
