# rsystrade v0.0.0

## Overview

-   Implements a variation of the *Starter System* from Robert Carver's book *Leveraged Trading*. This is intended as a kind of Minimal Viable Product.
-   Only takes data from file. No interaction with broker API. No database.
-   Assume:
    -   Use simulated data.
    -   Multiple rules (simple MAC provided).
    -   Multiple products (simple shares, no dividends, splits, etc).
    -   Single asset class.
    -   Units: Rounded to a whole number of shares. Minimum trade is 1 share.
    -   Invest all available capital.
    -   No position adjustment: Fixed position size for the duration of the 
        trade.
    -   Invest all available capital.
    -   No costs, no commissions.
    -   *Minimum notional exposure* is 1.
    -   FX rate is 1.
    -   No rolling (no back-adjustment needed).
    -   No *margining limits*.
    -   Borrowing cost is 0 (for leverage and short selling).
    -   Risk Target: 0.12.
    -   Sharpe Ratio: 0.24.
    -   Expected number of trades per year: 5.4.
    -   Stop loss fraction: 0.5.
    -   Percentage of capital at risk per trade: 6%.
    -   Simple compounding.
        -   Profits are added to capital at risk.
        -   Losses are subtracted from capital at risk.
        -   Some alternatives:
            -   Half compounding.
            -   "Three quarter" compounding.
    -   Expected value of scaled absolute signal is 1 with caps [-2, 2].

## Fixes

-   Nothing to fix. This is the first version!
