
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsystrade

<!-- badges: start -->
<!-- badges: end -->

*rsystrade* is a framework for researching, testing and executing
systematic trading strategies.

## Warning and disclaimer

This software is in early development and is highly experimental!

- Do not expect this software to run properly.
- This software is not safe for live trading.
- This software comes with no guarantees or warranties.  
- This software is for expert use only.  
- No support is provided.  
- This software was written as a personal learning project.  
- If you trade, invest and/or speculate you may lose a lot of money even
  if you know what you are doing, and much more so if you don’t know
  what you are doing. You may very well lose all or more than you have.

It takes considerable effort to do live trading with rsystrade. If you
do so, it is clear that you did it with intent and against good
judgement.

The advice above is the only advice I will offer on rsystrade.

## About this version

- Implements a variation of the *Starter System* from Robert Carver’s
  book *Leveraged Trading*. This is intended as a kind of Minimal Viable
  Product
- Only takes data from CSV file. No interaction with broker API. No
  database.
- Assume:
  - Use simulated data.
  - Multiple rules (simple MAC provided).
  - Multiple products (simple shares, no dividends, splits, etc).
  - Single asset class.
  - Units: Rounded to a whole number of shares. Minimum trade is 1
    share.
  - Invest all available capital.
  - No position adjustment: Fixed position size for the duration of the
    trade.
  - Invest all available capital.
  - No costs, no commissions.
  - *Minimum notional exposure* is 1.
  - FX rate is 1.
  - No rolling (no back-adjustment needed).
  - No *margining limits*.
  - Borrowing cost is 0 (for leverage and short selling).
  - Risk Target: 0.12.
  - Sharpe Ratio: 0.24.
  - Expected number of trades per year: 5.4.
  - Stop loss fraction: 0.5.
  - Percentage of capital at risk per trade: 6%.
  - Simple compounding.
    - Profits are added to capital at risk.
    - Losses are subtracted from capital at risk.
    - Some alternatives:
      - Half compounding.
      - “Three quarter” compounding.
  - Expected value of scaled absolute signal is 1 with caps \[-2, 2\].

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
if (!require(devtools)) install.packages('devtools') 
devtools::install_github("mahovo/rsystrade", dependencies = TRUE, build_vignettes = TRUE)
```

## Example

``` r
library(rsystrade)
## basic example code
```
