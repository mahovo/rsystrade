
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

- This version should install from GitGub.
- Only `sim` mode is supported.
- Produces all the expected tables with rudimentary functionality.
- All tables are built row by row each time the system is run. I am
  sticking with this structure for now, as I constantly want to check
  all intermediate calculations in this stage of development. Having all
  the tables at hand also makes it easy to analyse the system with all
  the usual data frame based tools (`tidyverse`, etc). I have ideas for
  possible later refactoring, which I will explore when performance
  issues compell me.
- Only takes data from CSV file. No interaction with broker API. No
  database.
- Assume:
  - Use simulated data.
  - Multiple rules (simple MAC provided).
  - Multiple products (simple shares, no dividends, splits, etc).
  - Single asset class.
  - Units: Rounded to a whole number of shares. Minimum trade is 1
    share.
  - Position adjustment is very primitive. Much too busy.
  - No costs, no commissions.
  - *Minimum notional exposure* is 1.
  - FX rate is 1.
  - No rolling (no back-adjustment needed).
  - No *margining limits*.
  - Borrowing cost is 0 (for leverage and short selling).
  - Risk Target: 0.12.
  - Sharpe Ratio: 0.24.
  - Expected number of trades per year is set to 5.4. (But the current
    implementation will adjust position sizes more or less daily.)
  - Stop loss fraction: 0.5. (But stop loss is commented out for now.)
  - Percentage of capital at risk per trade: 6%.
  - Simple compounding.
    - Profits are added to capital at risk.
    - Losses are subtracted from capital at risk.
  - Expected value of scaled absolute signal is 1 with caps \[-2, 2\].

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
if (!require(devtools)) install.packages('devtools') 
devtools::install_github("mahovo/rsystrade", dependencies = TRUE, build_vignettes = TRUE)
```

Then get help:

``` r
help(package = "rsystrade")
```

## Example

``` r
library(rsystrade)

n <- 500
min_periods <- 25

mac_20_80 <- function(prices) {
  mac_rule(
    prices,
    n_fast = 20L,
    n_slow = 80L
  )
}
mac_50_200 <- function(prices) {
  mac_rule(
    prices,
    n_fast = 50L,
    n_slow = 200L
  )
}

times <- as.timeDate(seq(from = as.Date("2000-01-01"), by = "day", length.out = n))
df1 <- data.frame(
  time = times,
  price = round(
    c(100, 100 * cumprod(1 + rnorm(n - 1, 0, 0.1/16))),
    2
  )
)
names(df1) <- c("time", "price")
names(df1)
df2 <- data.frame(
  time = times,
  price = round(
    c(100, 100 * cumprod(1 + rnorm(n -1, 0, 0.1/16))),
    2
  )
)

names(df2) <- c("time", "price")
names(df2)
write.csv(df1, "/rsystrade_example/path/to/data/testdata1.csv") ## Replace path!
write.csv(df2, "/rsystrade_example/path/to/data/testdata2.csv") ## Replace path!

algos <- list(
  list(
    instruments = list("testdata1"),
    rules = list(
      list("mac_20_80"),
      list("mac_50_200")
    )
  ),
  list(
    instruments = list("testdata2"),
    rules = list(
      c("mac_20_80"),
      c("mac_50_200")
    )
  )
)

my_system <- make_system(
  algos = algos,
  init_capital = 1000000,
  system_risk_target = 0.12,
  risk_window_length = 25,
  stop_loss_fraction = 0.5,
  min_periods = min_periods,
  mode = "sim",
  instrument_data_folder_path = "/rsystrade_example/path/to/data/"
)

run_system(
  my_system,
  min_periods = min_periods,
  mode = "sim",
  instrument_data_folder_path = "/rsystrade_example/path/to/data/"
)
```
