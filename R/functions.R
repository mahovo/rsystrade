
## Notes
## 1)
## Decimal fractions are used instead of percentages throughout.
## Example: 87 percent is expressed as 0.87.
##
## 2)
## "LT" refers to Robert Carver: Leveraged Trading
## "ST" refers to Robert Carver: Systematic Trading
## "F" for "formula".


## Basic calculations ====

## Leveraged growth
## L, leverage factor
## r, return rate on investment
## b, borrowing rate
## V, value (V_t, value at time t)

##LT F4.1a

#' Title
#'
#' @param L
#' @param r
#' @param b
#'
#' @return
#' @export
#'
#' @examples
growth_rate_lvrg <- function(L, r, b) {L * r - (L - 1) * b}


## LT F4.1b
#' Title
#'
#' @param V
#' @param L
#' @param r
#' @param b
#'
#' @return
#' @export
#'
#' @examples
growth_lvrg <- function(V, L, r, b) {(1 + (L * r - (L - 1) * b)) * V}


## LT F4.1c
#' Title
#'
#' @param V
#' @param L
#' @param r
#' @param b
#'
#' @return
#' @export
#'
#' @examples
profit_lvrg <- function(V, L, r, b) {(L * r - (L - 1) * b) * V}

#'Generate net returns vector from price vector
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom: Newer to older.
returns_from_prices <- function(prices) {
  N <- length(prices)
  (head(prices, N - 1) / tail(prices, N - 1)) - 1
}

#' Generate price vector from returns vector
#'
#' @param returns
#' @param initial_price
prices_from_returns <- function(returns, initial_price) {
  n <- length(returns) + 1 ## Length of price vector
  cumprod(c(initial_price, returns[2:(n - 1)] + 1))
}

#' Simulate returns
# sim_returns <- function(n) {
#
# }




## Alpha ====
#' Moving Average
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom: Newer to older.
#' @param n Moving Average period.
#'
#' Returns a number.
#' Calculates average of n items prior to time t in price vector.
#'
#' Moving average
#' LT F12
#'
#' @param prices Vector.
#' @param n Window length.
#' @param t
#'
#' N (length of price vector) smaller than or equal to n is accepted.
#' This will not be the desired moving average, but will also not fail.
moving_average <- function(prices, n, t = NA) {
  N <- length(prices)
  if(is.na(t)) {t = N} ## Set t to last item if no t is provided

  #stopifnot(N >= n)
  ma <- NA
  if(t <= n) { ## Handle t less than or equal to n
    ma <- mean(prices[1:t])
  } else {
    ma <- sum(prices[(t - n + 1):N]) / n
  }
  ma
}


#' Moving Average vector
#'
#' @param prices A vector of prices in currency. Newest first. Top to bottom: Newer to older.
#' @param n Moving Average period.
#'
#' Returns a vector.
#' Calculates moving average for each row in a price series data frame.
#'
#' Moving average
#' LT F12
#'
#' @param prices Vector.
#' @param n Window length.
#'
#' N (length of price vector) smaller than or equal to n is accepted.
#' This will not be the desired moving average, but will also not fail.
moving_average_vector <- function(prices, n) {
  N <- length(prices)

  #stopifnot(N >= n)
  ma_prices <- rep(NA, N)
  for(t in 1:n) { ## Handle t less than or equal to n
    ma_prices[t] <- mean(prices[1:t])
  }
  if(N >= n) {
    for(t in (n + 1):N) {
      ma_prices[t] <- sum(prices[(t - n + 1):t]) / n
    }
  } else {warning("Length of price vector is shorter than or equal to desired MA window length.\n")}
  ma_prices
}


#' Moving Average Crossover
#' LT F13
#'
#' @param ma_fast Fast moving average
#' @param ma_slow Slow moving average
#' #' @param gap
#'
#' Returns TRUE when ma_fast > ma_slow.
#' 1 indicates uptrend ie. go long.
#' -1 indicates downtrend ie. go short.
moving_average_crossover <- function(ma_fast, ma_slow, gap = 0) {
  ## Is the absolute difference bigger than the gap?
  ## And is ma_fast bigger than ma_slow?
  x <- abs(ma_fast - ma_slow) > gap
  y <- ma_fast > ma_slow
  x * (y - (1 - y))
}





## Risk ====

#' Notional exposure
#' LT F14
#'
#' @param risk_target Risk target as decimal fraction
#' @param capital Trading capital in currency
#' @param instrument_risk Instrument risk as decimal fraction
notional_exposure <- function(risk_target, capital, instrument_risk) {
  (risk_target * capital) / instrument_risk
}

#' Decimal fraction of capital at risk per trade for Starter System
#' F15, LT, p. 113
#'
#' @param risk_target Risk target as decimal fraction
#' @param stop_loss_fraction Stop loss fraction
risky_capital_pct <- function(risk_target, stop_loss_fraction) {
  risk_target * stop_loss_fraction
}

#' Instrument risk, annualized
#'
#' @param prices A time series of prices in currency. Newest first.
#' Top to bottom: Newer to older.
#' @param window_length
#' @param t
#'
#' Standard deviations of returns.
instr_risk <- function(prices, t, window_length = NA) {
  if(is.na(window_length)) {window_length = t} ## Include all if window_length is NA
  if(t > window_length) {
      sd_ <- sd(returns_from_prices(prices[(t - window_length + 1):t])) * 15.87451 ## 252 is the number of business days in a year. sqrt(252) = 15.87451
  } else if (t > 1) {
    sd_ <- sd(returns_from_prices(prices[1:t])) * 15.87451
    warning("Window length for instrument risk calculation is bigger than length of price vector.\n")
  } else {
    sd_ <- NA
    warning("instr_risk=NA. Careful! This might break something.\n")
  }
  sd_
}

#' Minimum exposure
#'
#' @param min_exposure Notional exposure of the minimum trade
#' @param instr_risk Instrument risk as decimal fraction
#' @param target_risk Target risk as decimal fraction
min_exposure <- function(min_exposure, instr_risk, target_risk) {
  (min_exposure * instr_risk) / target_risk
}

#' Minimum capital
#' LT F21
#'
#' @param min_exposure
#' @param instr_risk
#' @param target_risk
minimum_capital <- function(min_exposure, instr_risk, target_risk) {
  (min_exposure * instr_risk) / target_risk
}

#' Price unit volatility (instrument risk in price units)
#'
#' @param instr_risk
#' @param price
#'
#' LT F22
price_unit_vol <- function(instr_risk, price) {
  instr_risk * price
}

#' Stop loss gap
#'
#' @param price_unit_vol
#' @param stop_loss_fraction
#'
#' LT F23
stop_loss_gap <- function(price_unit_vol, stop_loss_fraction) {
  price_unit_vol * stop_loss_fraction
}

#' High water mark
#'
#' @param prices
#' @param t
#' @param t_trade_open
#'
#' LT F24
hwm <- function (prices, t, t_trade_open) {
  max(prices[t_trade_open:t])
}

#' Low water mark
#'
#' @param prices
#' @param t
#' @param t_trade_open
#'
#' LT F24
lwm <- function (prices, t, t_trade_open) {
  min(prices[t_trade_open:t])
}

#' Stop loss level
#' LT F24
#'
#' @param hwm High Water Mark
#' @param lwm Low  Water Mark
#' @param stop_loss_gap Stop loss gap
#' @param direction Is current trade long or short? 1 for long, -1 for short.
#' @param rnd If TRUE, add small random amount to stop loss level. Negative if short.
stop_loss_level <- function(hwm, lwm, stop_loss_gap, direction = 0, rnd = TRUE) {
  if(direction == 1){ ## If long
      hwm - stop_loss_gap + runif(1, 0.01, 0.03) * rnd
    } else {lwm + stop_loss_gap - runif(1, 0.01, 0.03) * rnd}
}

## Cost ====



## Buble indicator ====



## Data ====



## Research ====



## Portefolio ==== '



## Execution ====

#' Execute trades for simulation
#'
#' @param n_fast n for fast moving average
#' @param n_slow n for slow moving average
#' @param prices
#' @param init_capital
#' @param risk_target
#' @param risk_window_length
#' @param stop_loss_fraction
#' Generates a dataframe of simulated trades from time series of prices.
trade_sim <- function(prices, init_capital = 100000, n_fast = 16, n_slow = 64, risk_target = 0.12, risk_window_length = 25, stop_loss_fraction = 0.5) {

  stopifnot(nrow(prices) > n_slow + 1)
  stopifnot(n_slow > n_fast)


  ## We need to skip AT LEAST two days because:
  ## sd needs to data points as input.
  ## To get two returns, we need three prices.
  ##
  ## However, we should definitely make sure we have enough data for a full
  ## ma_slow window and instrument_risk.
  n_leadin <- max(n_slow, risk_window_length)
  trades_data <- data.frame(
    date = prices$date[1:n_leadin],
    price = prices$price[1:n_leadin],
    open_close = rep(NA, n_leadin),
    trade_on = rep(FALSE, n_leadin),
    direction = rep(0, n_leadin),
    instrument_risk = rep(NA, n_leadin),
    leverage_factor = rep(0, n_leadin),
    notional_exposure = rep(0, n_leadin),
    position_size_units = rep(0, n_leadin),
    position_size_ccy = rep(0, n_leadin),
    stop_loss_gap = rep(0,  n_leadin),
    close_trade_stop_loss = rep(0, n_leadin),
    ma_fast = prices$price[1:n_leadin],
    ma_slow = prices$price[1:n_leadin],
    mac = rep(FALSE, n_leadin)
  )

  accounts_data <- data.frame(
    date = prices$date[1:n_leadin],
    price = prices$price[1:n_leadin],
    pnl = rep(0, n_leadin),
    account_value = rep(init_capital, n_leadin),
    capital = rep(init_capital, n_leadin),
    cash = rep(init_capital, n_leadin), ## Uninvested money
    borrowed_cash = rep(0, n_leadin),
    borrowed_asset = rep(0, n_leadin)
  )


  # trade_on = FALSE ## Is there currently a trad on? TRUE or FALSE.

  trade_on = FALSE
  t_trade_open = 1 ## Set to 1 eventhough no trade is entered at t = 1
  direction <- 0
  capital = init_capital

  cat("Calculating...\n")

  ## We are starting at day three to be able to calculate sd for returns...
  for(t in (n_slow + 1):nrow(prices)) {
    open_close <- "---"

    price <- prices$price[t]

    # latest_trade_direction = 1 ## Is the latest opened trade long (1) or short (0)?
    latest_trade_direction <- trades_data$direction[t_trade_open]

    instrument_risk_ <- instr_risk(c(trades_data$price[1:(t - 1)], price), t = t, window_length = risk_window_length)
    leverage_factor <- risk_target/instrument_risk_
    notional_exposure_ <- notional_exposure(risk_target, capital, instrument_risk_)

    ma_fast <- moving_average(c(trades_data$price[1:(t - 1)], price), n_fast)
    ma_slow <- moving_average(c(trades_data$price[1:(t - 1)], price), n_slow)
    ## NOTE:
    ## --- TODO ---
    ## What's up with c(trades_data$price[1:(t - 1)], price), you ask...
    ## This is a posh way of writing trades_data$price[1:t].
    ## I am trying to qvoid creating a whole new instance of trades_data.
    ## Check if this is actually necessary!

    ## mac == 1 indicates long, mac == -1 indicates short
    ## mac == 0 indicates on signal.
    mac <- moving_average_crossover(ma_fast, ma_slow)

    # cat("ma_fast =", ma_fast, "\n")
    # cat("ma_slow =", ma_slow, "\n")
    # cat("mac =", mac, "\n")
    # cat("latest_trade_direction =", latest_trade_direction, "\n")
    # cat("instrument_risk =", instrument_risk_, "\n")
    # cat("notional_exposure =", notional_exposure_, "\n")

    #direction <- open_trade_mac(moving_average_crossover = mac, latest_trade_direction = latest_trade_direction) ## 0 if no change
    #direction <- mac
    # cat("direction =", direction, "\n")

    price_unit_vol_ <- price_unit_vol(instrument_risk_, price)
    stop_loss_gap_ <- stop_loss_gap(price_unit_vol_, stop_loss_fraction)

    if(trade_on == FALSE) {
      #if(latest_trade_direction == direction) {direction = 0}
      direction <- mac
      ## Close position if stop loss level was breached yesterday [LT, p. 138].
      ## Close position even if price has recovered. [LT, p. 141]
      close_trade_stop_loss_ <- close_trade_stop_loss(prices = trades_data$price, t = t - 1, t_trade_open = t_trade_open, stop_loss_gap = stop_loss_gap_, direction = direction, rnd = FALSE)

      if(direction != 0) {
        ## It is not allowed to enter a trade in the same direction as the previous trade.
        if(latest_trade_direction != direction) {
          position_size_units_ <- position_size_units(price, risk_target, accounts_data$capital[t - 1], instrument_risk_)
          position_size_ccy <- position_size_units_ * price

          ## Starter System: No position adjustment.
          #prev_position_size_units <- trades_data$position_size_units[t - 1]
          #prev_position_size_ccy <- trades_data$position_size_ccy[t - 1]

          #trade_amount_units <- (position_size_units_ - prev_position_size_units) * direction
          #trade_amount_ccy <- (position_size_ccy - prev_position_size_ccy) * direction

          trade_on <- TRUE
          open_close <- "OPEN"
          t_trade_open <- t
          latest_trade_direction <- direction

          ## Simple System:
          ## When opening a trade, amount of cash and capital should always be the same,
          ## As we only trade one instrument, and always close the trade before opening a new.
          ## Only list amount borrowed.
          ## Don't list negative amount when leverage is <1, ie. position_size_ccy[t] < cash[t - 1].

          ## When long trade has just been opened:
          ## cash[t] = cash[t - 1] - position_size_ccy[t] + borrowed_cash[t]
          ##
          #### borrowed_cash[t] = max[0, position_size_ccy[t] - cash[t - 1]]
          #### borrowed_asset[t] = 0
          #### capital[t] = cash[t] + position_size_ccy[t] - borrowed_cash[t]

          ## When short trade has just been opened:
          ## cash[t] = cash[t - 1] + borrowed_asset[t]
          ## borrowed_cash[t] = 0
          ## borrowed_asset[t] = position_size_ccy[t]
          ## capital[t] = cash[t - 1]

          borrowed_cash <- max(0, position_size_ccy - accounts_data$cash[t - 1])
          borrowed_asset <- position_size_ccy * (direction < 0) ## 0 if long

          cash <- accounts_data$cash[t - 1] - ((direction > 0) * position_size_ccy) + borrowed_cash + borrowed_asset
          if(direction > 0) {
            capital <- cash + position_size_ccy - borrowed_cash
          } else {
            capital <- accounts_data$cash[t - 1]
          }
          account_value <- capital # <------ TODO
        } else { ## latest_trade_direction == direction: no change (don't enter trade)
          position_size_units_ <- trades_data$position_size_units[t - 1] ## Still 0
          position_size_ccy <- trades_data$position_size_ccy[t - 1] ## Still 0

          borrowed_cash <- 0 ## No trade is on
          borrowed_asset <- 0

          ## While no trade on:
          ## capital[t] = cash[t]

          cash <- accounts_data$cash[t - 1]
          capital <- accounts_data$capital[t - 1]
          account_value <- capital # <------ TODO
        }
      } else { ## direction == 0: no position change (don't enter trade)
        ## We don't have any trade on, and nothing changes
        position_size_units_ <- trades_data$position_size_units[t - 1] ## Still 0
        position_size_ccy <- trades_data$position_size_ccy[t - 1] ## Still 0

        ## How much did we borrow when we opened the trade?
        borrowed_cash <- accounts_data$borrowed_cash[t_trade_open]
        borrowed_asset <- accounts_data$borrowed_asset[t_trade_open]

        cash <- accounts_data$cash[t - 1]
        capital <- accounts_data$capital[t - 1]
        account_value <- capital # <------ TODO
      }
    } else { ## If trade is on, check stop loss
      ## Starter System: stop_loss_gap should be fixed for duration of trade [LT p. 138]
      ## Instead I use a new calculation of price volatility (instrument risk) each day,
      ## since I already calculated it above.

      #instr_risk_at_entry <- trade_data$instrument_risk[t_trade_open]
      #price_at_entry <- trade_data$price[t_trade_open]
      #price_vol_at_entry <- price_unit_vol(instr_risk_at_entry, price_at_entry)

      ## Close position if stop loss level was breached yesterday [LT, p. 138].
      ## Close position even if price has recovered. [LT, p. 141]
      close_trade_stop_loss_ <- close_trade_stop_loss(prices = trades_data$price, t = t - 1, t_trade_open = t_trade_open, stop_loss_gap = stop_loss_gap_, direction = direction, rnd = FALSE)

      ## NOTE:
      ## position_size_units_ will never be negative, as long as capital is not allowed
      ## to reach 0 or below.
      ## The break below takes care of that.

      ## While long trade is on:
      ## cash[t] = cash[t - 1]
      ##
      #### borrowed_cash[t] = borrowed_cash[t_trade_open]
      #### borrowed_asset[t] = 0
      #### capital[t] = position_size_ccy[t] - borrowed_cash[t_trade_open] + cash

      ## While short trade is on:
      ## cash[t] = cash[t - 1]
      ## borrowed_cash[t] = 0
      ## borrowed_asset[t] = position_size_ccy[t]
      ## capital[t] = cash[t_trade_open - 1] + (position_size_ccy[t] - position_size_ccy[t_trade_open])

      ## When long trade has just been closed:
      ## cash[t] = cash[t - 1] + position_size_ccy[t] - borrowed_cash[t_trade_open]
      ## borrowed_cash[t] = 0
      ## borrowed_asset[t] = 0
      ## capital[t] = cash[t]

      ## When short trade has just been closed:
      ## cash[t] = cash[t - 1] - position_size_ccy[t]
      ## borrowed_cash[t] = 0
      ## borrowed_asset[t] = 0
      ## capital[t] = cash[t]

      position_size_units_ <- trades_data$position_size_units[t - 1]
      position_size_ccy <- position_size_units_ * price

      borrowed_cash = accounts_data$borrowed_cash[t_trade_open] * (direction > 0) * trade_on ## 0 if short
      borrowed_asset = position_size_ccy * (1 - (direction > 0)) * trade_on ## 0 if long


      if(direction == 1) { ## If long
        cash <- accounts_data$cash[t - 1]
        capital <- cash + position_size_ccy - accounts_data$borrowed_cash[t_trade_open]
        if(price < close_trade_stop_loss_) {
          trade_on <- FALSE ## Close trade
          open_close <- "CLOSE"
          direction <- 0
          cash <- accounts_data$cash[t - 1] + position_size_ccy - accounts_data$borrowed_cash[t_trade_open]
          capital <- cash
        }
      } else if(direction == -1) { ## If short
        cash <- accounts_data$cash[t - 1]
        capital <- accounts_data$cash[t_trade_open - 1] + (trades_data$position_size_ccy[t_trade_open] - position_size_ccy)
        if(price > close_trade_stop_loss_) {
          trade_on <- FALSE ## Close trade
          open_close <- "CLOSE"
          direction <- 0
          cash <- accounts_data$cash[t - 1] - position_size_ccy
          capital <- cash
        }
      } else {
        cash <- accounts_data$cash[t - 1]
        capital <- accounts_data$capital[t - 1]
      }
      account_value <- capital # <------ TODO

      ## Starter system: Position sizes are fixed for the duration of the trade.
      ## So no position adjustments.
      ## If trade was closed, position will be 0:
      position_size_units_ <- trade_on * trades_data$position_size_units[t - 1]
      position_size_ccy <- trade_on * position_size_units_ * price

      ## 1 if direction = 1 and trade_on = FALSE, long trade closed
      ## 0 if direction = 1 and trade_on = TRUE, long trade still open
      ## -1 if direction = -1 and trade_on = FALSE, short trade closed
      ## 0 if direction = -1 and trade_on = TRUE, short trade still open
      #position_sign <- (direction - (direction * trade_on))

      #borrowed <- accounts_data$borrowed[t_trade_open] * trade_on

      #cash <- accounts_data$cash[t - 1] + position_sign * position_size_ccy
      #capital <- cash + position_sign * (position_size_ccy - borrowed)
      #account_value <- capital

    }

    pnl = capital - init_capital

    # cat("price =", price, "\n")
    # cat("date =", prices$date[t], "\n")
    # cat("ma_fast =", ma_fast, "\n")
    # cat("ma_slow =", ma_slow, "\n")
    # cat("mac =", mac, "\n")
    # cat("instrument_risk =", instrument_risk_, "\n")
    # cat("notional_exposure =", notional_exposure_, "\n")
    # cat("position_size_units =", position_size_units_, "\n")
    # cat("position_size_ccy =", position_size_ccy, "\n")
    # cat("direction =", direction, "\n")
    # cat("stop_loss_gap =", stop_loss_gap_, "\n")
    # cat("close_trade_stop_loss =", close_trade_stop_loss_, "\n")
    # cat("trade_on =", trade_on, "\n")

    trades_data[t, ] <- list(
      date = prices$date[t],
      price = price,
      open_close = open_close,
      trade_on = trade_on,
      direction = direction,
      instrument_risk = instrument_risk_,
      leverage_factor = leverage_factor,
      notional_exposure = notional_exposure_,
      position_size_units = position_size_units_,
      position_size_ccy = position_size_ccy,
      stop_loss_gap = stop_loss_gap_,
      close_trade_stop_loss = close_trade_stop_loss_,
      ma_fast = ma_fast,
      ma_slow = ma_slow,
      mac = mac
    )

    accounts_data[t, ] <- list(
      date = prices$date[t],
      price = price,
      pnl = pnl,
      account_value = account_value,
      capital = capital,
      cash = cash, ## Uninvested money
      borrowed_cash = borrowed_cash,
      borrowed_asset = borrowed_asset
    )

    if(capital <= 0) {
      cat("BOOM! You are broke! Capital is 0 - or worse!\n")
      cat("Your account crashed at", as.character(trades_data$date[t]))
      break
    }

  }

  # cat("Note: Window length warnings are normal:\n")
  # cat("For t =< n, moving_average will be mean of t first prices.\n")
  # cat("So for t =< n_fast, ma_fast = ma_slow, while both move gradually towards their \n")
  # cat("proper window lengths.\n")
  # cat("For n_fast < t < n_slow, ma_slow will still be moving towards its proper window\n")
  # cat("length, while n_fast will be at its proper window length.\n")
  # cat("For t > n_slow, ma_slow will also be at its proper window length.\n")
  cat("\n")
  cat("Done!\n")

  list(trades_data, accounts_data) ## Return list of data frames
}

#' Open trade for Moving Average Crossover
#'
#' @param moving_average_crossover 1 for long, 0 for short.
#' @param latest_trade_direction 1 for long, 0 for short.
#' Returns 1 for going long and -1 for going short.
#' Returns 0 for no change.
#'
#' 1 if latest_trade_direction = -1 and moving_average_crossover = TRUE
#' 1 if latest_trade_direction = 0 and moving_average_crossover = TRUE
#' -1 if latest_trade_direction = 1 and moving_average_crossover = FALSE
#' -1 if latest_trade_direction = 0 and moving_average_crossover = FALSE
#' 0 if latest_trade_direction = 1 and moving_average_crossover = TRUE
#' 0 if latest_trade_direction = -1 and moving_average_crossover = FALSE
#'
#' No trade will ever open when direction = 0, so latest_trade_direction can only
#' ever be 0 before the first trade.
#'
#' moving_average_crossover:
#' TRUE indicates uptrend ie. go long.
#' FALSE indicates downtrend ie. go short.
# open_trade_mac <- function(moving_average_crossover, latest_trade_direction = 0) {
##  --- TODO ---
#   next_trade_direction <-
#   next_trade_direction
# }


#' Close trade
#'
#' @param prices
#' @param t
#' @param t_trade_open
#' @param stop_loss_gap
#' @param rnd
#' @param direction Is current trade long or short? 1 for long, -1 for short.
close_trade_stop_loss <- function(prices, t, t_trade_open, stop_loss_gap, direction = 0, rnd = FALSE) {
  stop_loss_level(hwm(prices, t, t_trade_open), lwm(prices, t, t_trade_open), stop_loss_gap, direction, rnd)
}


#' Position size in units
#'
#' @param price
#' @param risk_target
#' @param capital
#' @param instrument_risk
position_size_units <- function(price, risk_target, capital, instrument_risk) {
  floor(notional_exposure(risk_target, capital, instrument_risk) / price)
}



## Accounting ====

#' Account value
# account_value <- function() {
#   # <------ TODO
# }

#' Cash
#' --- DON'T USE ---
#'
#' @param prev_capital Value of capital at t - 1.
#' @param trade_amount Amount of account currency for current trade.
#' @param open_trade Boolean. A trade is being opened at time t.
#' @param open_trade Boolean. A trade is being closed at time t.
# cash <- function(prev_cash, trad_amount, open_trade, close_trade) {
#   # <------ TODO
#   # See https://qoppac.blogspot.com/2016/06/capital-correction-pysystemtrade.html
#   if(open_trade) {
#     cash <- prev_cash - trad_amount
#   } else if(close_trade) {
#     cash <- prev_cash + trad_amount
#   } else {cash <- prev_cash}
#
#   cash
# }


#' Capital
#'
# capital <- function() {
#   # <------ TODO
# }


#' Profit and loss
# pnl <- function() {
#   # <------ TODO
#   new_broker_account_value - prev_broker_account_value
# }

#' Accumulated profit and loss
# cum_pnl <- function() {
#   # <------ TODO
# }



## Monitoring ====




## PANIC ====



## Dead Man Switch ====
## Liquidate portefolio if Dead Man Switch is not activated on time.



## Starter System ====
## Ch 6: Trading the starter system







