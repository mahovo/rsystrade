## Execution in simulation mode ====

#' Execute trades in simulation mode
#'
#' Generates a dataframe of simulated trades from time series of prices.
#'
#' @param n_fast n for fast moving average.
#' @param n_slow n for slow moving average.
#' @param prices A vector of prices in currency. Newest first. Top to bottom:
#'   Newer to older.
#' @param init_capital Initial capital.
#' @param risk_target Risk target.
#' @param risk_window_length Risk window length.
#' @param stop_loss_fraction Stop loss fraction.
#'
#' @returns A list of trades data and accounts data.
#' @export
#'
#' @examples
#'
trade_sim <- function(
    prices,
    init_capital = 100000,
    n_fast = 16,
    n_slow = 64,
    risk_target = 0.12,
    risk_window_length = 25,
    stop_loss_fraction = 0.5) {

  stopifnot(nrow(prices) > n_slow + 1)
  stopifnot(n_slow > n_fast)


  ## We need to skip AT LEAST two days because:
  ## sd needs two data points as input.
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

  cat("Updating system...\n")

  ## We are starting at day three to be able to calculate sd for returns...
  for(t in (n_slow + 1):nrow(prices)) {
    open_close <- "---"

    price <- prices$price[t]

    # latest_trade_direction = 1 ## Is the latest opened trade long (1) or short
                                 ## (0)?
    latest_trade_direction <- trades_data$direction[t_trade_open]

    instrument_risk_ <- instr_risk(
      c(trades_data$price[1:(t - 1)], price),
      t = t,
      window_length = risk_window_length)
    leverage_factor <- risk_target/instrument_risk_
    notional_exposure_ <- notional_exposure(
      risk_target,
      capital,
      instrument_risk_
    )

    ma_fast <- moving_average(
      c(trades_data$price[1:(t - 1)], price),
      n_fast
    )
    ma_slow <- moving_average(
      c(trades_data$price[1:(t - 1)], price),
      n_slow
    )
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

    #direction <- open_trade_mac(
    #  moving_average_crossover = mac,
    #  latest_trade_direction = latest_trade_direction
    #) ## 0 if no change
    #direction <- mac
    #cat("direction =", direction, "\n")

    price_unit_vol_ <- price_unit_vol(instrument_risk_, price)
    stop_loss_gap_ <- stop_loss_gap(price_unit_vol_, stop_loss_fraction)

    if(trade_on == FALSE) {
      #if(latest_trade_direction == direction) {direction = 0}
      direction <- mac
      ## Close position if stop loss level was breached yesterday [LT, p. 138].
      ## Close position even if price has recovered. [LT, p. 141]
      close_trade_stop_loss_ <- close_trade_stop_loss(
        prices = trades_data$price,
        t = t - 1,
        t_trade_open = t_trade_open,
        stop_loss_gap = stop_loss_gap_,
        direction = direction,
        rnd = FALSE
      )

      if(direction != 0) {
        ## It is not allowed to enter a trade in the same direction as the
        ## previous trade.
        if(latest_trade_direction != direction) {
          position_size_units_ <- position_size_units(
            price,
            risk_target,
            accounts_data$capital[t - 1],
            instrument_risk_
          )
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
          ## When opening a trade, amount of cash and capital should always be
          ## the same,
          ## As we only trade one instrument, and always close the trade before
          ## opening a new.
          ## Only list amount borrowed.
          ## Don't list negative amount when leverage is <1,
          ## ie. position_size_ccy[t] < cash[t - 1].

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
        } else { ## latest_trade_direction == direction: no change (don't enter
                 ## trade)
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
      ## Starter System: stop_loss_gap should be fixed for duration of trade
      ## [LT p. 138]
      ## Instead I use a new calculation of price volatility (instrument risk)
      ## each day,
      ## since I already calculated it above.

      #instr_risk_at_entry <- trade_data$instrument_risk[t_trade_open]
      #price_at_entry <- trade_data$price[t_trade_open]
      #price_vol_at_entry <- price_unit_vol(instr_risk_at_entry, price_at_entry)

      ## Close position if stop loss level was breached yesterday [LT, p. 138].
      ## Close position even if price has recovered. [LT, p. 141]
      close_trade_stop_loss_ <- close_trade_stop_loss(
        prices = trades_data$price,
        t = t - 1,
        t_trade_open = t_trade_open,
        stop_loss_gap = stop_loss_gap_,
        direction = direction,
        rnd = FALSE
      )

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

  #cat("Note: Window length warnings are normal:\n")
  #cat("For t =< n, moving_average will be mean of t first prices.\n")
  #cat("So for t =< n_fast, ma_fast = ma_slow, while both move gradually towards their \n")
  #cat("proper window lengths.\n")
  #cat("For n_fast < t < n_slow, ma_slow will still be moving towards its proper window\n")
  #cat("length, while n_fast will be at its proper window length.\n")
  #cat("For t > n_slow, ma_slow will also be at its proper window length.\n")
  cat("\n")
  cat("Done!\n")

  list(trades_data, accounts_data) ## Return list of data frames
}







