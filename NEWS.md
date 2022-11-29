# rsystrade v0.0.0.9001

+ Implements the *Starter System* from Robert Carver's book *Leveraged Trading*. This is intended as a kind of Minimal Viable Product.

+ Only takes data from file. No interaction with broker API.
+ Assume:
	+ Use simulated data
	+ Single product (shares)
	+ Single rule: Moving Average Crossover (16, 64)
	+ Units: Rounded to a whole number of shares. Minimum trade is 1 share.
	+ Invest all available capital
	+ No position adjustment: Fixed position size for the duration of the trade.
	+ No costs, no commissions
	+ FX rate is 1
	+ *Minimum notional exposure* is 1
	+ No rolling (no back-adjustment needed)
	+ No *margining limits*
	+ Borrowing cost is 0 (for leverage and short selling)
	+ Risk Target: 0.12
	+ Sharpe Ratio: 0.24
	+ Expected number of trades per year: 5.4
	+ Stop loss fraction: 0.5
	+ Percentage of capital at risk per trade: 6%
	+ Simple compounding
		+ Profits are added to capital at risk
		+ Losses are substracted from capital at risk
		+ Some alternatives:
			+ Half compounding
			+ "Three quarter" compounding
