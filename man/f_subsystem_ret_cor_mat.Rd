% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formulas-risk.R
\name{f_subsystem_ret_cor_mat}
\alias{f_subsystem_ret_cor_mat}
\title{Subsystem Returns Correlation Matrix}
\usage{
f_subsystem_ret_cor_mat(subsystem_returns, method = "Pearson", min_cor)
}
\arguments{
\item{subsystem_returns}{Dataframe where each columns is a vector of
subsystem (daily) percentage returns}

\item{method}{Method
\itemize{
\item \code{"Pearson"}, Pearsons Correlation Coefficient
\item \code{ewa}, Exponential Weighted Average Correlation
\deqn{E[X_t | X_{t-1}] = \frac{1}{\sum_{i=0}^{t-2}A^i}\sum_{j=1}^{t-1}A^{j-1} X_{t-j}}
}}
}
\value{
Correlation matrix
}
\description{
Calculates the correlation matrix of vectors of subsystem (daily) percentage
returns. The subsystem returns are the returns produced by the subsystem in
backtesting. Notice, these are not the returns that we get directly from
the price series of the each instrument, but rather the returns we get from
applying the rules of the subsystem to the instrument of the subsystem. A
subsystem consists of one instrument and one or more rules applied to this
instrument.
}
