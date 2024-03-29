% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formulas-risk.R
\name{f_signal_cor_mat}
\alias{f_signal_cor_mat}
\title{Signals Correlation Matrix}
\usage{
f_signal_cor_mat(signals)
}
\arguments{
\item{signals}{Dataframe where each column is a signal vector}
}
\value{
Correlation matrix
}
\description{
Calculates the correlation matrix of signal vectors. Typically calculates the
correlation matrix for a subsystem of all the signal vectors affecting a
single instrument in the system.
}
