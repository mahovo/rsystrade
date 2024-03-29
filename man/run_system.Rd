% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sys_frwk.R
\name{run_system}
\alias{run_system}
\title{Run Trade System}
\usage{
run_system(
  trade_system,
  min_periods = 1,
  mode = "sim",
  instrument_data_folder_path = "~/git/rsystrade/misc/temp/data/"
)
}
\arguments{
\item{min_periods}{Minimum number of observations before first position is
calculated.}

\item{mode}{"live" for live trading or "sim" for simulation mode.}

\item{instrument_data_folder_path}{Path of instrument data folder.}

\item{system}{List containing a full trade system.}
}
\value{
List containing full updated trade system
}
\description{
Run Trade System
}
