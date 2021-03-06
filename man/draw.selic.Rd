% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw.selic.R
\name{draw.selic}
\alias{draw.selic}
\title{Create a chart of the Base Interest Rate (SELIC) time series}
\usage{
draw.selic(file, start = NULL, ylim = NULL, open = TRUE)
}
\arguments{
\item{file}{A \code{character}. The name of the file in which the plot must be printed. The extension can be either '.png' or '.pdf'. All charts are stored in the 'graphs' folder, under the BETS installation directory.}

\item{start}{A \code{character}. The starting period of the series.}

\item{ylim}{A \code{numeric vector}. Y axis limits.}

\item{open}{A \code{boolean}. Indicates whether the plot must be opened after being created.}
}
\value{
An image file is saved in the 'graphs' folder, under the BETS installation directory.
}
\description{
Creates a plot of series 4189
}
