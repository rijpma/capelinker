% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{xgbm_ff}
\alias{xgbm_ff}
\title{xgb.DMatrix from dataframe and formula}
\usage{
xgbm_ff(dat, f, labelled = TRUE)
}
\arguments{
\item{dat}{a data.frame}

\item{f}{a formula. It must have a response term.}

\item{labelled}{whether the data.frame contains the}
}
\description{
\code{xgm_ff} creates a xgb::xgb.DMatrix from a data.frame and formula
}
