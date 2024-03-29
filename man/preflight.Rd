% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep.R
\name{preflight}
\alias{preflight}
\title{Check whether dataset is ready for linkage}
\usage{
preflight(
  dat,
  vrbs = NULL,
  modstring = c("m_boost_stel_rein", "m_rf_baptisms_sparse", "m_rf_baptisms_full")
)
}
\arguments{
\item{dat}{the dataset to check, as a data.table.}

\item{vrbs}{a vector of the names of variables to check. Defaults to NULL,
in which case variable names are extracted from the model specified with
modstring}

\item{modstring}{the name of the pretrained model to check against}
}
\value{
Text to the console showing the results of the tests.
}
\description{
\code{preflight} checks whether a dataset is ready for linkage
}
\details{
Preflight does a number of checks on the dataset.
\itemize{
     \item The type of the variables
     \item The share of missing observations by variable, and whether they
         are coded as missing values or empty strings
     \item The share of length one observations by variable (these might break linkage)
     \item The share of case (lower, upper, title) by variable. \code{capelinker} 
         currently does not automatically convert to a single case, but leaves this 
         choice to the user. Different cases do count for the string distance.
     \item The share of accented letters and non-alphabetic symbols by variable. A
         gain, these are not automatically fixed, but left to the user's discretion. 
         Note that some analphabetics (for instance "." or "-") could be frequently 
         and consistently applied in the data. String distance calculation does not 
         fail on accented letters, but it does count towards the string distance.
     \item The set of unique characters in each of the variables.
     \item Whether the variables required for one of the pretrained models
         in capelinker are present and correctly named. }

Capelinker also checks the dataset for the requirements of a number of pretrainined models in the capelinker package. The following models are available: 
\itemize{
 \item \code{m_boost_stel_rein} a model linking households from one year to the next in the opgaafrollen (default).
 \item \code{m_rf_baptisms_sparse} a model linking parents in baptism records to marriage records, based on minimal information: male surname (mlast), male first name (mfirst), female first name (wfirst, female surname not used because it would typically not be reported in the baptism records), and year of marriage/baptism (year).
 \item \code{m_rf_baptisms_full} a model linking parents in baptism and marriage records, using additional information: initials, profession, and soundex distances of the names. Performance is not much better than the sparse model.
}

The following variables are expected by all these models:
\itemize{
     \item \code{mlast} the male surname.
     \item \code{mfirst} the male first name.
     \item \code{wlast} the female last name.
     \item \code{wfist}the female first name.
     \item \code{minitils} male initials in the form JF (so no K, and no punctuation)
     \item \code{winitils} female initials in the form JF (so no K, and no punctuation)
     \item \code{year} the year of observation of the two records
}

The baptism record models also expect and check:
\itemize{
 \item \code{mprof} the male profession
}

The opgaafrollen model also expects and checks:
\itemize{
 \item \code{settlerchildren}
}
}
\examples{
d2 = data.table::data.table(mlast = c("jongh", "Jong", "smit (Smid)"), persid = c(1:3))
preflight(d2)

}
