\name{data.stata}
\alias{data.stata}
\title{Generate data.frame with data for Stata}
\usage{
data.stata(valid, missing, missing_codes = NULL)
}
\arguments{
  \item{valid}{Vector of valid data (NA, where missing).}

  \item{missing}{Vector of missing data (NA, where valid).}

  \item{missing_codes}{Vector of additional missing codes.}
}
\description{
Next to the data and missings there is a parameter
missing_codes, that allows to turn valid cases into
missings.
}
\details{
The vector with the valid data and the one with the missing
data must have the same length. This means that vor every
case either the valid or the missing or both attributes
must be NA.
}

