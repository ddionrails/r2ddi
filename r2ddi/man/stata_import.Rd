\name{stata_import}
\alias{stata_import}
\title{stata to ddi}
\usage{
  stata_import(filename, data_name, data_label = NULL,
    missing_codes = NULL, keep_data = TRUE,
    multicore = FALSE, is_stata_mis = TRUE)
}
\arguments{
  \item{filename}{Name of a Stata file.}

  \item{data_name}{Name of the dataset.}

  \item{data_label}{Label of the dataset.}

  \item{missing_codes}{Array of values, that should be
  treated as missing values.}

  \item{keep_data}{Defines, if the original data should be
  included in the ddi object.}

  \item{multicore,}{Defines, if you want r2ddi to use R's
  multicore functionallity.}
}
\description{
  Imports a Stata file and transforms it to a ddi object.
}
\examples{

## Import Stata file
test <- stata2ddi("test.dta", "test_data", multicore=TRUE)

## Export ddi object to XML
ddi2xml(test, "text.xml")

}

