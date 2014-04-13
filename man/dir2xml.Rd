\name{dir2xml}
\alias{dir2xml}
\title{Import multiple files from one directory}
\usage{
dir2xml(path_in, path_out, file_type = "dta", multicore = TRUE,
  missing_codes = NULL, my_cores = 2)
}
\arguments{
  \item{path_in}{Path with Stata files (input).}

  \item{path_out}{Path for XML files (output).}

  \item{file_type}{Currently, only "dta" for Stata files is
  supported.}

  \item{multicore}{Use multicore functionallity}

  \item{missing_codes}{Define vector with study-wide
  missing codes, e.g. -9:-1 for the SOEP.}

  \item{my_cores}{Set number of cores for multi-core
  processing, default is 2 cores.}
}
\description{
Import multiple files from one directory
}

