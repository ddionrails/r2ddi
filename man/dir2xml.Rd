\name{dir2xml}
\alias{dir2xml}
\title{Import multiple files from one directory}
\usage{
dir2xml(path_in, path_out, file_type = "dta", multicore = TRUE,
  missing_codes = NULL, my_cores = 1)
}
\arguments{
  \item{path_in}{Path to files}

  \item{file_type}{Accepted filetypes ("dta", "sav", "csv",
  or "all" for the for the previous three)}

  \item{multicore}{Use multicore functionallity}
}
\description{
Import multiple files from one directory
}

