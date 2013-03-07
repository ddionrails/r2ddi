#' Generate data.frame with data for Stata
#'
#' Next to the data and missings there is a parameter
#' missing_codes, that allows to turn valid cases into
#' missings.
#'
#' The vector with the valid data and the one with the
#' missing data must have the same length. This means
#' that vor every case either the valid or the missing
#' or both attributes must be NA.
#'
#' @param valid Vector of valid data (NA, where missing).
#' @param missing Vector of missing data (NA, where valid).
#' @param missing_codes Vector of additional missing codes.

data.stata <-
  function(valid, missing, missing_codes=NULL)
{

  if(length(valid) != length(missing))
    missing <- rep(NA, length(valid))

  if(is.null(missing_codes))
    return(
      data.frame(
        valid            = valid,
        missing          = missing,
        stringsAsFactors = FALSE))

  return(
    data.frame(
      valid            = ifelse(
                           valid %in% missing_codes,
                           NA,
                           valid),
      missing          = ifelse(
                           valid %in% missing_codes & valid != NA,
                           valid,
                           missing),
      stringsAsFactors = FALSE))
}


