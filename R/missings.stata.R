#' Missings in Stata

missings.stata <- function(data, missings, missing_codes=NULL)
{

  if(length(data) != length(missings))
    missings <- rep(NA, length(data))

  if(is.null(missing_codes))
    return(missings)

  missings <-
    ifelse(
      data %in% missing_codes & data != NA,
      var,
      missings)

  return(missings)
}


