#

missings.stata <- function(data, missings, missing.codes=NULL)
{

  if(length(data) != length(missings))
    missings <- rep(NA, length(data))

  if(is.null(missing.codes))
    return(missings)

  missings <-
    ifelse(
      data %in% missing.codes & data != NA,
      var,
      missings)

  return(missings)
}


