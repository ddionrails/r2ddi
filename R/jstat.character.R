#' Calculate frequencies for character objects
#'
#' @param variable variable-object
jstat.character <- function(variable, time=NULL)
{
  if(is.null(time))
    tab <- table(variable$data_table$valid)
  else
    tab <- table(variable$data_table$valid, time)
  list(table = toJSON(tab))
}

