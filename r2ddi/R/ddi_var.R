#' Generate varDscr
#'
#' @param id Variable name.
#' @param labl Human readable label.
#' @param intrvl Variable format.
#' @param catgry data.frame with categories.
#' @param sum_stat data.frame with summary statistics.
#' @export
ddi_var <- function(id,
                    labl,
                    intrvl   = NULL,
                    catgry   = NULL,
                    sum_stat = NULL)
{
  ddi_var <-
    list(
      id       = id,
      labl     = labl,
      intrvl   = intrvl,
      catgry   = catgry,
      sum_stat = sum_stat)
  class(ddi_var) <- "ddi_var"
  ddi_var
}
