#' Generate a ddi code book.
#'
#' @param id Variable name.
#' @param labl Human readable label.
#' @param intrvl Variable format.
#' @param catgry data.frame with categories.
#' @param sum_stat data.frame with summary statistics.
#' @export
ddi_file_dscr <- function(id,
                          name      = NULL,
                          labl      = NULL,
                          format    = NULL,
                          data_dscr = NULL)

{
  ddi_file_dscr <- list(id        = id,
                        name      = ifelse(is.null(name), id, name),
                        labl      = labl,
                        format    = format,
                        data_dscr = data_dscr)
  class(ddi_file_dscr) <- "ddi_file_dscr"
  ddi_file_dscr
}

