#' Generate a ddi code book.
#'
#' @param id Variable name.
#' @param labl Human readable label.
#' @param intrvl Variable format.
#' @param catgry data.frame with categories.
#' @param sum_stat data.frame with summary statistics.
#' @export
ddi_code_book <- function( id        = NULL ,
                           doc_dscr  = NULL ,
                           stdy_dscr = NULL ,
                           file_dscr = NULL )
{
  ddi_code_book <-
    list(
      id        = id        ,
      doc_dscr  = doc_dscr  ,
      stdy_dscr = stdy_dscr ,
      file_dscr = file_dscr )
  lapply(ddi_code_book, function(x) ifelse(is.null(x), list(), x))
  class(ddi_code_book) <- "ddi_code_book"
  ddi_code_book
}

#' Print ddi codebook
#'
#' @param x ddi_code_book object
#' S3method print ddi_code_book
#' @export
print.ddi_code_book <- function(x) {
  cat("DDI Codebook\n")
  cat("------------\n")
  cat("List of datasets:\n")
  for(i in names(x$file_dscr))
  {
    cat("* ", i, sep="")
    cat(" (", length(x[["file_dscr"]][[i]][["var_dscr"]]), " variables)\n", sep="")
  }
}

