#' ddi_variable
#'
#' @param id Variable name as string identifier.
#' @export
ddi_variable <- function(id) {
  variable <- list(
    id = id,
    label = NULL,
    statistics = NULL)
  class(variable) <- "ddi_variable"
  variable
}

#' print.ddi_variable
#' 
#' @param x ddi_variable object
#' @S3method print ddi_variable
#' @export
print.ddi_variable <- function(x) {
  cat("\nDDI Variable\n\n",
      "Name:  ", x$id, "\n",
      "Label: ", as.character(x$label), "\n",
      sep="")
}
