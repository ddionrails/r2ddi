#' Calculate frequencies for character objects
#'
#' @param variable variable-object
freq.character <- function(variable)
{
  valid_tab <- valid_labels <- table(variable$data_table$valid)
  attributes(valid_tab) <- NULL

  if(length(valid_tab) > 0)
    value_table <-
      data.frame(
        value            = names(valid_labels),
        label            = names(valid_labels),
        freq             = valid_tab,
        valid            = TRUE,
        stringsAsFactors = FALSE)
  else
    value_table <- NULL

  value_table
}

