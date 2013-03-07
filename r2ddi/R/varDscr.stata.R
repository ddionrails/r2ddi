#' Variable description (Stata)
#'
#' Produce variable description for Stata-based documentation.
#' 
varDscr.stata <- 
  function(
    i,
    name,
    label,
    format,
    val_labels,
    var,
    missings      = NA,
    missing_codes = NULL)
{

  data_frame <- data.stata(var, missings, missing_codes)

  # TODO: Do we handle "."-missings correctly?
  tmp_values <-
    ifelse(
      as.numeric(names(val_labels)) > 2147483620,
      as.numeric(names(val_labels)) - 2147483621,
      0)

  value_frame <-
    data.frame(
      label        = val_labels,
      stata_values = names(val_labels),
      values       =
        ifelse(
          as.numeric(names(val_labels)) > 2147483620,
          paste(".", letters[tmp_values], sep=""),
          names(val_labels)),
      valid        =
        ifelse(
          as.numeric(names(val_labels)) > 2147483620,
          FALSE,
          TRUE)
      )

  var_dscr <- list(
    name        = name,
    label       = label,
    val_labels  = val_labels,
    data        = data,
    data_frame  = data_frame,
    value_frame = value_frame,
    miss        = r2ddi:::missings.stata(var, missings, missing_codes),
    format      = format)

  attributes(var_dscr$data) <- NULL

  return(var_dscr)
}
