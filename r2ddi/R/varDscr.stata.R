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
    missing_codes = NULL,
    keep_data     = TRUE)
{

  data_frame <- data.stata(var, missings, missing_codes)

  # TODO: Do we handle "."-missings correctly?
  tmp_values <-
    ifelse(
      val_labels >= 2147483621,
      val_labels - 2147483621,
      0)

  value_frame <-
    data.frame(
      label            = names(val_labels),
      stata_value      = val_labels,
      value            = ifelse(
                           val_labels >= 2147483621,
                           paste(".", letters[tmp_values], sep=""),
                           val_labels),
      valid            = ifelse(
                           val_labels >= 2147483621 |
                           val_labels %in% missing_codes,
                           FALSE,
                           TRUE),
      stringsAsFactors = FALSE)

  # TODO:
  #   - replace data and miss by data_frame
  #   - replace val_labels by value_frame
  var_dscr <-
    list(
      name        = name,
      label       = label,
      val_labels  = val_labels,
      data        = data_frame$valid,
      data_frame  = data_frame,
      value_frame = value_frame,
      miss        = data_frame$missing,
      format      = format)

  attributes(var_dscr$data) <- NULL

  # TODO: is "r2ddi:::" neccessary?
  var_dscr <-
    r2ddi:::ddiExtractor(
      var_dscr,
      keep_data   = keep_data,
      file_format = "Stata")

  return(var_dscr)
}
