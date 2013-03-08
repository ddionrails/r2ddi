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

  data_table <- data.stata(var, missings, missing_codes)

  value_table <-
    data.frame(
      label            = names(val_labels),
      value            = val_labels,
      valid            = ifelse(
                           val_labels >= 2147483621 |
                           val_labels %in% missing_codes,
                           FALSE,
                           TRUE),
      stringsAsFactors = FALSE)

  var_dscr <-
    list(
      name        = name,
      label       = label,
      val_labels  = val_labels,
      data        = data_table$valid,
      data_table  = data_table,
      value_table = value_table,
      miss        = data_table$missing,
      format      = format)

  attributes(var_dscr$data) <- NULL

  var_dscr <-
    ddiExtractor(
      var_dscr,
      keep_data   = keep_data,
      file_format = "Stata")


  if(nrow(var_dscr$value_table) > 0 &
     var_dscr$intrvl == "labeled_numeric")
  {
    tmp_values <-
      as.numeric(
        var_dscr$value_table$value[
          as.numeric(var_dscr$value_table$value) > 2147483621 &
          !is.na(var_dscr$value_table$value)]) - 2147483621
    var_dscr$value_table$value[
      as.numeric(var_dscr$value_table$value) > 2147483621 &
      !is.na(var_dscr$value_table$value)] <-
        paste(".", letters[tmp_values], sep="")
    var_dscr$value_table$value[
      suppressWarnings(as.numeric(var_dscr$value_table$value)) == 2147483621 &
      !is.na(var_dscr$value_table$value)] <- "."
  }

  return(var_dscr)
}
