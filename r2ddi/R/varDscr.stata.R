#

varDscr.stata <- 
  function(
    i,
    name,
    label,
    format,
    val_labels,
    var,
    missings,
    missing_codes=NULL)
{

  var_dscr <- list(
    name       = name,
    label      = label,
    val_labels = val_labels,
    data       =
      if(is.null(missing_codes))
      {
        var
      } else {
        ifelse(var %in% missing_codes, NA, var)
      },
    miss       = r2ddi:::missings.stata(var, missings, missing_codes),
    format     = format)

  tmp_labels <-
    ifelse(
      as.numeric(names(var_dscr$val_labels)) > 2147483620,
      as.numeric(names(var_dscr$val_labels)) - 2147483621,
      0)

  var_dscr_df <-
    data.frame(
      label = var_dscr$val_labels,
      stata_values = names(var_dscr$val_labels),
      values =
        ifelse(
          as.numeric(names(var_dscr$val_labels)) > 2147483620,
          paste(".", letters[tmp_labels], sep=""),
          names(var_dscr$val_labels)),
      valid =
        ifelse(
          as.numeric(names(var_dscr$val_labels)) > 2147483620,
          FALSE,
          TRUE)
      )

  attributes(var_dscr$data) = NULL

  return(var_dscr)
}
