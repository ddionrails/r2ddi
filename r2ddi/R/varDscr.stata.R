#

varDscr.stata <- 
  function(
    i,
    var,
    missings,
    attr,
    missing_codes=NULL)
{

  varDscr <- list(
    name = attr$names[[i]],
    label = attr$var_labels[[i]],
    val_labels       = attr(var, "val_labels"),
    data =
      if(is.null(missing_codes))
      {
        var
      } else {
        ifelse(var %in% missing_codes, NA, var)
      },
    miss = r2ddi:::missings_stata(var, missings, missing_codes),
    format = attr$formats[[i]])

  tmp_labels <-
    ifelse(
      as.numeric(names(varDscr$val_labels)) > 2147483620,
      as.numeric(names(varDscr$val_labels)) - 2147483621,
      0)

  varDscr_df <-
    data.frame(
      label = varDscr$val_labels,
      stata_values = names(varDscr$val_labels),
      values =
        ifelse(
          as.numeric(names(varDscr$val_labels)) > 2147483620,
          paste(".", letters[tmp_labels], sep=""),
          names(varDscr$val_labels)),
      valid =
        ifelse(
          as.numeric(names(varDscr$val_labels)) > 2147483620,
          FALSE,
          TRUE)
      )

  attributes(varDscr$data) = NULL

  return(varDscr)
}
