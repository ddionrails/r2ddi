#

varDscr.stata <- 
  function(
    i,
    var,
    missings,
    attr,
    missing.codes=NULL)
{

  varDscr <- list(
    name   = attr$names[[i]],
    label  = attr$var.labels[[i]],
    val.labels   = attr(var, "val.labels"),
    data   =
      if(is.null(missing.codes))
      {
        var
      } else {
        ifelse(var %in% missing.codes, NA, var)
      },
    miss   = r2ddi:::missings.stata(var, missings, missing.codes),
    format = attr$formats[[i]])

  attributes(varDscr$data) = NULL

  return(varDscr)
}
