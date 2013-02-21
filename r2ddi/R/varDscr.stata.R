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
    data   =
      if(is.null(missing.codes))
      {
        var
      } else {
        ifelse(var %in% missing.codes, NA, var)
      },
    miss   = r2ddi:::missings.stata(var, missings, missing.codes),
    format = attr$formats[[i]])

  return(varDscr)
}
