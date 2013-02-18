
missings.stata <-
  function(x, stata_file,
           missing.codes=NULL) {

  ## get missings either from attributes or from codes supplied by user
  mis <- attr(stata_file, "missing")[[varname]]

  if(is.null(missing.codes)){
    if(is.null(mis))
      mis <- rep(NA, length(stata_file[[varname]]))
    else
      return(mis) 
  }
                                      # we have some (additional?) codes
  if (is.null(mis))
    mis <- rep(NA, length(stata_file[[varname]]))

  mis <-
    ifelse(
      !is.na(mis),
      mis,
      ifelse(stata_file[[varname]] %in% missing.codes, stata_file[[varname]], NA))

  return(mis)
}

