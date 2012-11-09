#
# stata2ddi(filename)
#
# Import Stata file into ddi object
#
stata2ddi = function(filename) {

  library("foreign")

  # Read Stata file
  stata_file =
    read.dta(
      filename,
      convert.factors=FALSE,
      convert.dates=FALSE,
      missing.type=TRUE )


  ######################### FUNCTIONS #########################

  # Internal function for extracting metadata
  extract_ddiVar = function(var) {
    ddiVar = list()

    ddiVar$raw_data = var
    
    if(class(var) == "numeric") {
      ddiVar$sumStat = stat_numeric(var)
    }

    return(ddiVar)
  }

  # Calculate sumStat for numeric variables
  stat_numeric = function(var) {
    summary = summary(var)
    sumStat = list()
    for(i in 1:length(summary)) {
      sumStat[ names(summary)[i] ] = summary[i]
    }
    return(sumStat)
  }


  ######################### START #########################

  ddi = list()
  ddi$fileDscr = list()
  ddi$fileDscr$fileName = filename
  ddi$fileDscr$name = filename
  ddi$fileDscr$varDscr = lapply(stata_file, extract_ddiVar)

  for( varname in colnames(stata_file) ) {
    ddi$fileDscr$varDscr[[varname]][["name"]] = varname
  }

# class(ddi) = "ddi"
  return(ddi)
}
