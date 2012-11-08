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

  stata_file = as.list(stata_file)

  # Internal function for extracting metadata
  extract_ddiVar = function(var) {
    ddiVar = list()
    class(ddiVar) = "ddiVar"

    

    return(ddiVar)
  }

  ddi = list()
  class(ddi) = "ddi"
  ddi$fileDscr = list()
  ddi$fileDscr$fileName = filename
  ddi$fileDscr$name = filename
  ddi$fileDscr$varDscr = lapply(stata_file, extract_ddiVar)

  return(ddi)
}
