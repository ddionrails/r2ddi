#
# stata2ddi(filename)
#
# Import Stata file into ddi object
#
stata2ddi = function(filename) {

  stata_file =
    read.dta(
      filename,
      convert.factors=FALSE,
      convert.dates=FALSE,
      missing.type=TRUE )


  ddi = list()
  class(ddi) = "ddi"

  return(ddi)
}
