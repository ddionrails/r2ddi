#
# csv2ddi(filename)
#
# Import CSV file into ddi object
#
csv2ddi = function(filename) {

  csv_file =
    read.csv(
      input_file,
      stringsAsFactors=FALSE,
      sep=csv.sep )

  ddi = list()
  class(ddi) = "ddi"

  return(ddi)
}
