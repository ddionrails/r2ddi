#
# spss2ddi(filename)
#
# Import SPSS file into ddi object
#
spss2ddi = function(filename) {

  spss_file =
    read.spss(
      input_file,
      to.data.frame=FALSE,
      use.value.labels=FALSE )

  ddi = list()
  class(ddi) = "ddi"

  return(ddi)
}
