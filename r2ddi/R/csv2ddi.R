#
# csv2ddi(filename)
#
# Import CSV file into ddi object
#
csv2ddi <-
  function(
    filename,
    data_name=NULL,
    data_label=NULL,
    missing_codes=NULL,
    csv_sep=",",
    keep_data=NULL ) {

  csv_file <-
    read.csv(
      filename,
      stringsAsFactors=FALSE,
      sep=csv_sep )

  dataDscr <- list()
  dataDscr$name        <- data_name
  dataDscr$file_name   <- filename
  dataDscr$file_format <- "CSV"


  for( varname in colnames(csv_file) ) {
    dataDscr$varDscr[[varname]] <-
      list( 
        name = varname,
        data =
          if(is.null(missing_codes)) {
            csv_file[[varname]]
          } else {
            ifelse(
              csv_file[[varname]] %in% missing_codes,
              NA,
              csv_file[[varname]])
          } )
  }


  dataDscr$varDscr <-
    lapply(
      dataDscr$varDscr,
      ddiExtractor.extract_ddiVar,
      keep_data,
      file_format="CSV")

  ddi <- list( fileDscr=list(dataDscr) )
  names(ddi$fileDscr) <- dataDscr$name

  ## class(ddi) = "ddi"
  return(ddi)

}
