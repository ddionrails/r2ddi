#
# stata2ddi(file, data.label, keep.data)
#
# Import Stata file into ddi object
#
# Arguments:
# * file: Path to data file
# * data.label: Name of the data set
# * keep.data: Include the original data in the DDI object
#
stata2ddi <- function(filename, data_name, data_label=NULL,
                           missing.codes=NULL, keep_data=TRUE) {

  # Read Stata file
  stata_file <-
    read.dta(
      filename,
      convert.factors=FALSE,
      convert.dates=FALSE,
      missing.type=TRUE )

  dataDscr <- list()
  dataDscr$name <- data_name
  dataDscr$file_name <- filename
  dataDscr$file_format <- "Stata"
  dataDscr$timeStamp <- attr(stata_file, "time.stamp")
  ## data.label provided by user overrides data label in the dataset
  if(is.null(data_label)) {
    dataDscr$label <- attr(stata_file, "datalabel")
  } else {
    dataDscr$label <- data_label
  }
  dataDscr$varDscr = list()
  
  names(attr(stata_file, "var.labels"))   <-
    names(attr(stata_file, "val.labels")) <-
      names(attr(stata_file, "formats"))  <-
        names(attr(stata_file, "types"))  <- names(stata_file)

  stata.missings <-
    attr(stata_file, "missing")
  attr(stata_file, "missing") <-
    NULL

  dataDscr$varDscr <-
    lapply(
      seq_along(stata_file),
      function(i){
        r2ddi:::varDscr.stata(
          i,
          stata_file[[i]],
          stata.missings[[i]],
          attributes(stata_file),
          missing.codes)
      })

  names(dataDscr$varDscr) <- names(stata_file)

#  for( varname in colnames(stata_file) ) {
#    dataDscr$varDscr[[varname]] <-
#      list( name = varname,
#           label = attr(stata_file, "var.labels")[[varname]],
#            data = if(is.null(missing.codes)) {
#                     stata_file[[varname]]
#                   } else {
#                     ifelse(
#                       stata_file[[varname]] %in% missing.codes,
#                       NA,
#                       stata_file[[varname]])
#                   },
#        missings = r2ddi:::missings.stata(varname, stata_file, missing.codes),
#          format = attr(stata_file, "formats")[[varname]],
#      val_labels = attr(stata_file, "label.table")[[varname]]
#        )
#  }

  dataDscr$varDscr <-
    lapply(
      dataDscr$varDscr,
      r2ddi:::ddiExtractor.extract_ddiVar,
      keep_data,
      file_format = "Stata")

  ddi <- list( fileDscr=list(dataDscr) )
  names(ddi$fileDscr) <- dataDscr$name

  class(ddi) = "ddi"
  return(ddi)
}
