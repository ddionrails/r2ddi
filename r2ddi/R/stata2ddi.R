#
# stata2ddi(file, data_label, keep_data)
#
# Import Stata file into ddi object
#
# Arguments:
# * file: Path to data file
# * data_label: Name of the data set
# * keep_data: Include the original data in the DDI object
#
stata2ddi <- function(filename, data_name, data_label=NULL,
                           missing_codes=NULL, keep_data=TRUE) {

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
  
  names(attr(stata_file, "var.labels")) <-
    names(attr(stata_file, "val.labels")) <-
      names(attr(stata_file, "formats")) <-
        names(attr(stata_file, "types")) <- names(stata_file)

  for( varname in colnames(stata_file) ) {
    dataDscr$varDscr[[varname]] <-
      list( name = varname,
           label = attr(stata_file, "var.labels")[[varname]],
            data = if(is.null(missing_codes)) {
                     stata_file[[varname]]
                   } else {
                     ifelse(
                       stata_file[[varname]] %in% missing_codes,
                       NA,
                       stata_file[[varname]])
                   },
        missings = r2ddi:::missings.stata(varname, stata_file, missing_codes),
          format = attr(stata_file, "formats")[[varname]],
      val_labels = attr(stata_file, "label.table")[[varname]]
        )
  }

  dataDscr$varDscr <- lapply(dataDscr$varDscr, r2ddi:::ddiExtractor.extract_ddiVar, keep_data, file_format = "Stata")

  ddi <- list( fileDscr=list(dataDscr) )
  names(ddi$fileDscr) <- dataDscr$name

  ## class(ddi) = "ddi"
  return(ddi)
}
