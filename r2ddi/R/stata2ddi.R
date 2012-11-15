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

  require("foreign")

  ######################### START #########################

  # Read Stata file
  stata_file =
    read.dta(
      filename,
      convert.factors=FALSE,
      convert.dates=FALSE,
      missing.type=TRUE )

  dataDscr = list()
  dataDscr$name = data_name
  dataDscr$file_name = filename
  dataDscr$file_format = "Stata"
  dataDscr$timeStamp = attr(stata_file, "time.stamp")
  ## data.label provided by user overrides data label in the dataset
  if(is.null(data_label)) {
    dataDscr$label = attr(stata_file, "datalabel")
  } else {
    dataDscr$label = data_label
  }
  
  names(attr(stata_file, "var.labels")) <-
   names(attr(stata_file, "val.labels")) <-
    names(attr(stata_file, "formats")) <-
      names(attr(stata_file, "types")) <- names(stata_file)

  get.missings <- function(x, stata_file=stata_file,
                           missing.codes=missing_codes) {
    ## get missings either from attributes or from codes supplied by user
    mis <- attr(stata_file, "missing")[[varname]]
    if(is.null(missing.codes))   # nothing supplied, exit 
      return(mis) 
                                        # we have some (additional?) codes
    if (is.null(mis))
      mis <- rep(NA, length(stata_file[[varname]]))
    mis <- ifelse(!is.na(mis), mis,
                  ifelse(stata_file[[varname]] %in% missing_codes, stata_file[[varname]], NA))
    return(mis)
  }

  
  for( varname in colnames(stata_file) ) {
    dataDscr$varDscr[[varname]] <-
      list( name = varname,
           label = attr(stata_file, "var.labels")[[varname]],
            data = if(is.null(missing_codes)) {stata_file[[varname]]} else {ifelse(stata_file[[varname]] %in% missing_codes, NA, stata_file[[varname]])},
        missings = get.missings(varname, stata_file=stata_file),
          format = attr(stata_file, "formats")[[varname]],
      val_labels = attr(stata_file, "label.table")[[varname]]
        )
  }

  dataDscr$varDscr = lapply(dataDscr$varDscr, ddiExtractor.extract_ddiVar, keep_data, file_format = "Stata")

  ddi <- list(fileDscr = list( dataDscr))
  names(ddi$fileDscr) <- dataDscr$name

  ## class(ddi) = "ddi"
  return(ddi)
}
