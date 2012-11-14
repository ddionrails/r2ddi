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
stata2ddi = function(file, data_name, data_label, keep_data=TRUE) {

  library("foreign")


  ######################### START #########################

  # Read Stata file
  stata_file =
    read.dta(
      file,
      convert.factors=FALSE,
      convert.dates=FALSE,
      missing.type=TRUE )

  dataDscr = list()
  dataDscr$name = data_name
  dataDscr$label = data_label
  dataDscr$file_name = file
  dataDscr$file_format = "Stata"
  dataDscr$timeStamp = attr(stata_file, "time.stamp")
  dataDscr$label = attr(stata_file, "datalabel")

  names(attr(stata_file, "var.labels")) = names(stata_file)
  names(attr(stata_file, "val.labels")) = names(stata_file)
  names(attr(stata_file, "formats")) = names(stata_file)
  names(attr(stata_file, "types")) = names(stata_file)

  for( varname in colnames(stata_file) ) {
    dataDscr$varDscr[[varname]] = list()
    dataDscr$varDscr[[varname]][["name"]] = varname
    dataDscr$varDscr[[varname]][["label"]] =
      attr(stata_file, "var.labels")[[varname]]
    dataDscr$varDscr[[varname]][["data"]] = stata_file[[varname]]
    dataDscr$varDscr[[varname]][["missings"]] =
      attr(stata_file, "missing")[[varname]]
    dataDscr$varDscr[[varname]][["format"]] =
      attr(stata_file, "formats")[[varname]]
    dataDscr$varDscr[[varname]][["val_labels"]] =
      attr(stata_file, "label.table")[[varname]]
  }

  dataDscr$varDscr = lapply(dataDscr$varDscr, function(x) ddiExtractor.extract_ddiVar(x, keep_data, file_format = "Stata"))

  ddi = list()
  ddi$fileDscr = list()
  ddi$fileDscr[[dataDscr$name]] = dataDscr

# class(ddi) = "ddi"
  return(ddi)
}
