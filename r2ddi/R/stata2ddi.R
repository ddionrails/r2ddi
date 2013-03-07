#' stata to ddi
#'
#' Whatever...
#' 
#' @param filename Name of a Stata file
#' @export stata2ddi
stata2ddi <-
  function(
    filename,
    data_name,
    data_label    = NULL,
    missing_codes = NULL,
    keep_data     = TRUE,
    multicore     = FALSE)
{

  # Read Stata file
  stata_file <-
    read.dta(
      filename,
      convert.factors = FALSE,
      convert.dates   = FALSE,
      missing.type    = TRUE )

  dataDscr <-
    list(
      name        = data_name,
      file_name   = filename,
      file_format = "Stata",
      timeStamp   = attr(stata_file, "time.stamp"))

  if(is.null(data_label))
  {
    dataDscr$label <- attr(stata_file, "datalabel")
  }else{
    dataDscr$label <- data_label
  }

  dataDscr$varDscr = list()

  # TODO(mhebing): still neccessary after use of lapply?  
  names(attr(stata_file, "var.labels"))   <-
    names(attr(stata_file, "val.labels")) <-
      names(attr(stata_file, "formats"))  <-
        names(attr(stata_file, "types"))  <- names(stata_file)

  stata_missings <-
    attr(stata_file, "missing")
  attr(stata_file, "missing") <-
    NULL

  #Value labels [jgoebel: Verstehe nicht was das soll??]
  for(name in names(attr(stata_file, "label.table")))
  {
    val_labels = names(attr(stata_file, "label.table")[[name]])
    names(val_labels) = attr(stata_file, "label.table")[[name]]
    attr(stata_file[[name]], "val_labels") = val_labels
  }

  dataDscr$varDscr <-
    lapply(
      seq_along(stata_file),
      function(i)
      {
        r2ddi:::varDscr.stata(
          i             = i,
          var           = stata_file[[i]],
          missings      = stata_missings[[i]],
          attr          = attributes(stata_file),
          missing_codes = missing_codes)
      })

  names(dataDscr$varDscr) <- names(stata_file)

  # TODO(mhebing): move call of ddiExtractor() to varDscr.stata()?!
  if(multicore)
  {
    dataDscr$varDscr <-
      mclapply(
        dataDscr$varDscr,
        r2ddi:::ddiExtractor,
        keep_data,
        file_format = "Stata")
  } else {
    dataDscr$varDscr <-
      lapply(
        dataDscr$varDscr,
        r2ddi:::ddiExtractor,
        keep_data,
        file_format = "Stata")
  }

  ddi <- list( fileDscr=list(dataDscr) )
  names(ddi$fileDscr) <- dataDscr$name

  class(ddi) = "ddi"
  return(ddi)
}
