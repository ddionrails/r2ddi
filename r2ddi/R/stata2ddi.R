#' stata to ddi
#'
#' Imports a Stata file and transforms it to a ddi object.
#' 
#' @param filename Name of a Stata file.
#' @param data_name Name of the dataset.
#' @param data_label Label of the dataset.
#' @param missing_codes Array of values, that should be treated as missing values.
#' @param keep_data Defines, if the original data should be included in the ddi object.
#' @param multicore, Defines, if you want r2ddi to use R's multicore functionallity.
#' @example examples/stata.R
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

  data_dscr <-
    list(
      name        = data_name,
      file_name   = filename,
      file_format = "Stata",
      timeStamp   = attr(stata_file, "time.stamp"))

  if(is.null(data_label))
  {
    data_dscr$label <- attr(stata_file, "datalabel")
  } else {
    data_dscr$label <- data_label
  }

  data_dscr$var_dscr = list()

  # TODO(mhebing): still neccessary after use of lapply?  
  names(attr(stata_file, "var.labels"))   <-
    names(attr(stata_file, "val.labels")) <-
      names(attr(stata_file, "formats"))  <-
        names(attr(stata_file, "types"))  <- names(stata_file)

  stata_missings              <- attr(stata_file, "missing")
  attr(stata_file, "missing") <- NULL

  #Value labels [jgoebel: Verstehe nicht was das soll??]
  for(name in names(attr(stata_file, "label.table")))
  {
    val_labels = names(attr(stata_file, "label.table")[[name]])
    names(val_labels) = attr(stata_file, "label.table")[[name]]
    attr(stata_file[[name]], "val_labels") = val_labels
  }

  data_dscr$var_dscr <-
    lapply(
      seq_along(stata_file),
      function(i)
      {
        r2ddi:::varDscr.stata(
          i             = i,
          name          = attr(stata_file, "names")[i],
          label         = attr(stata_file, "var.labels")[i],
          format        = attr(stata_file, "formats")[i],
          val_labels    = attr(var, "val_labels"),
          var           = stata_file[[i]],
          missings      = stata_missings[[i]],
          missing_codes = missing_codes)
      })

  names(data_dscr$var_dscr) <- names(stata_file)

  # TODO(mhebing): move call of ddiExtractor() to var_dscr.stata()?!
  if(multicore)
  {
    data_dscr$var_dscr <-
      mclapply(
        data_dscr$var_dscr,
        r2ddi:::ddiExtractor,
        keep_data,
        file_format = "Stata")
  } else {
    data_dscr$var_dscr <-
      lapply(
        data_dscr$var_dscr,
        r2ddi:::ddiExtractor,
        keep_data,
        file_format = "Stata")
  }

  ddi <-
   list(
     file_dscr = list(data_dscr))
  names(ddi$file_dscr) <- data_dscr$name

  class(ddi) = "ddi"
  return(ddi)
}
