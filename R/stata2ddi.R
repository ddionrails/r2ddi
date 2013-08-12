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
    multicore     = FALSE,
    is_stata_mis  = TRUE)
{

  # Read Stata file
  stata_file <-
    read.dta(
      filename,
      convert.factors = FALSE,
      convert.dates   = FALSE,
      missing.type    = is_stata_mis)

  # TODO: still neccessary after use of lapply?  
  #       -> Neccessary for val.labels ?!
  names(attr(stata_file, "var.labels"))   <-
    names(attr(stata_file, "val.labels")) <-
      names(attr(stata_file, "formats"))  <-
        names(attr(stata_file, "types"))  <- names(stata_file)

  # Build basic data_dscr
  data_dscr <-
    list(
      name        = data_name,
      file_name   = filename,
      file_format = "Stata",
      label       = ifelse(
                      is.null(data_label),
                      attr(stata_file, "datalabel"),
                      data_label),
      timeStamp   = attr(stata_file, "time.stamp"),
      var_dscr    = list())

  # Add var_dscr to data_dscr
  # TODO Move all attributes (exept for missing_codes and keep_data)
  #      to the raw data_dscr list to reduce attributes.
  internal_function <-
      function(i)
      {
        r2ddi:::varDscr.stata(
          i             = i,
          name          = attr(stata_file, "names")[i],
          label         = attr(stata_file, "var.labels")[i],
          format        = attr(stata_file, "formats")[i],
          val_labels    = attr(stata_file, "label.table")[[
                            attr(stata_file, "val.labels")[[
                              names(stata_file)[i] ]] ]],
          var           = stata_file[[i]],
          missings      = attr(stata_file, "missing")[[i]],
          missing_codes = missing_codes,
          keep_data     = keep_data)
      }

  if(multicore == TRUE)
    data_dscr$var_dscr <-
      mclapply(
        seq_along(stata_file),
        internal_function)
  else
    data_dscr$var_dscr <-
      lapply(
        seq_along(stata_file),
        internal_function)

  names(data_dscr$var_dscr) <- names(stata_file)

  ddi <-
   list(
     file_dscr = list(data_dscr))

  names(ddi$file_dscr) <- data_dscr$name

  class(ddi) = "ddi"
  return(ddi)
}
