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
#' @export
stata_import <- function(filename,
                         data_name,
                         data_label    = NULL ,
                         missing_codes = NULL ,
                         keep_data     = TRUE ,
                         multicore     = FALSE,
                         is_stata_mis  = TRUE )
{
  main <- function() {
    stata_file <- .read_stata(filename, is_stata_mis)
    import_options <- list(missing_codes = missing_codes,
                           keep_data     = keep_data    ,
                           multicore     = multicore    ,
                           is_stata_mis  = is_stata_mis )
    code_book <- ddi_code_book()
    code_book$file_dscr[[data_name]] <- ddi_file_dscr(id     = data_name,
                                                      name   = filename,
                                                      labl   = data_label,
                                                      format = "Stata")
    code_book$file_dscr[[data_name]]$data_dscr <- .data_dscr(stata_file, import_options)
    code_book
  }


  .read_stata <- function(filename, is_stata_mis) {
    stata_file <- read.dta(filename,
                           convert.factors = FALSE,
                           convert.dates   = FALSE,
                           missing.type    = is_stata_mis)
    names(attr(stata_file, "var.labels"))   <-
      names(attr(stata_file, "val.labels")) <-
        names(attr(stata_file, "formats"))  <-
          names(attr(stata_file, "types"))  <- names(stata_file)
    stata_file
  }

  .data_dscr <- function(stata_file, import_options) {
    if(multicore == TRUE)
      data_dscr <- mclapply(seq_along(stata_file), function(x) .parser(x, stata_file, import_options))
    else
      data_dscr <- lapply(seq_along(stata_file), function(x) .parser(x, stata_file, import_options))
    names(data_dscr) <- names(stata_file)
    data_dscr
  }


  .parser <- function(i, stata_file, import_options) {
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
      missing_codes = import_options$missing_codes,
      keep_data     = import_options$keep_data)
  }


  code_book <- main()
  code_book
}
