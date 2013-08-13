#' stata to ddi
#'
#' Imports a Stata file and transforms it to a ddi object.
#' 
#' @param filename Name of a Stata file.
#' @param data_name Name of the dataset.
#' @param data_label Label of the dataset.
#' @param missing_codes Array of values, that should be treated as missing values.
#' @param keep_data Defines, if the original data should be included in the ddi object.
#' @example examples/stata.R
#' @export
stata_import <- function(filename,
                         data_name,
                         data_label    = NULL ,
                         missing_codes = NULL ,
                         keep_data     = TRUE ,
                         is_stata_mis  = TRUE )
{
  main <- function() {
    stata_file <- .read_stata(filename, is_stata_mis)
    import_options <- list(missing_codes = missing_codes,
                           keep_data     = keep_data    ,
                           is_stata_mis  = is_stata_mis )
    code_book <- ddi_code_book()
    code_book$file_dscr[[data_name]] <- ddi_file_dscr(id     = data_name ,
                                                      name   = filename  ,
                                                      labl   = data_label,
                                                      format = "Stata"   )
    code_book$file_dscr[[data_name]]$data_dscr <- .data_dscr(stata_file, import_options)
    code_book
  }

  .read_stata <- function(filename, is_stata_mis) {
    stata_file <- read.dta(filename,
                           convert.factors = FALSE,
                           convert.dates   = FALSE,
                           missing.type    = is_stata_mis)

    # TODO: The following lines might be a performance disaster.
    # names(attr(stata_file, "var.labels"))   <-
    #   names(attr(stata_file, "val.labels")) <-
    #     names(attr(stata_file, "formats"))  <-
    #       names(attr(stata_file, "types"))  <- names(stata_file)
    stata_file
  }

  .data_dscr <- function(stata_file, import_options) {
    data_dscr <- lapply(seq_along(stata_file),
                        function(x) .parser(x, stata_file, import_options))
    names(data_dscr) <- names(stata_file)
    data_dscr
  }

  .parser <- function(i, stata_file, import_options) {
    var <- ddi_var(id   = attr(stata_file, "names")[i],
                   labl = attr(stata_file, "var.labels")[i])
    var$format <- attr(stata_file, "formats")[i]
    var$miss   <- attr(stata_file, "missing")[[i]]
    r2ddi:::ddi_var_dscr_stata(
      i             = i,
      var           = var,
      data          = stata_file[[i]],
      val_labels    = attr(stata_file, "label.table")[[
                        attr(stata_file, "val.labels")[i] ]],
      import_options = import_options)
  }

  main()
}

