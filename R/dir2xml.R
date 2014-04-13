#' Import multiple files from one directory
#'
#' @param path_in Path with Stata files (input).
#' @param path_out Path for XML files (output).
#' @param file_type Currently, only "dta" for Stata files is supported.
#' @param multicore Use multicore functionallity
#' @param missing_codes Define vector with study-wide missing codes,
#'        e.g. -9:-1 for the SOEP.
#' @param my_cores Set number of cores for multi-core processing,
#'        default is 2 cores.
#' @export
dir2xml <- function(path_in, path_out, file_type = "dta", multicore = TRUE, missing_codes = NULL, my_cores = 2) {


  main <- function() {
  # check if path_out exists, ddi2xml will fail otherwise
  if(!file_test("-d",path_out)) {
    message(paste0(path_out," is not a valid directory. Create the directory or check your file permissions."))
  } else {
    file_list <- .file_list(path_in, file_type)
    if(multicore)
      result_list <- mclapply(file_list, .process, path_in, path_out, missing_codes, mc.preschedule=FALSE, mc.cores = my_cores)
    else
      result_list <- lapply(file_list, .process, path_in, path_out, missing_codes)
    result_list
    }
  }

  .process <- function(x, path_in, path_out, missing_codes) {
    ddi <- stata2ddi(
      file.path(path_in, x),
      .filename_without_extension(x),
      keep_data = FALSE,
      missing_codes = missing_codes)
    ddi2xml(ddi, file.path(
      path_out, paste(.filename_without_extension(x), ".xml", sep="")
    ))
    x
  }

  .filename_without_extension <- function(x) {
    gsub("(.*)[.][a-z0-1]*", "\\1", x, ignore.case=T)
  }

  .file_list <- function(path_in, file_type) {
    fl <- list.files(path_in)
    ex <- regexpr(fl, pattern = .pattern(file_type), ignore.case = TRUE)
    fl[ex != -1]
  }

  .pattern <- function(file_type) {
    if(!(file_type %in% c("all", "csv", "sav", "dta"))) {
      cat("Invalid file type:", file_type, "\n")
      pattern <- NULL
    } else if(file_type == "all") {
      pattern <- ".(dta|csv|sav)$"
    } else {
      pattern <- paste(".", file_type, "$", sep = "")
    }
    pattern
  }

  main()
}

