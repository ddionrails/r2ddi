#' Import multiple files from one directory
#'
#' @param path_in Path to files
#' @param file_type Accepted filetypes ("dta", "sav", "csv", or "all" for the
#'                  for the previous three)
#' @param multicore Use multicore functionallity
#' @export
dir2xml <- function(path_in, path_out, file_type = "all", multicore = TRUE) {

  main <- function() {
    file_list <- .file_list(path_in, file_type)
    if(multicore)
      result_list <- mclapply(file_list, .process, path_in, path_out)
    else
      result_list <- lapply(file_list, .process, path_in, path_out)
    result_list
  }

  .process <- function(x, path_in, path_out) {
    ddi <- stata2ddi(paste(path_in, x, sep = ""), x, keep_data = FALSE)
    ddi2xml(ddi, paste(path_out, x, ".xml", sep=""))
    x
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

