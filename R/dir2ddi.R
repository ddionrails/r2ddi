#' Import multiple files from one directory
#'
#' @param path Path to files
#' @param file_type Accepted filetypes ("dta", "sav", "csv", or "all" for the
#'                  for the previous three)
#' @param multicore Use multicore functionallity
#' @export
dir2ddi <- function(path, file_type = "all", multicore=TRUE) {

  main <- function() {
    pattern <- .pattern(file_type)
    if is.null(pattern) return()
    file_list <- .file_list(path, pattern)

    if(multicore) {
      result_list <- mclapply(file_list, .run_stata2ddi(path, x))
    } else {
      result_list <- lapply(file_list, .run_stata2ddi(path, x))
    }

    master <- .combine(result_list)
    return(master)
  }

  .run_stata2ddi <- function(path, x) {
    stata2ddi(paste(path, x, sep = ""),
              x,
              keep_data = FALSE)
  }

  .file_list <- function(path, pattern) {
    fl <- list.files(path)
    fl <- fl[regexpr(fl,
                     pattern     = pattern,
                     ignore.case = TRUE) != -1]
    fl
  }

  .combine <- function(result_list) {
    master <- result_list[[1]]
    if (length(result_list) > 1)
      for(i in 2:length(file_list))
        master <-
          attachFileDscr(
            master,
            result_list[[i]])
    master
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

  x <- main()
  x
}
