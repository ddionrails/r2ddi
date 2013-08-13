#' Import multiple files from one directory
#'
#' @param path Path to files
#' @param file_type Accepted filetypes ("dta", "sav", "csv", or "all" for the
#'                  for the previous three)
#' @param multicore Use multicore functionallity
#' @export
dir2ddi <- function(path, file_type = "all", multicore = TRUE) {

  main <- function() {
    file_list <- .file_list(path, file_type)
    if(multicore)
      result_list <- mclapply(file_list, function(x) .run_stata2ddi(path, x))
    else
      result_list <- lapply(file_list, function(x) .run_stata2ddi(path, x))
    master <- .combine(result_list)
    master
  }

  .run_stata2ddi <- function(path, x) {
    stata2ddi(paste(path, x, sep = ""), x, keep_data = FALSE)
  }

  .file_list <- function(path, file_type) {
    fl <- list.files(path)
    ex <- regexpr(fl, pattern = .pattern(file_type), ignore.case = TRUE)
    fl[ex != -1]
  }

  .combine <- function(result_list) {
    master <- result_list[[1]]
    if (length(result_list) > 1)
      for(i in 2:length(file_list))
        master <- attachFileDscr(master, result_list[[i]])
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

  main()
}

