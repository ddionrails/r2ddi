#' Import multiple files from one directory
#'
#' @param path Path to files
#' @param file_type Accepted filetypes ("dta", "sav", "csv", or "all" for the
#'                  for the previous three)
#' @param multicore Use multicore functionallity
#' @export
dir2ddi <-
  function(path, file_type = "all", multicore=TRUE)
{

  if(!(file_type %in% c("all", "csv", "sav", "dta")))
  {
    cat("Invalid file type:", file_type, "\n")
    return()
  } else if(file_type == "all") {
    pattern <- ".(dta|csv|sav)$"
  } else {
    pattern <- paste(".", file_type, "$", sep = "")
  }

  file_list <- list.files(path)
  
  file_list <-
    file_list[
      regexpr(
        file_list,
        pattern     = pattern,
        ignore.case = TRUE) != -1]

  result_list <-
    mclapply(
      file_list,
      function(x)
      {
        stata2ddi(
          paste(path, x, sep = ""),
          x,
          keep_data = FALSE)
      })

  master <- result_list[[1]]

  if (length(result_list) > 1)
    for(i in 2:length(file_list))
      master <-
        attachFileDscr(
          master,
          result_list[[i]])

  return(master)
}
