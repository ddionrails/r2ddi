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

  path_list <- paste(path, file_list, sep = "")

  master <- stata2ddi(path_list[1], file_list[1], multicore = multicore)

  if (length(file_list) > 1)
    for(i in 2:length(file_list))
      master <-
        attachFileDscr(
          master,
          stata2ddi(path_list[i], file_list[i], multicore = multicore))

  return(master)
}
