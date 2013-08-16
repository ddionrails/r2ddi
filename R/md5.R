#' Hash function for internal use
#'
#' @param valid
#' @param id
md5 <- function(valid, id = NULL)
{
  if(is.null(id))
    id <- 1:length(valid)
  valid <- valid[order(id)]
  digest(paste(valid, collapse = ''), serialize = FALSE)
}

