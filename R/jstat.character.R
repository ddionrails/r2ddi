#' Calculate frequencies for character objects
#'
#' @param variable variable-object
#' @param time Time variable if long data
jstat.character <- function(variable, time=NULL)
{
  main <- function() {
    l <- list(table = .table(variable$data_table$valid, time),
              md5   = .md5(variable$data_table$valid))
    l
  }

  .table <- function(valid, time) {
    if(is.null(time))
      tab <- table(valid)
    else
      tab <- table(var = valid, time = time)
    tab <- data.frame(tab, stringsAsFactors = FALSE)
    x <- list()
    for (i in rownames(tab))
      x[[i]] <- tab[i, ]
    x
  }

  .md5 <- function(valid) {
    digest(paste(valid, collapse = ''), serialize = FALSE)
  }

  main()
}

