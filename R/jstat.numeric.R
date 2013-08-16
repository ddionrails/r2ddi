#' Calculate frequencies for character objects
#'
#' @param variable variable-object
#' @param time Time variable if long data
jstat.numeric <- function(variable, time=NULL)
{
  main <- function() {
    l <- list(densit = .density(variable$data_table$valid),
              md5   = md5(variable$data_table$valid))
    l
  }

  .density <- function(valid) {
    d <- density(valid, n = 50)
    d$call <- NULL
    d$x <- round(d$x, 4)
    d$y <- round(d$y, 4)
    d
  }

  main()
}

