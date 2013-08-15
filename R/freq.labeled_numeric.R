#' Calculate frequencies for valued_numeric objects
#'
#' Will be implemented as a S3 method with binding to valued_numeric - maybe ;-)
#'
#' @param variable variable-object
freq.labeled_numeric <- function(variable)
{
  main <- function() {
    valid_tab   <- valid_labels   <- table(variable$data_table$valid)
    missing_tab <- missing_labels <-table(variable$data_table$missing)
    attributes(valid_tab) <- attributes(missing_tab) <- NULL

    valid_freq   <- .valid_freq(valid_tab, valid_labels)
    missing_freq <- .missing_freq(missing_labels, missing_tab)
    freq         <- .freq(valid_freq, missing_freq)
    value_table  <- .value_table(variable)
  
    value_table$valid <- ifelse(is.na(value_table$valid.x),
                                value_table$valid.y,
                                value_table$valid.x)
    value_table$freq  <- ifelse(is.na(value_table$freq),
                                0,
                                value_table$freq)
    value_table$valid.y <- value_table$valid.x <- NULL
    value_table
  }
  
  .valid_freq <- function(valid_tab, valid_labels) {
    if(length(valid_tab) > 0)
      return(
        data.frame(
          value            = names(valid_labels),
          freq             = valid_tab,
          valid            = TRUE,
          stringsAsFactors = FALSE))
    else
      return(NULL)
  }
  
  .missing_freq <- function(missing_tab, missing_labels) {
    if(length(missing_tab) > 0)
      missing_freq <-
        data.frame(
          value            = names(missing_labels),
          freq             = missing_tab,
          valid            = FALSE,
          stringsAsFactors = FALSE)
    else
      missing_freq <- NULL
  }
  
  .freq <- function(valid_freq, missing_freq)
    if(!is.null(valid_freq) & !is.null(missing_freq))
      return(rbind(valid_freq, missing_freq))
    else if(!is.null(valid_freq))
      return(valid_freq)
    else if (!is.null(missing_freq))
      return(missing_freq)
    else
      return(NULL)
  }
  
  .value_table <- function(variable) {
    if(is.null(freq)) {
      value_table      <- variable$value_table
      value_table$freq <- 0
      value_table$valid.x <- value_table$valid.y <- value_table$valid
    } else {
      value_table <-
        merge(
          variable$value_table,
          freq,
          by  = "value",
          all = TRUE)
    }
    value_table
  }

  main()
}

