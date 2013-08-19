#
# extract_ddiVar(var_dscr, keep_data, file_format)
#
# Helper function
#
# Params:
# * var_dscr: Variable
# * keep_data=[TRUE|FALSE]: keep original data in ddi-object
#                           -> increases size
# * file_format=[NA|Stata|SPSS|CSV|data.frame]: format (default=NA)
#
ddiExtractor <- function(var_dscr,
                         import_options,
                         file_format = NULL)
{

  main <- function() {
    if (class(var_dscr$data_table$valid) == "numeric" | class(var_dscr$data_table$valid) == "integer") {
      if (is.null(var_dscr$val_labels)) {
        var_dscr$sumStat <- .stat_numeric(var_dscr)
        if(import_options$jstat)
          var_dscr$jstat       <- jstat.numeric(var_dscr, import_options$time)
        var_dscr$intrvl  <- "numeric"
      } else {
        var_dscr$sumStat     <- .stat_labeled_numeric(var_dscr)
        var_dscr$value_table <- freq.labeled_numeric(var_dscr)
        if(import_options$jstat)
          var_dscr$jstat       <- jstat.labeled_numeric(var_dscr, import_options$time)
        var_dscr$intrvl      <- "labeled_numeric"
      }
    } else if (class(var_dscr$data_table$valid) == "character") {
      var_dscr$sumStat     <- .stat_character(var_dscr)
      var_dscr$value_table <- freq.character(var_dscr)
      if(import_options$jstat)
        var_dscr$jstat       <- jstat.character(var_dscr, import_options$time)
      var_dscr$intrvl      <- "string"
    } else if (class(var_dscr$data_table$valid) == "factor") {
      var_dscr$sumStat <- .stat_factor(var_dscr)
      var_dscr$intrvl  <- "factor"
    }
    if (import_options$keep_data == FALSE) { 
      var_dscr$data_table     <- NULL
    }
    var_dscr$val_labels <- NULL
    var_dscr$missings <- NULL
    var_dscr
  }

  .stat_numeric <- function(var_dscr) {
    summary <- summary(var_dscr$data_table$valid)
    sumStat <- list()
    for(i in 1:length(summary)) {
      sumStat[ names(summary)[i] ] <- summary[i]
    }   
    sumStat$valid <- length(var_dscr$data_table$valid[!is.na(var_dscr$data_table$valid)])
    sumStat$invalid <- length(var_dscr$data_table$valid[is.na(var_dscr$data_table$valid)])
    return(sumStat)
  }

  .stat_labeled_numeric <- function(var_dscr) {
    sumStat <- list()
    sumStat$valid <- length(var_dscr$data_table$valid[!is.na(var_dscr$data_table$valid)])
    sumStat$invalid <- length(var_dscr$data_table$valid[is.na(var_dscr$data_table$valid)])
    return(sumStat)
  }

  .stat_character <- function(var_dscr) {
    sumStat <- list()
    sumStat$valid <- length(var_dscr$data_table$valid[!is.na(var_dscr$data_table$valid)])
    sumStat$invalid <- length(var_dscr$data_table$valid[is.na(var_dscr$data_table$valid)])
    return(sumStat)
  }

  .stat_factor <- function(var_dscr) {
    return("stat_factor")
  }

  main()
}


