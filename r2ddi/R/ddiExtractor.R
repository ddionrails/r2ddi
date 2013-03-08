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
ddiExtractor <-
  function(
    var_dscr,
    keep_data,
    file_format = NULL)
{

  ##### INTERNAL FUNCTIONS #####

  stat_numeric <- function(var_dscr)
  {
    summary <- summary(var_dscr$data)
    sumStat <- list()
    for(i in 1:length(summary))
    {
      sumStat[ names(summary)[i] ] <- summary[i]
    }   
    sumStat$valid <- length(var_dscr$data[!is.na(var_dscr$data)])
    sumStat$invalid <- length(var_dscr$data[is.na(var_dscr$data)])
    return(sumStat)
  }

  stat_labeled_numeric <- function(var_dscr)
  {
    sumStat <- list()
    sumStat$valid <- length(var_dscr$data[!is.na(var_dscr$data)])
    sumStat$invalid <- length(var_dscr$data[is.na(var_dscr$data)])
    return(sumStat)
  }

  stat_character <- function(var_dscr)
  {
    sumStat <- list()
    sumStat$valid <- length(var_dscr$data[!is.na(var_dscr$data)])
    sumStat$invalid <- length(var_dscr$data[is.na(var_dscr$data)])
    return(sumStat)
  }

  stat_factor <- function(var_dscr)
  {
    return("stat_factor")
  }

  # DEPRECATED !!!
  catgry_labeled_numeric <- function(var_dscr)
  {
    catgry <- list()
    return(catgry)
  }

  catgry_factor <- function(var_dscr)
  {
    return("catgry_factor")
  }

  catgry_character <- function(var_dscr)
  {
    t <- table(var_dscr$data)
    catgry <- list()
    for(i in 1:length(t))
    {
      cat <- list()
      cat$value <- names(t)[i]
      cat$valid <- TRUE
      cat$freq <- t[[i]]
      catgry[[i]] <- cat
    }
    return(catgry)
  }

  ##### START #####

  if (class(var_dscr$data) == "numeric" | class(var_dscr$data) == "integer")
  {
    if (is.null(var_dscr$val_labels))
    {
      var_dscr$sumStat <- stat_numeric(var_dscr)
      var_dscr$intrvl  <- "numeric"
    } else {
      var_dscr$sumStat     <- stat_labeled_numeric(var_dscr)
#      var_dscr$catgry      <- catgry_labeled_numeric(var_dscr)
      var_dscr$value_table <- freq.labeled_numeric(var_dscr)
      var_dscr$intrvl      <- "labeled_numeric"
    }
  } else if (class(var_dscr$data) == "character") {
    var_dscr$sumStat     <- stat_character(var_dscr)
#    var_dscr$catgry      <- catgry_character(var_dscr)
    var_dscr$value_table <- freq.character(var_dscr)
    var_dscr$intrvl      <- "string"
  } else if (class(var_dscr$data) == "factor") {
    var_dscr$sumStat <- stat_factor(var_dscr)
#    var_dscr$catgry  <- catgry_factor(var_dscr)
    var_dscr$intrvl  <- "factor"
  }

  if (keep_data == FALSE)
  { 
    var_dscr$data     <- NA
    var_dscr$missings <- NA
  }
   
  return(var_dscr)
}


