#
# extract_ddiVar(var, keep_data, file_format)
#
# Helper function
#
# Params:
# * var: Variable
# * keep_data=[TRUE|FALSE]: keep original data in ddi-object
#                           -> increases size
# * file_format=[NA|Stata|SPSS|CSV|data.frame]: format (default=NA)
#
ddiExtractor <-
  function(
    var,
    keep_data,
    file_format = NULL)
{

  ##### INTERNAL FUNCTIONS #####

  stat_numeric <- function(var)
  {
    summary <- summary(var$data)
    sumStat <- list()
    for(i in 1:length(summary))
    {
      sumStat[ names(summary)[i] ] <- summary[i]
    }   
    sumStat$valid <- length(var$data[!is.na(var$data)])
    sumStat$invalid <- length(var$data[is.na(var$data)])
    return(sumStat)
  }

  stat_labeled_numeric <- function(var)
  {
    sumStat <- list()
    sumStat$valid <- length(var$data[!is.na(var$data)])
    sumStat$invalid <- length(var$data[is.na(var$data)])
    return(sumStat)
  }

  stat_character <- function(var)
  {
    sumStat <- list()
    sumStat$valid <- length(var$data[!is.na(var$data)])
    sumStat$invalid <- length(var$data[is.na(var$data)])
    return(sumStat)
  }

  stat_factor <- function(var)
  {
    return("stat_factor")
  }

  # DEPRECATED !!!
  catgry_labeled_numeric <- function(var)
  {
    catgry <- list()
    return(catgry)
  }

  catgry_factor <- function(var)
  {
    return("catgry_factor")
  }

  catgry_character <- function(var)
  {
    t <- table(var$data)
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

  if (class(var$data) == "numeric" | class(var$data) == "integer")
  {
    if (is.null(var$val_labels))
    {
      var$sumStat <- stat_numeric(var)
      var$intrvl  <- "numeric"
    } else {
      var$sumStat <- stat_labeled_numeric(var)
      var$catgry  <- catgry_labeled_numeric(var)
      var$value_table <- freq.labeled_numeric(var)
      var$intrvl  <- "labeled_numeric"
    }
  } else if (class(var$data) == "character") {
    var$sumStat <- stat_character(var)
    var$catgry  <- catgry_character(var)
    var$intrvl  <- "string"
  } else if (class(var$data) == "factor") {
    var$sumStat <- stat_factor(var)
    var$catgry  <- catgry_factor(var)
    var$intrvl  <- "factor"
  }

  if (keep_data == FALSE)
  { 
    var$data     <- NA
    var$missings <- NA
  }
   
  return(var)
}


