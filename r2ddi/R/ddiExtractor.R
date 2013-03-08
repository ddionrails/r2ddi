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

  catgry_labeled_numeric <- function(var)
  {
    valid_tab   <- valid_labels   <- table(var$data_frame$valid)
    missing_tab <- missing_labels <-table(var$data_frame$missing)
    attributes(valid_tab) <- attributes(missing_tab) <- NULL

    if(length(valid_tab) > 0)
      valid_freq <-
        data.frame(
          value            = names(valid_labels),
          freq             = valid_tab,
          valid            = TRUE,
          stringsAsFactors = FALSE)
    else
      valid_freq <- NULL

    if(length(missing_tab) > 0)
      missing_freq <-
        data.frame(
          value            = names(missing_labels),
          freq             = missing_tab,
          valid            = FALSE,
          stringsAsFactors = FALSE)
    else
      missing_freq <- NULL

    if(!is.null(valid_freq) & !is.null(missing_freq))
      freq <- rbind(valid_freq, missing_freq)
    else if(!is.null(valid_freq))
      freq <- valid_freq
    else if (!is.null(missing_freq))
      freq <- missing_freq
    else
      freq <- NULL

    xx <-
      merge(
        var$value_frame,
        freq,
        by  = "value",
        all = TRUE)

    xx$valid <- ifelse(is.na(xx$valid.x), xx$valid.y, xx$valid.x)
    xx$freq  <- ifelse(is.na(xx$freq),    0,          xx$freq   )

    xx$valid.y <- xx$valid.x <- NULL

    browser()

    catgry <- list()

if(FALSE)
{
    if(length(valid_tab) > 0)
    {
      for(i in 1:length(valid_tab))
        catgry[[names(valid_tab)[i] ]] <-
          list(
            value   = names(valid_tab[i]),
            labl    = var$value_frame$label[
                        var$value_frame$value == names(valid_tab)[i] ],
            valid   = TRUE,
            freq    = valid_tab[[i]])
    }

    if(length(missing_tab) > 0)
    {
      for(i in 1:length(missing_tab))
        catgry[[names(missing_tab)[i] ]] <-
          list(
            value   = names(missing_tab[i]),
            labl    = var$value_frame$label[
                        var$value_frame$value == names(missing_tab)[i] ],
            valid   = TRUE,
            freq    = missing_tab[[i]])
    }
}
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
      var$intrvl  <- "labeled numeric"
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


