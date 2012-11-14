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
ddiExtractor.extract_ddiVar = function(var, keep_data, file_format=NA) {

  ##### INTERNAL FUNCTIONS #####

  # Calculate sumStat for numeric variables
  stat_numeric = function(var) {
    summary = summary(var$data)
    sumStat = list()
    for(i in 1:length(summary)) {
      sumStat[ names(summary)[i] ] = summary[i]
    }   
    sumStat$valid = length(var$data[!is.na(var$data)])
    sumStat$invalid = length(var$data[is.na(var$data)])
    return(sumStat)
  }

  stat_labeled_numeric = function(var){
    sumStat = list()
    sumStat$valid = length(var$data[!is.na(var$data)])
    sumStat$invalid = length(var$data[is.na(var$data)])
    return(sumStat)
  }

  stat_character = function(var) {
    sumStat = list()
    sumStat$valid = length(var$data[!is.na(var$data)])
    sumStat$invalid = length(var$data[is.na(var$data)])
  }

  stat_factor = function(var) {
    return("stat_factor")
  }

  catgry_labeled_numeric = function(var) {
    tab = table(var$data)
    catgry = list()
    for(i in 1:length(tab)) {
      cat = list()
      cat$catVal = names(t[1])
      cat$labl = names(var$values)[var$values == i]
      cat$valid = TRUE
      cat$freq = tab[[i]]
      catgry[[i]] = cat
    }
    return(catgry)
  }

  catgry_factor = function(var) {
    return("catgry_factor")
  }

  ##### START #####

  if(class(var$data) == "numeric") {
    if(is.null(var$val_labels)){
      var$sumStat = stat_numeric(var)
    } else {
      var$sumStat = stat_labeled_numeric(var)
      var$catgry = catgry_labeled_numeric(var)
    }
  } else if(class(var$data) == "character") {
    var$sumStat = stat_character(var)
  } else if(class(var$data) == "factor") {
    var$sumStat = stat_factor(var)
    var$catgry = catgry_factor(var)
  }

  if( keep_data == FALSE ) { 
    var$data = NA
    var$missings = NA
  }   
  return(var)
}


