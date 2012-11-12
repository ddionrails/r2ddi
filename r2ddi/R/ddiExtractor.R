
ddiExtractor.extract_ddiVar = function(var, keep_data) {

  ##### INTERNAL FUNCTIONS #####

  # Calculate sumStat for numeric variables
  stat_numeric = function(var) {
    summary = summary(var$data)
    sumStat = list()
    for(i in 1:length(summary)) {
      sumStat[ names(summary)[i] ] = summary[i]
    }   
    return(sumStat)
  }

  stat_labeled_numeric = function(var){
    return("stat_labeld_numeric")
  }

  stat_character = function(var) {
    return("stat_character")
  }

  stat_factor = function(var) {
    return("stat_factor")
  }

  catgry_labeled_numeric = function(var) {
    return("catgry_labeled_numeric")
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


