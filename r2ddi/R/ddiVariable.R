
#
# example()
#
# Returns example of a ddiVariable object.
#
ddiVariable.example = function() {

  # Generate example object and assign class
  var = list()
  class(var) = "ddiVariable"

  # Add example data
  var$data = c(1, 2, 1, 4, 2, 1, 4, 3)
  attr(var$data, "variableName") = "var_name"

  # Return ddiVariable object
  return(var)
}

#
# calculate()
#
# Main function for calculating statistics of a ddiVariable.
#
ddiVariable.calculate = function(var) {
  if(class(var$data) == "numeric") {
    return(ddiVariable.calculate_numeric (var))
  }
}

#
# calculate_numeric()
#
# Calculate statistics for a numeric ddiVariable
#
ddiVariable.calculate_numeric = function(var) {
  summary = summary(var$data)
  var$varStat = list()
  for(i in 1:length(summary)) {
    var$varStat[ names(summary)[i] ] = summary[i]
  }
  return(var)
}
