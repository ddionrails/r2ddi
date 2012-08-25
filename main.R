#
# Libraries:
# * foreign for reading SPSS and Stata data files
# * XML for generating the DDI file
#
library("foreign")
library("XML")

#
# listToXML transforms a list to xml
#
listToXML <- function(node, sublist){
  for(i in 1:length(sublist)){
    child <- newXMLNode(names(sublist)[i], parent=node);
    if (typeof(sublist[[i]]) == "list"){
        listToXML(child, sublist[[i]])
    }
    else{
      xmlValue(child) <- sublist[[i]]
    }
  } 
}

#
# r2ddi is the main function
#
r2ddi = function(filename="data/test.csv") {
  #
  # Get the file format from the file extension using regexpr
  # *.csv => CSV
  # *.sav => SPSS
  # *.dta => Stata
  #
  if (regexpr(filename, pattern="csv$", ignore.case=TRUE) != -1){
    cat("[INFO] Input format: CSV\n")
    csv_list = to.list(read.csv(filename, stringsAsFactors=FALSE))
    varlist = csv_to_varlist(csv_list)
  } else if (regexpr(filename, pattern="sav$", ignore.case=TRUE) != -1){
    cat("[INFO] Input format: SPSS (*.sav)\n")
  } else if (regexpr(filename, pattern="dta$", ignore.case=TRUE) != -1){
    cat("[INFO] Input format: Stata (*.dta)\n")
  } else {
    cat("[ERROR] Don't know the file extension\n")
  }
}

#
# Function for lapply on CSV data
#
handle_csv_var = function(input){
  if (typeof(input) == "integer") {
    var = newXMLNode("var")
    addAttributes(var, "nature" = "interval")
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "min"), min(input, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "max"), max(input, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "valid"), sum(table(input)) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "invalid"), length(input) - sum(table(input)) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "mean"), mean(input, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "sd"), sd(input, na.rm=TRUE) ))
    return(var)
  } else if (typeof(input) == "character") {
    var = newXMLNode("var")
    addAttributes(var, "nature" = "interval")
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "valid"), sum(table(input)) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "invalid"), length(input) - sum(table(input)) ))
    t = table(input)
    for(i in 1:length(t)) {
      addChildren(var, newXMLNode("catgry",
        newXMLNode("catValu", dimnames(t)[[c(1, i)]]),
        newXMLNode("catStat", t[[i]]) ))
    }
    return(var)
  } else {
  }
  return(var)
}

