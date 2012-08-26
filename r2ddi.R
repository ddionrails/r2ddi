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
    csv_list = as.list(read.csv(filename, stringsAsFactors=FALSE))
    varlist = lapply(csv_list, handle_csv)
    for(i in names(varlist)) addAttributes(varlist[[`i`]], "name"=i)
  } else if (regexpr(filename, pattern="sav$", ignore.case=TRUE) != -1){
    cat("[INFO] Input format: SPSS (*.sav)\n")
    spss_list = read.spss(filename, to.data.frame=FALSE)
  } else if (regexpr(filename, pattern="dta$", ignore.case=TRUE) != -1){
    cat("[INFO] Input format: Stata (*.dta)\n")
  } else {
    cat("[ERROR] Don't know the file extension\n")
  }
  doc = newXMLDoc()
  content = newXMLNode("codebook")
  doc = addChildren(doc, addChildren(content, varlist))
  doc
}

#
# Function for lapply on CSV data
#
handle_csv = function(input){
  if (class(input) == "numeric" || class(input) == "integer") {
    var = newXMLNode("var")
    addAttributes(var, "nature" = "interval")
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "min"), min(input, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "max"), max(input, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "valid"), sum(table(input)) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "invalid"), length(input) - sum(table(input)) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "mean"), mean(input, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "sd"), sd(input, na.rm=TRUE) ))
    return(var)
  } else if (class(input) == "character") {
    var = newXMLNode("var")
    addAttributes(var, "nature" = "nominal/ordinal")
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
    var = newXMLNode("var")
    return(var)
  }
  return(var)
}

