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
r2ddi = function(input_file="data/test.csv", output_file=NA, mc=FALSE, csv.sep=",") {
  #
  # Get the file format from the file extension using regexpr
  # *.csv => CSV
  # *.sav => SPSS
  # *.dta => Stata
  #
  if (is.na(output_file)) output_file = paste(input_file, ".xml", sep="")
  if (regexpr(input_file, pattern="csv$", ignore.case=TRUE) != -1){
    cat("[INFO] Input format: CSV\n[INFO] Read input file... ")
    csv_list = as.list(read.csv(input_file, stringsAsFactors=FALSE, sep=csv.sep))
    cat("[DONE]\n[INFO] Run lapply... ")
    varlist = lapply(csv_list, handle_csv)
    cat("[DONE]\n")
    for(i in names(varlist)) addAttributes(varlist[[`i`]], "name"=i)
  } else if (regexpr(input_file, pattern="sav$", ignore.case=TRUE) != -1){
    cat("[INFO] Input format: SPSS (*.sav)\n")
    cat("[INFO] Read input file... ")
    spss_list = read.spss(input_file, to.data.frame=FALSE, use.value.labels=FALSE)
    cat("[DONE]\n")
    if(mc){
      library("multicore")
      cat("[INFO] Run mclapply... ")
      varlist = mclapply(spss_list, handle_spss)
      cat("[DONE]\n")
    } else {
      cat("[INFO] Run lapply... ")
      varlist = lapply(spss_list, handle_spss)
      cat("[DONE]\n")
    }
    #for(i in names(varlist)) addAttributes(varlist[[`i`]], "name"=i)
  } else if (regexpr(input_file, pattern="dta$", ignore.case=TRUE) != -1){
    cat("[INFO] Input format: Stata (*.dta)\n")
    cat("[INFO] Read input file... ")
    input = read.dta(input_file, convert.factors=FALSE, convert.dates=FALSE, missing.type=TRUE)
    cat("[DONE]\n")
    stata_list = rep(list(NA), length(names(input)))
    names(stata_list) = names(input)
    j = 1
    for(i in names(input)) {
      stata_list[[`i`]] = list(name = names(input)[j],
        data = input[`i`],
        labl = attr(input, "var.labels")[j],
        catgry = attr(input, "label.table")[attr(input, "val.labels")[j]] )
      j = j + 1
    }
    varlist = lapply(stata_list, handle_stata)
  } else {
    cat("[ERROR] Don't know the file extension\n")
  }
  doc = newXMLDoc()
  content = newXMLNode("codebook")
  doc = addChildren(doc, addChildren(content, varlist))
  saveXML(doc, file=output_file)
}

#
# Function for lapply on CSV data
#
handle_csv = function(input){
  if (class(input) == "numeric" || class(input) == "integer") {
    var = newXMLNode("var")
    addAttributes(var, "nature" = "interval")
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "min"), min(input, na.rm=TRUE)))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "max"), max(input, na.rm=TRUE)))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "valid"), sum(table(input))))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "invalid"), length(input) - sum(table(input))))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "mean"), mean(input, na.rm=TRUE)))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "sd"), sd(input, na.rm=TRUE)))
    return(var)
  } else if (class(input) == "character") {
    var = newXMLNode("var")
    addAttributes(var, "nature" = "nominal/ordinal")
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "valid"), sum(table(input))))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "invalid"), length(input) - sum(table(input))))
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

#
# Function for lapply on SPSS data
#
handle_spss = function(input) {
  if (is.null(attr(input, "value.labels")) && (class(input) == "numeric" || class(input) == "integer")) {
    var = newXMLNode("var")
    addAttributes(var, "nature"="intervall")
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "min"), min(input, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "max"), max(input, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "valid"), sum(table(input)) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "invalid"), length(input) - sum(table(input)) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "mean"), mean(input, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "sd"), sd(input, na.rm=TRUE) ))
    return(var)
  } else if (!is.null(attr(input, "value.labels"))) {
    var = newXMLNode("var")
    addAttributes(var, "nature" = "nominal/ordinal")
    t = table(input)
    for(i in names(t)) {
      addChildren(var, newXMLNode("catgry",
        newXMLNode("labl", names(attr(input, "value.labels"))[attr(input, "value.labels") == i]),
        newXMLNode("catValu", i),
        newXMLNode("catStat", t[names(t) == i]) ))
    }
    return(var)
  } else {
    var = newXMLNode("var")
    return(var)
  }
}

#
# Function for lapply on Stata data
#
handle_stata = function(input) {
  data = input$data[[input$name]]
  if(input$catgry == "NULL" && typeof(data) == "integer") {
    var = newXMLNode("var")
    addAttributes(var, "nature"="intervall")
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "min"), min(data, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "max"), max(data, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "valid"), sum(table(data)) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "invalid"), length(data) - sum(table(data)) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "mean"), mean(data, na.rm=TRUE) ))
    addChildren(var, newXMLNode("sumStat", attrs=c("type" = "sd"), sd(data, na.rm=TRUE) ))
    return(var)
  } else if(input$catgry != "NULL") {
    var = newXMLNode("var")
    addAttributes(var, "nature" = "nominal/ordinal")
    t = table(data)
    return(var)
  } else {
    var = newXMLNode("var")
    return(var)
  }
}

