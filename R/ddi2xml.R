#' New XML function
#'
#' @method ddi2xml
#' @export ddi2xml
ddi2xml <- function(ddi,
                    filename,
                    include_string_catgry = FALSE)
{

  main <- function() {
    codebook <- newXMLNode("codebook")
    dataDscr <- newXMLNode("dataDscr", parent = codebook)
    lapply(ddi$file_dscr, .renderFileDscr, codebook)
    saveXML(codebook, file = filename)
  }

  .renderFileDscr <- function(file_dscr, codebook) {
    fileDscrNode <- newXMLNode("fileDscr",
                               parent = codebook,
                               attrs  = c(ID = file_dscr$id))
    lapply(file_dscr$data_dscr, .renderVar, file_dscr$id, codebook)
  }

  .renderVar <- function(data_dscr, filename, codebook) {
    attrs <- c(ID      = data_dscr$id,
               intrvl  = data_dscr$intrvl,
               files   = filename)
    varNode <- newXMLNode("var",
                          parent = codebook["dataDscr"],
                          attrs  = attrs)
    newXMLNode("labl", data_dscr$labl, parent = varNode)
    if(exists("jstat", where = data_dscr))
      lapply(names(data_dscr$jstat), function(x) {
        .renderJstat(x, data_dscr$jstat[[x]], varNode)
      })
    lapply(seq_along(data_dscr$sumStat), .renderSumStat, data_dscr$sumStat, varNode)
    if(include_string_catgry | data_dscr$intrvl != "string")
      if(exists("value_table", where = data_dscr))
        if(nrow(data_dscr$value_table) > 0)
          apply(data_dscr$value_table, 1, .renderCatgry, varNode)
  }

  .renderJstat <- function(name, content, parent_node) {
    jstatNode <- newXMLNode(
      "jstat",
      toJSON(content),
      parent = parent_node,
      attrs  = c(name = name))
  }

  .renderSumStat <- function(i, sumStats, varNode) {
    sumStatNode <- newXMLNode("sumStat",
                              sumStats[[i]],
                              parent = varNode,
                              attrs  = c(type = names(sumStats)[i]))
  }

  .renderCatgry <- function(value, varNode) {
    catgryNode <- newXMLNode("catgry", parent = varNode)
    addAttributes(catgryNode, missing = value["valid"])
    newXMLNode("catValu", value["value"], parent = catgryNode)
    if(!is.na(value["label"]))
      newXMLNode("labl", value["label"], parent = catgryNode)
    newXMLNode("catStat", value["freq"], parent = catgryNode, attrs = c(type = "freq"))
  }

  main()
}
