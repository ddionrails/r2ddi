#' New XML function
#'
#' @method ddi2xml
#' @export ddi2xml
ddi2xml <- function(ddi, filename)
{

  renderFileDscr <- function(file_dscr, codebook)
  {
    fileDscrNode <-
      newXMLNode(
        "fileDscr",
        parent = codebook,
        attrs  = c(ID = file_dscr$name))
    lapply(file_dscr$var_dscr, renderVar, file_dscr$name, codebook)
  }

  renderVar <- function(var_dscr, filename, codebook)
  {
    varNode <-
      newXMLNode(
        "var",
        parent = codebook["dataDscr"],
        attrs  = c(
          ID      = var_dscr$name,
          intrvl  = var_dscr$intrvl,
          files   = filename))
      newXMLNode("labl", var_dscr$label, parent=varNode)
    lapply(seq_along(var_dscr$sumStat), renderSumStat, var_dscr$sumStat, varNode)
    if(exists("value_table", where = var_dscr))
      if(nrow(var_dscr$value_table) > 0)
        apply(var_dscr$value_table, 1, renderCatgry, varNode)
  }

  renderSumStat <- function(i, sumStats, varNode)
  {
    sumStatNode <-
      newXMLNode(
        "sumStat",
        sumStats[[i]],
        parent = varNode,
        attrs  = c(type = names(sumStats)[i]))
  }

  renderCatgry <- function(value, varNode)
  {
    catgryNode <-
      newXMLNode(
        "catgry",
        parent = varNode)
    addAttributes(catgryNode, missing=value["valid"])
    newXMLNode("catValu", value["value"], parent = catgryNode)
    if(!is.na(value["label"]))
      newXMLNode("labl", value["label"], parent = catgryNode)
    newXMLNode("catStat", value["freq"],  parent = catgryNode, attrs = c(type = "freq"))
  }

  ##### START #####

  codebook <- newXMLNode("codebook")
  dataDscr <- newXMLNode("dataDscr", parent = codebook)

  lapply(ddi$file_dscr, renderFileDscr, codebook)

  saveXML(codebook, file = filename)
}
