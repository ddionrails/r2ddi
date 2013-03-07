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
    lapply(var_dscr$catgry,  renderCatgry,  varNode)
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

  renderCatgry <- function(catgry, varNode)
  {
    catgryNode <-
      newXMLNode(
        "catgry",
        parent=varNode)
    if (catgry$valid == TRUE)
      addAttributes(catgryNode, missing=FALSE)
    if (catgry$valid == FALSE)
      addAttributes(catgryNode, missing=TRUE)
    newXMLNode("catValu", catgry$value, parent = catgryNode)
    newXMLNode("labl",    catgry$labl,  parent = catgryNode)
    newXMLNode("catStat", catgry$freq,  parent = catgryNode, attrs = c(type = "freq"))
  }

  ##### START #####

  codebook <- newXMLNode("codebook")
  dataDscr <- newXMLNode("dataDscr", parent = codebook)

  lapply(ddi$file_dscr, renderFileDscr, codebook)

  saveXML(codebook, file = filename)
}
