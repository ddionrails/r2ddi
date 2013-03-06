#' New XML function
#'
#' @method ddi2xml
#' @export ddi2xml
ddi2xml <- function(ddi, filename)
{

  renderFileDscr <- function(fileDscr, codebook)
  {
    fileDscrNode <-
      newXMLNode(
        "fileDscr",
        parent = codebook,
        attrs  = c(ID = fileDscr$name))
    lapply(fileDscr$varDscr, renderVar, fileDscr$name, codebook)
  }

  renderVar <- function(varDscr, filename, codebook)
  {
    varNode <-
      newXMLNode(
        "var",
        parent = codebook["dataDscr"],
        attrs  = c(
          ID      = varDscr$name,
          intrvl  = varDscr$intrvl,
          files   = filename))
      newXMLNode("labl", varDscr$label, parent=varNode)
    lapply(seq_along(varDscr$sumStat), renderSumStat, varDscr$sumStat, varNode)
    lapply(varDscr$catgry,  renderCatgry,  varNode)
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

  lapply(ddi$fileDscr, renderFileDscr, codebook)

  saveXML(codebook, file = filename)
}
