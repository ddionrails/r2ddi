# New XML function

ddi2xml <- function(ddi, filename){

  renderSumStat <- function(i, sumStats, dataDscrNode) {
    sumStatNode <-
      newXMLNode(
        "sumStat",
        sumStats[[i]],
        parent=dataDscrNode,
        attrs=c(type=names(sumStats)[i]))
  }

  renderCatgry <- function(catgry, dataDscrNode) {
    catgryNode <-
      newXMLNode(
        "catgry",
        parent=dataDscrNode)
    if(catgry$valid == TRUE)
      addAttributes(catgryNode, missing=FALSE)
    if(catgry$valid == FALSE)
      addAttributes(catgryNode, missing=TRUE)
    newXMLNode("catValu", catgry$value, parent=catgryNode)
    newXMLNode("labl",    catgry$label, parent=catgryNode)
    newXMLNode("catStat", catgry$freq,  parent=catgryNode, attrs=c(type="freq"))
  }

  renderVar <- function(varDscr, filename, codebook) {
    dataDscrNode <-
      newXMLNode(
        "var",
        parent=codebook["dataDscr"],
        attrs=c(
          name=varDscr$name,
          files=filename))
    lapply(seq_along(varDscr$sumStat), renderSumStat, varDscr$sumStat, dataDscrNode)
    lapply(varDscr$catgry,  renderCatgry,  dataDscrNode)
  }

  renderFileDscr <- function(fileDscr, codebook) {
    fileDscrNode <-
      newXMLNode(
        "fileDscr",
        parent=codebook,
        attrs=c(name=fileDscr$name))
    lapply(fileDscr$varDscr, renderVar, fileDscr$name, codebook)
  }

  ##### START #####

  codebook <- newXMLNode("codebook")
  dataDscr <- newXMLNode("dataDscr", parent=codebook)

  lapply(ddi$fileDscr, renderFileDscr, codebook)

  saveXML(codebook, file=filename)
}
