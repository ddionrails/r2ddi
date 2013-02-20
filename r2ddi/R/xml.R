
xml <- function(ddi, filename){

  renderVar <- function(varDscr, codebook) {
    dataDscrNode <-
      newXMLNode(
        "var",
        parent=codebook["dataDscr"],
        attrs=c(name=varDscr$name))
  }

  renderFileDscr <- function(fileDscr, codebook) {
    fileDscrNode <-
      newXMLNode(
        "fileDscr",
        parent=codebook,
        attrs=c(name=fileDscr$name))
    lapply(fileDscr$varDscr, renderVar, codebook)
  }

  codebook <- newXMLNode("codebook")
  dataDscr <- newXMLNode("dataDscr", parent=codebook)

  lapply(ddi$fileDscr, renderFileDscr, codebook)

  return(codebook)
}
