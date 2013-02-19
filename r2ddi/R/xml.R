
xml <- function(ddi, filename){

  renderFileDscr <- function(file, codebook=codebook) {
    fileNode <-
      newXMLNode(
        "fileDscr",
        parent=codebook,
        attrs=c(name=file$name))
  }

  codebook <- newXMLNode("codebook")
  dataDscr <- newXMLNode("dataDscr", parent=codebook)

  lapply(ddi$fileDscr, renderFileDscr, codebook)

  return(codebook)
}
