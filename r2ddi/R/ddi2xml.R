ddi2xml = function(ddi, filename, version="2.5") {


  ##### INTERNAL FUNCTIONS #####

  xml2 = function(ddi) {
    content = newXMLNode("codebook")
    addChildren(content, newXMLNode("dataDscr"))
    for( filename in names(ddi$fileDscr) ) {
      addChildren(
        content,
        newXMLNode(
          "fileDscr",
           attrs = c("ID" = filename),
           newXMLNode(
             "fileTxt",
              newXMLNode(
                "fileName",
                 filename ))))
      for( var in ddi$fileDscr[[filename]][["varDscr"]] ) {
        xmlVar =
          newXMLNode(
            "var",
            attrs = c("ID" = var$name, "files" = filename) )
        for ( stat in names(var$sumStat) ) {
          addChildren(
            xmlVar,
            newXMLNode(
              "sumStat",
              attrs = c("type"=stat),
              var$sumStat[[stat]] ))
        }
        addChildren(content[["dataDscr"]], xmlVar)
      }
    }
    return(content)
  }

  ##### LIBRARIES #####

  library('XML')

  ##### START #####

  if ( version == "2.5" ) {
    content = xml2(ddi)
    doc = newXMLDoc()
    doc = addChildren(doc, content)
    saveXML(doc, file = filename)
  }
}
