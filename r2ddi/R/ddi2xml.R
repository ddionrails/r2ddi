#
# ddi2xml(ddi, filename, version)
#
# Write ddi-object to an xml-file.
#
# Params:
# * ddi: ddi-object (input)
# * filename: Output-file
# * version: DDI-version (default = 2.5)
#
ddi2xml = function(ddi, filename, version="2.5") {


  ##### INTERNAL FUNCTIONS #####

  #
  # xml2(ddi)
  #
  # Produce xml content for DDI 2.5
  #
  xml2 = function(ddi) {

    # Root element
    content = newXMLNode("codebook")

    # Add dataDscr-node, because it's not possible within the loop
    addChildren(content, newXMLNode("dataDscr"))

    # Loop for all (data-)files
    for( filename in names(ddi[['fileDscr']]) ) {
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

      # Loop for variables
      for( var in ddi[['fileDscr']][[filename]][['varDscr']] ) {
        xmlVar =
          newXMLNode(
            "var",
            attrs = c("ID" = var$name, "files" = filename) )

        # Within the variable-loop:
        # -> Loop for summary statistics
        for ( stat in names(var$sumStat) ) {
          addChildren(
            xmlVar,
            newXMLNode(
              "sumStat",
              attrs = c("type"=stat),
              var$sumStat[[stat]] ))
        }

        # Add variable node to root
        addChildren(content[["dataDscr"]], xmlVar)
      }
    }

    # Return
    return(content)
  }

  ##### LIBRARIES #####

  library('XML')

  ##### START #####

  if ( version == "2.5" ) {

    # Generate xml-document-object
    doc = newXMLDoc()

    # Add content to xml-document-object,
    # calling the function for DDI 2.5
    addChildren(doc, xml2(ddi))

    # Write xml-document-object to xml-file
    saveXML(doc, file = filename)
  }
}
