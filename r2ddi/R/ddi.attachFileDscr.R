#
# attachFileDscr
#
ddi.attachFileDscr = function( main, attachment ) {
  file_list = names( attachment$fileDscr )
  for( file in file_list ) {
    main$fileDscr[[file]] = attachment$fileDscr[[file]]
  }

  return( main )
}
