#' Attach file_dscr from another DDI object
#'
#' @param master This is the main object, which will be extended.
#' @param attachment This object will be attached to the master object.
#' @export
attachFileDscr <-
  function(master, attachment)
{

  # TODO: It would be much easier to use a simple c().

  file_list <- names(attachment$file_dscr)

  for( file in file_list )
  {
    master$file_dscr[[file]] <- attachment$file_dscr[[file]]
  }

  return(master)
}
