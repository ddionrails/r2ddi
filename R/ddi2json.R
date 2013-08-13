#' Convert and write DDI object to JSON
#'
#' @param ddi DDI object (input)
#' @param filename File (output)
#' @export
ddi2json <-
  function(ddi, filename)
{
  json <- toJSON(ddi)
  write(json, filename)
}
