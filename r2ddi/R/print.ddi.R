#

print.ddi <- function(ddi)
{

  cat("DDI object description\n")
  cat("----------------------\n")
  cat("List of datasets:\n")
  for(i in names(ddi$fileDscr))
  {
    cat("* ", i, sep="")
    cat(" (", length(ddi[["fileDscr"]][[i]][["varDscr"]]), " variables)\n", sep="")
  }

}
