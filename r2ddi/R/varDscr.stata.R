
varDscr.stata <- function(i, var, attr) {
  varDscr = list(
    name = attr$names[[i]],
    label = attr$var.labels[[i]],
    data = var,
    format = attr$formats[[i]])
  return(varDscr)
}
