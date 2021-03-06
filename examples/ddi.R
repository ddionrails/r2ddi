#
# ddi.example()
#
# Return example of DDI-list-structure.
#
ddi.example = function() {
  ddi = list(
    doc_dscr  = list(),
    stdy_dscr = list(),
    file_dscr = list(
      file_plong = list(
        name    = "file1",
        var_dscr = list(
          var_age = list(
            name    = "age",
            labl    = "Age in 2011",  
            nature  = "interval",
            valid   = c(12, 45, 22, 76, 52),
            invalid = c(NA, NA, NA, NA, NA),
            sumStat = list(
              min = 12,
              max = 76,
              valid = 5,
              invalid = 0
            )
          ),
          var_sex = list(
            name    = "sex",
            labl    = "Gender",
            nature  = "categorial",
            valid   = c(1, NA, 0, 1, 0),
            invalid = c(NA, 1, NA, NA, NA),
            sumStat = list(
              min = 0,
              max = 1,
              valid = 4,
              invalid = 1
            ),
            catgry = list(
              cat_1 = list(
                catValu = 0,
                labl    = "male",
                valid   = TRUE,
                catStat = list(
                  type   = "freq",
                  value  = 2
                )
              ),
              cat_2 = list(
                catValu = 1,
                labl    = "female",
                valid   = TRUE,
                catStat = list(
                  type   = "freq",
                  value  = 2
                )
              )
            )
          )
        )
      ),
      file_hlong = list()
    )
  )
  class(ddi) = "ddi"
  return(ddi)
}            

#
# print.ddi(ddi)
#
# DDI-object-specific function for printing a DDI-object.
#
print.ddi = function(ddi) {
  result =
    lapply(
      ddi$file_dscr,
      function(x) {
        x$name
      } )
  return(result)
}

#
# stata2ddi(filename)
#
# Returns file-element for given filename.
#
stata2ddi = function(filename) {

  library("foreign")

  # Read Stata file
  stata_file =
    read.dta(
      filename,
      convert.factors=FALSE,
      convert.dates=FALSE,
      missing.type=TRUE )

  # Generate scaleton for file-object.
  file = list()
  class(file) = "ddi.file"
  file$name = filename
  file$var_dscr = list()

  # Internal function: extract_metadata(stata_var)
  extract_metadata = function(stata_var) {
    var = list()
    var$raw = stata_var
    return(var)
  }

  file$var_dscr = lapply(stata_file, extract_metadata)

  return(file)

}


