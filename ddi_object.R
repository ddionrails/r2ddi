
new.ddi = function() {
  ddi = list(
    docDscr  = list(),
    stdyDscr = list(),
    fileDscr = list(
      file_plong = list(
        name    = "file1",
        varDscr = list(
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

print.ddi = function(ddi) {
  result =
    lapply(
      ddi$fileDscr,
      function(x) {
        x$name
      } )
  return(result)
}

