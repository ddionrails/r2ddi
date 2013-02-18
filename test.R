#source("source.R", echo=TRUE)

test1 <-
  stata2ddi(
    "r2ddi/data/test.dta",
    "stata_data",
    "Stata Test Data",
    keep_data=TRUE )

ddi2xml(test1, "test/test1.xml")


#test2 <-
#  csv2ddi(
#    "r2ddi/data/test.csv",
#    "csv_data",
#    "CSV Test Data",
#    keep_data=TRUE )
#
#ddi2xml(test2, "test/test2.xml")
