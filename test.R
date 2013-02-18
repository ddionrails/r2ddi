#source("source.R", echo=TRUE)

library("r2ddi")

filename="r2ddi/data/test.dta"
data_name="stata_data"
data_label="Stata Test Data"
missing_codes=NULL
keep_data=TRUE

test1 <-
  stata2ddi(
    filename,
    data_name,
    data_label,
    missing_codes,
    keep_data )

#ddi2xml(test1, "test/test1.xml")


#test2 <-
#  csv2ddi(
#    "r2ddi/data/test.csv",
#    "csv_data",
#    "CSV Test Data",
#    keep_data=TRUE )
#
#ddi2xml(test2, "test/test2.xml")
