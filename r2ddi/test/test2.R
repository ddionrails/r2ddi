
library("r2ddi")

## Params
filename      = "test/test2.dta"
data_name     = "stata_data"
data_label    = "Stata Test Data"
missing_codes = NULL
keep_data     = TRUE

## Load data and create ddi object
test2 <-
  stata2ddi(
    filename,
    data_name,
    data_label,
    missing_codes,
    keep_data )

## Export ddi object to xml
ddi2xml(test2, "tmp/test2.xml")

