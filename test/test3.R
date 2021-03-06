
library("r2ddi")

## Params
filename      = "test/test3.dta"
data_name     = "stata_data"
data_label    = "Stata Test Data"
missing_codes = c(-1, -2, -3)
keep_data     = TRUE

## Load data and create ddi object
test3 <-
  stata2ddi(
    filename = filename,
    data_name = data_name,
    data_label = data_label,
    missing_codes = missing_codes,
    keep_data = keep_data )

## Export ddi object to xml
ddi2xml(test3, "tmp/test3.xml")

