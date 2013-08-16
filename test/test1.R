
library("r2ddi")

## Params
filename      = "test/test1.dta"
data_name     = "stata_data"
data_label    = "Stata Test Data"
missing_codes = NULL
keep_data     = TRUE

## Load data and create ddi object
test1 <-
  stata2ddi(
    filename = filename,
    data_name = data_name,
    data_label = data_label,
    missing_codes = missing_codes,
    keep_data = keep_data )

## Export ddi object to xml
ddi2xml(test1, "tmp/test1.xml")

## Export ddi object to json
#ddi2json(test1, "tmp/test1.json")
