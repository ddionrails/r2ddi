
library("r2ddi")

## Params
filename      = "test/test1.dta"
data_name     = "stata_data"
data_label    = "Stata Test Data"
missing_codes = NULL
keep_data     = TRUE

## Load data and create ddi object
test1a <-
  stata_import(
    filename,
    data_name,
    data_label,
    missing_codes,
    keep_data )

## Export ddi object to xml
ddi2xml(test1a, "tmp/test1a.xml")

## Export ddi object to json
ddi2json(test1a, "tmp/test1a.json")
