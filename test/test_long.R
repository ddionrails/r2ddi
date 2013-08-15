
library("r2ddi")

## Params
filename      = "test/test_long.dta"
data_name     = "stata_data"
data_label    = "Stata Test Data"
missing_codes = NULL
keep_data     = TRUE
time_id       = "wave"

## Load data and create ddi object
test_long <-
  stata2ddi(
    filename = filename,
    data_name = data_name,
    data_label = data_label,
    missing_codes = missing_codes,
    time_id = time_id,
    keep_data = keep_data )

## Export ddi object to xml
ddi2xml(test_long, "tmp/test_long.xml")

## Export ddi object to json
#ddi2json(test_long, "tmp/test_long.json")
