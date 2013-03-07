
## Import Stata file
test <- stata2ddi("test.dta", "test_data", multicore=TRUE)

## Export ddi object to XML
ddi2xml(test, "text.xml")

