context("DDI Variable")

test_that(
  "ddi_variable() creates a new object",
  {
    x <- ddi_variable("varname")
    expect_that(x$id, equals("varname"))
    expect_that(class(x), equals("ddi_variable"))
  })
