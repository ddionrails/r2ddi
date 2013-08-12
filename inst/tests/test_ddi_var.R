context("Variable")

test_that(
  "var works",
  {
    x <- ddi_var("sex", "Gender")
    expect_that(x$id, equals("sex"))
    expect_that(x$labl, equals("Gender"))
  })
