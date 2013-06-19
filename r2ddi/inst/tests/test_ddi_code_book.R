
context("Codebook")

test_that(
  "Codebook works",
  {
    x = ddi_code_book()
    expect_that(class(x), equals("ddi_code_book"))
  })
