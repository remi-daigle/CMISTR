require(testthat)

test_that("simScore returns a vector",{
  expect_equal(class(simScore(risk=2,certainty=1)), "integer")
})
