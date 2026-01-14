test_that("precision_test", {
  cv(c(1, 1, 1)) |> expect_equal(0)
  cv(c(1, 1, 1)) |> expect_equal(0)
})
