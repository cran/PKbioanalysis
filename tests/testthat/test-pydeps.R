test_that("pydeps", {
  skip_if_no_py()
  skip_on_cran()
  expect_true(reticulate::py_module_available("numpy"))
  expect_true(reticulate::py_module_available("scipy"))
  expect_true(reticulate::py_module_available("pandas"))
  expect_true(reticulate::py_module_available("rainbow"))
  expect_true(reticulate::py_module_available("src"))
})