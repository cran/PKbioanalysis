skip_if_no_py()
test_that("smoothing_testing", {
  # path <- system.file("extdata", "waters_raw_ex", package="PKChromaMetrics")
  # main <- read_chrom(path)
  main <- smooth_chrom(main, filter = "mean", window = 2, iter = 3)

  is_smoothed(main)

  expect_set_equal(is_smoothed(main)$smoothed, TRUE)
})
