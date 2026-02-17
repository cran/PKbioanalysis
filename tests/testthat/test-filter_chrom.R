skip_if_no_py()

test_that("filtering_works_2way", {
  # expect_no_error(print(main))

  res <- filter_chrom(
    main,
    transitions_ids = c(18, 19, 20),
    samples_id = c(3, 4)
  )

  filter_chrom(main, transitions_ids = c(10:20), samples_id = c(4, 5, 10)) |> # sample id does not exit
    expect_error("Sample ID not found")

  expect_no_error(print(res))

  lapply(res@runs$files, \(x) !is.null(x)) |>
    unlist() |>
    all() |>
    expect_true()

  expect_equal(names(res@runs$files), as.character(c(3, 4))) # samples
  expect_equal(ncol(res@runs$files[[1]][[1]]) - 1, 3) # transitions in chromatogram
  expect_equal(res@transitions$transition_id, c(18, 19, 20)) # transitions list
})

test_that("filtering_works_transition", {
  res <- filter_chrom(main, transitions_ids = c(18, 19, 20))

  lapply(res@runs$files, \(x) !is.null(x)) |>
    unlist() |>
    all() |>
    expect_true()

  expect_no_error(print(res))

  expect_equal(names(res@runs$files), names(main@runs$files)) # samples
  expect_equal(ncol(res@runs$files[[1]][[1]]) - 1, 3) # transitions in chromatogram
  expect_equal(res@transitions$transition_id, c(18, 19, 20)) # transitions list
})


test_that("filtering_works_samples", {
  res <- filter_chrom(main, samples_id = c(3, 4))

  lapply(res@runs$files, \(x) !is.null(x)) |>
    unlist() |>
    all() |>
    expect_true()

  expect_no_error(print(res))
  expect_equal(names(res@runs$files), as.character(c(3, 4))) # samples
  expect_equal(ncol(res@runs[[1]][[1]]) - 1, ncol(main@runs[[1]][[1]]) - 1) # transitions in chromatogram
  expect_equal(res@transitions$transition_id, main@transitions$transition_id) # transitions list
})

test_that("filter_works with smoothing", {
  main <- smooth_chrom(main, filter = "mean", window = 2, iter = 3)
  res <- filter_chrom(
    main,
    transitions_ids = c(18, 19, 20),
    samples_id = c(3, 4)
  )

  expect_no_error(print(res))
  expect_equal(names(res@runs$files), as.character(c(3, 4))) # samples
  expect_equal(ncol(res@runs$files[[1]][[1]]) - 1, 3) # transitions in chromatogram
  expect_equal(ncol(res@runs$files[[1]]$smoothed) - 1, 3) # transitions in chromatogram

  expect_equal(res@transitions$transition_id, c(18, 19, 20)) # transitions list
})


test_that("filter_works_with_compounds", {
  res <- filter_chrom(main, cmpd_ids = c(1, 2, 3))

  expect_no_error(print(res))
  expect_equal(names(res@runs), names(main@runs)) # samples

  expect_equal(res@transitions$transition_id, main@transitions$transition_id) # transitions list

  expect_equal(res@compounds$compound_id, c(1, 2, 3)) # compounds list
})
