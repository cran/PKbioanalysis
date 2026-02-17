test_that("update_RT", {
  dat <- main
  # scenario 1: update RT for single sample id and single compound id
  expect_error(
    update_RT(
      dat,
      sample_id = 1,
      compound_id = 1,
      target = "single",
      force = FALSE,
      peak_start = 0.1,
      peak_end = 1
    ),
    "Sample ID not found"
  ) # invalid sample

  expect_error(
    update_RT(
      dat,
      sample_id = 5,
      compound_id = 110,
      target = "single",
      force = FALSE,
      peak_start = 0.1,
      peak_end = 1
    ),
    "Compound ID not found"
  ) # invalid cmpd

  # assert if sample_id is null, target is either all next or default.
  expect_error(
    update_RT(
      dat,
      sample_id = NULL,
      compound_id = 1,
      target = "single",
      force = FALSE,
      peak_start = 0.1,
      peak_end = 1
    ),
    "Sample ID not specified"
  )

  # dat <- add_compound(dat, "Epi", 10)

  ## helpers (check_integrated, check_expected_RT)
  # check if the compound is integrated
  lapply(4:6, \(x) is_integrated(dat, sample_id = x, compound_id = 20)) |>
    unlist() |>
    all() |>
    expect_false()

  expect_false(is_integrated(dat, sample_id = 4, compound_id = 20))

  # check expected RT
  expect_false(has_default_bounds(dat, compound_id = 20))

  expect_error(has_default_bounds(dat, compound_id = 23), "Compound ID not found")

  # start integration and setting default
  # update RT and integrate (update_RT)

  ##################################################

  #################################################
  expect_no_error(
    y <- update_RT(
      dat,
      sample_id = NULL,
      compound_id = "C1",
      target = "all",
      force = FALSE,
      mode = "auto",
      peak_start = 0.1,
      peak_end = 1
    )
  )

  # example of automatic integration with peak present
  (update_RT(
    y,
    sample_id = NULL,
    compound_id = "C1",
    target = "all",
    force = FALSE,
    mode = "auto",
    peak_start = 0.8,
    peak_end = 1
  )@peaks$observed_peak_start >=
    0.8) |>
    any() |>
    expect_true()
})

test_that("next_RT_update", {
  ##################################################
  has_default_bounds(main, compound_id = 1) |> expect_false()

  # test integrate_all_slack_next logic
  .integrate_next_slack(
    main,
    compound_id = 1,
    sample_id = "5",
    peak_start = 0.2,
    peak_end = 1.1,
    mode = "manual"
  ) |>
    expect_error("Expected RT not set")

  x <- .integrate_all_slack(
    main,
    compound_id = 1,
    peak_start = 0.5,
    peak_end = 1.4,
    mode = "manual"
  )

  x <- .integrate_next_slack(
    x,
    compound_id = 1,
    sample_id = 5,
    peak_start = 0.2,
    peak_end = 1.1,
    mode = "manual"
  )

  is_integrated(x, sample_id = 4, compound_id = 1) |> expect_true()
  is_integrated(main, sample_id = 4, compound_id = 1) |> expect_false()

  is_integrated(x, sample_id = 5, compound_id = 1) |> expect_true()
  is_integrated(x, sample_id = 6, compound_id = 1) |> expect_true()

  all(c(0.2, 0.5) %in% x@peaks$observed_peak_start) |> expect_true()
  all(c(1.1, 1.4) %in% x@peaks$observed_peak_end) |> expect_true()

  plot_chrom(x, sample_id = 5, integrated = T, show_RT = T) |> expect_no_error()
  plot_chrom(x, sample_id = 6, integrated = T, show_RT = T) |> expect_no_error()

  # all_next
  expect_error(
    update_RT(
      dat,
      sample_id = NULL,
      compound_id = 1,
      target = "all_next",
      force = FALSE,
      peak_start = 0.1,
      peak_end = 1,
      "Sample ID not specified"
    )
  )

  expect_no_error(
    z <- update_RT(
      y,
      sample_id = 5,
      compound_id = 1,
      target = "all_next",
      force = FALSE,
      mode = "manual",
      peak_start = 0.5,
      peak_end = 1.5
    )
  )

  expect_no_error(
    z <- update_RT(
      y,
      sample_id = 5,
      compound_id = 1,
      target = "all_next",
      force = FALSE,
      mode = "auto",
      peak_start = 0.5,
      peak_end = 1
    )
  )
})

test_that("individual_RT_update", {
  #################################################
  # test integrate_single logic
  x <- .integrate_individual_slack(
    main,
    compound_id = 1,
    sample_id = 4,
    peak_start = 0.1,
    peak_end = 1,
    mode = "auto"
  ) |>
    expect_error("Expected RT not set")

  x <- .integrate_individual_slack(
    x,
    compound_id = 1,
    sample_id = 4,
    peak_start = 0.3,
    peak_end = 1.2,
    mode = "manual"
  )

  is_integrated(x, sample_id = 4, compound_id = 1) |> expect_true()
  is_integrated(x, sample_id = 5, compound_id = 1) |> expect_true()
  is_integrated(x, sample_id = 6, compound_id = 1) |> expect_true()

  all(c(0.1, 0.3) %in% x@peaks$observed_peak_start) |> expect_true()
  all(c(1, 1.2) %in% x@peaks$observed_peak_end) |> expect_true()

  plot_chrom(x, sample_id = 5, integrated = T, show_RT = T) |> expect_no_error()

  ###############
  # single
  expect_error(
    y <- update_RT(
      dat,
      sample_id = NULL,
      compound_id = 1,
      target = "single",
      force = FALSE,
      peak_start = 0.1,
      peak_end = 1,
      "Sample ID not specified"
    )
  )

  expect_no_error(
    z <- update_RT(
      y,
      sample_id = 4,
      compound_id = 1,
      target = "single",
      force = FALSE,
      mode = "manual",
      peak_start = 0.2,
      peak_end = 1.2
    )
  )

  expect_no_error(
    z <- update_RT(
      y,
      sample_id = 4,
      compound_id = 1,
      target = "single",
      force = FALSE,
      mode = "auto",
      peak_start = 0.85,
      peak_end = 1
    )
  )

  # assert if force is F, observed RT is not overwritten.

  # expect_warning() # expected RT not found
  # expect_warning() # observed RT was found
  # expect_warning() # peakstart > peakend
  # expect_error() # no peak found in auto mode, force manual integration.

  # no peak logic
  expect_no_error(
    y <- update_RT(
      y,
      sample_id = NULL,
      compound_id = 1,
      target = "all",
      force = FALSE,
      mode = "auto",
      peak_start = 1,
      peak_end = 2
    )
  )

  # theshold works

  # TODO  equal 3 peak like targetlynx

  ## Peak with corrected baseline DAD4
})


test_that("filter_intensities", {
  dat <- filter_chrom(main, transitions_ids = c(10:20), samples_id = c(4, 5, 6))

  plot_chrom(dat, sample_id = 4)
  expect_error(
    .filter_peak(
      dat,
      samples_ids = 4,
      transition_id = "T1",
      peak_start = 0.1,
      peak_end = 1
    )
  )

  expect_no_error(
    .filter_peak(
      dat,
      samples_ids = 4,
      transition_id = "T10",
      peak_start = 0.1,
      peak_end = 1
    )
  )
  .filter_peak(
    dat,
    samples_ids = 4,
    transition_id = "T10",
    peak_start = 0.1,
    peak_end = 1
  ) |>
    .find_max_intensity() |>
    nrow() |>
    expect_equal(1)

  n_transitions <- dat@runs |> length()
  .filter_peak(
    dat,
    samples_ids = NULL,
    transition_id = "T10",
    peak_start = 0.1,
    peak_end = 1
  ) |>
    .find_max_intensity() |>
    nrow() |>
    expect_equal(n_transitions)
})
