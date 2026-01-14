test_that(".which_transition", {
  .which_transition(main, 1) |> expect_equal(1)
  .which_transition(main, 2) |> expect_equal(2)
  .which_transition(main, 33) |> expect_error("Compound ID not found")
})


test_that("filter_peak", {
  x <- .filter_peak(
    main,
    transition_id = 1,
    samples_ids = c(1, 2),
    peak_start = 0.5,
    peak_end = 1.5,
    smoothed = FALSE
  )
  x |> expect_s3_class("data.frame") |> ncol() |> expect_equal(3)
  colnames(x) |> expect_equal(c("RT", "T1", "sample_id"))
  x$sample_id |> unique() |> expect_equal(c("1", "2"))

  .filter_peak(
    main_nosmooth,
    transition_id = 1,
    samples_ids = c(1, 2),
    peak_start = 0.5,
    peak_end = 1.5,
    smoothed = TRUE
  ) |>
    expect_error(
      "Chromatogram not smoothed. Please smooth the chromatogram first."
    )
})

test_that("test integration status", {
  x <- update_peak_area(
    main,
    compound_id = 6,
    sample_id = 20,
    area = 1234,
    observed_rt = 1,
    observed_peak_start = 0.1,
    observed_peak_end = 0.2,
    observed_peak_height = 100
  ) |>
    expect_error("Sample ID does not exist")

  x <- update_peak_area(
    main,
    compound_id = 6,
    sample_id = 5,
    area = 1234,
    observed_rt = 1,
    observed_peak_start = 0.1,
    observed_peak_end = 0.2,
    observed_peak_height = 100
  )

  is_integrated(x, sample_id = 5, compound_id = 6) |> expect_true()
})


test_that("integrate_all", {
  is_integrated(main, sample_id = 6, compound_id = 20) |> expect_false()
  x <- .integrate_all_slack(
    main,
    compound_id = 20,
    peak_start = 0.1,
    peak_end = 1,
    manual = TRUE
  )

  is_integrated(x, sample_id = 6, compound_id = 20) |> expect_true()

  plot_chrom(x, sample_id = 4) |> expect_no_error()
  plot_chrom(x, sample_id = 4, integrated = T, show_RT = T) |> expect_no_error()
})


test_that("set_expected_bounds", {})


test_that("set_observed_bounds", {})


test_that("extract_peak_bounds", {
  bounds <- extract_peak_bounds(main, compound_id = 1)
  bounds |> expect_type("list")
  bounds$min |> expect_type("double")
  bounds$max |> expect_type("double")
  bounds$min |> expect_equal(0.5)
  bounds$max |> expect_equal(1.5)

  extract_peak_bounds(main, compound_id = 2) |>
    expect_error("No observed RT values found for the specified compound_id.")
})


test_that("integrate function", {
  integrate(main, 1, NULL)
})


test_that("small_peak_filter", {
  # test small peak filter

  main2 <- update_RT(
    main,
    1,
    peak_start = 1,
    peak_end = 2,
    target = "all",
    manual = F
  )
  apply_area_cutoff(main2, 10**3, 1)

  main2 <- update_RT(
    main,
    "C2",
    peak_start = 1,
    peak_end = 2,
    target = "all",
    manual = F
  )
  apply_area_cutoff(main2, 10**3, "C2")
})
