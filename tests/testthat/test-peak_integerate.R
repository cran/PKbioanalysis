skip_if_no_py()

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
    mode = "manual"
  )

  is_integrated(x, sample_id = 6, compound_id = 20) |> expect_true()

  plot_chrom(x, sample_id = 4) |> expect_no_error()
  plot_chrom(x, sample_id = 4, integrated = T, show_RT = T) |> expect_no_error()
})


test_that("extract_peak_bounds", {

  extract_peak_bounds(main, compound_id = 1) |> expect_error("Compound must have")

  # set expected bounds
  # x <- update_RT(
  #   main,
  #   sample_id = NULL,
  #   compound_id = "C1",
  #   target = "all",
  #   force = FALSE,
  #   manual = FALSE,
  #   peak_start = 0.1,
  #   peak_end = 1
  # )

  # set observed bounds 
  
  x <- .integrate_all_slack(
    main,
    compound_id = 20,
    peak_start = 0.1,
    peak_end = 1,
    mode = "manual"
  )

  bounds <- extract_peak_bounds(x, compound_id = 20)
  bounds |> expect_type("list")
  length(bounds$min) |> expect_equal(7)
  bounds$min |> expect_type("double")
  bounds$max |> expect_type("double")

  bounds$min |> unique() |> expect_equal(0.1)
  bounds$max |> unique() |> expect_equal(1)

})


test_that("integrate function", {
  x <- update_RT(
    main,
    compound_id = 1,
    sample_id = NULL,
    peak_start = 0.5,
    peak_end = 1.5,
    target = "all",
    mode = "manual"
  ) |> expect_no_error()

  is_integrated(x, sample_id = 4, compound_id = 1) |> expect_true()
  integrate(x, compound_id = 1, samples_ids = NULL) |> expect_no_error()
})


test_that("small_peak_filter", {
  # test small peak filter

  main2 <- update_RT(
    main,
    1,
    peak_start = 1,
    peak_end = 2,
    target = "all",
    mode = "auto"
  )
  apply_area_cutoff(main2, 10**3, 1)

  main2 <- update_RT(
    main,
    2,
    peak_start = 1,
    peak_end = 2,
    target = "all",
    mode = "auto"
  )
  apply_area_cutoff(main2, 10**3, 2)  |> expect_no_error()

})
