test_that("suitability", {
  has_suitability_config(quantobj) |> expect_false()

  config_suitability(quantobj, vial_pos = "2:H,10") |>
    expect_error("Selected vial has to be present in at least 3 runs.")

  x <- config_suitability(quantobj, vial_pos = "2:H,9")

  has_suitability_results(x) |> expect_false()

  get_vials(x) |>
    table() |>
    as.data.frame() |>
    dplyr::filter(Var1 == "2:H,9") |>
    dplyr::pull(Freq) |>
    expect_equal(3)

  # linearity not needed for suitability
  # x <- Reduce(function(acc, y) {
  #     run_linearity(acc, compound_id = y)
  #     run_linearity(x, compound_id = y)
  # }, names(quantobj@linearity), init = x)

  prepare_suitability(x) |> expect_no_error()

  x <- run_suitability(x)
  has_suitability_results(x) |> expect_true()

  checkmate::assertDataFrame(x@suitability$results)
  checkmate::assertNames(
    names(x@suitability$results),
    must.include = c("compound", "RSD")
  )

  plot_suitability(x) |> expect_s3_class("ggplot")
  plot_suitability_trend(x)
})
