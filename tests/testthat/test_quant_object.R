
.reset_samples_db()
cmpyml <- system.file("cmpds_MTG.yaml", package = "PKbioanalysis")
cmpyml <- .parse_cmpds(cmpyml) |> suppressWarnings()
.save_cmpd_db(cmpyml)

dat <- system.file("extdata", "08122019_MTG.txt", package = "PKbioanalysis")
suppressWarnings(
  quantobj <- read_experiment_results(dat, vendor = "targetlynx_csv", logkey = "#")
)

quantobj <- create_quant_object(quantobj, 1)

test_that("create_quant_object", {
  checkmate::checkClass(quantobj, "QuantRes") |> expect_true()
  length(quantobj@quanttab) |> expect_equal(4)
  length(quantobj@linearity) |> expect_equal(4)
  length(quantobj@suitability) |> expect_equal(2)
  length(quantobj@resEstim) |> expect_equal(4)
  nrow(quantobj@compounds_metadata) |> expect_equal(4)

  check_quant_method_quantres(quantobj, 1) |> expect_true()
  update_IS_info(quantobj, 1) |> expect_no_error()
})


test_that("quantres_to_matrix", {
  quantres_to_matrix(quantobj, wide = TRUE) |> expect_no_error()
})


test_that("has IS", {
  has_IS(quantobj, "MITRAGYNINE") |> expect_true()
  has_IS(quantobj, "Ketoconazole") |> expect_false()

  derive_rel_response(quantobj, "MITRAGYNINE") |>
    length() |>
    expect_equal(nrow(quantobj@quanttab$MITRAGYNINE))

  derive_rel_response(quantobj, "Ketoconazole") |>
    expect_error("No internal standard")
})

