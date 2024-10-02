test_that("retreive database", {
    testthat::skip_on_cran()
    .reset_samples_db()
    # locate inst folder
    x <- system.file("cmpds.yaml", package = "PKbioanalysis")  |> 
        .parse_cmpds()  |> suppressWarnings()
    .save_cmpd_db(x)
    .get_methodsdb() |> expect_no_error()
    .get_method_cmpds(1)$compound |> 
        expect_equal(x$compounds)
})

