skip_on_cran()
skip_on_ci()
test_that("study app launches and server is valid", {
  shiny::testServer(study_app_server, {
    expect_true(!is.null(session))
    expect_true(is.environment(session))
  })
})

test_that("chrom app launches and server is valid", {
  shiny::testServer(chromapp_server, {
    expect_true(!is.null(session))
    expect_true(is.environment(session))
  })
})

####################################################
test_that("quant app launches and server is valid", {
  shiny::testServer(quantapp_server, {
    expect_true(!is.null(session))
    expect_true(is.environment(session))
  })
})


test_that("linearity server", {
  shiny::testServer(linearity_data_server, {
    expect_true(!is.null(session))
    expect_true(is.environment(session))
  })
})
