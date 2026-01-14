# test_that("linearity_sync", {
#   main <- sync_linearity(quantobj)
# })
test_that("run linearity", {
  suppressWarnings({
    x <- run_linearity(quantobj, compound_id = "MITRAGYNINE") |>
      expect_no_error()

    x <- Reduce(
      function(acc, y) {
        run_linearity(acc, compound_id = y)
      },
      # names(quantobj@linearity),
      c("MITRAGYNINE", "7OH-MITRA"),
      init = quantobj
    ) |>
      expect_no_error()

    has_linearity(x, "MITRAGYNINE") |> expect_true()
    has_linearity(x, "7OH-MITRA") |> expect_true()
    has_linearity(x, "Ketoconazole") |> expect_false() #

    tabulate_summary_linearity(x, "MITRAGYNINE") |> nrow() |> expect_equal(1)
    tabulate_summary_linearity(x) |> nrow() |> expect_equal(2)

    plot_linearity(x, "MITRAGYNINE") |> expect_no_error()

    plot_residuals(x, "MITRAGYNINE") |> expect_no_error()
  })
})


test_that("run linearity normalized", {
  # test that fails if no IS assigned
  # test that normalize fails if not integrated IS
  # test it calculates on rel_response

  suppressWarnings({
    x <- run_linearity(
      quantobj,
      compound_id = "MITRAGYNINE",
      normalize = TRUE
    )

    has_linearity(x, "MITRAGYNINE") |> expect_true()
  })

  suppressWarnings({
    x <- run_linearity(
      quantobj,
      compound_id = "Ketoconazole",
      normalize = TRUE
    ) |>
      expect_error("All rel_response values are missing")

    has_linearity(quantobj, "Ketoconazole") |> expect_false()
  })

})

