test_that("correct_parsing", {
  plot_chrom(main, sample_id = 3, ncol = 3) |> expect_no_error()

  n_cmpd <- nrow(main@compounds) # number of compounds
  n_samples <- length(main@runs$files) # number of samples
  # length = n_samples * n_cmpd
  expect_equal(n_samples * n_cmpd == nrow(main@peaks), TRUE)
})

test_that("check chromres cmpds against db", {
  # .reset_samples_db()
  x <- system.file("cmpds.yaml", package = "PKbioanalysis") |>
    .parse_cmpds() |>
    suppressWarnings()
  .save_cmpd_db(x)

  check_chrom_cmpds(main, 1) |> expect_true()

  x <- system.file("cmpds2.yaml", package = "PKbioanalysis") |>
    .parse_cmpds() |>
    suppressWarnings()
  .save_cmpd_db(x)
  check_chrom_cmpds(main, 2) |> expect_false()
})


test_that("add-remove-cmpds", {
  skip() # old tests, not relevant anymore

  # add compound
  dat1 <- add_compound(main, "dopamine32", 3)
  n_cmpd <- nrow(dat1@compounds) # number of compounds
  n_samples <- length(dat1@runs) # number of samples
  # length = n_samples * n_cmpd
  expect_equal(n_samples * n_cmpd == nrow(dat1@peaks), TRUE)

  # add compound
  dat2 <- add_compound(dat1, "epi", 3)
  n_cmpd <- nrow(dat2@compounds) # number of compounds
  n_samples <- length(dat2@runs) # number of samples
  # length = n_samples * n_cmpd
  expect_equal(n_samples * n_cmpd == nrow(dat2@peaks), TRUE)

  expect_equal(all(c("dopamine32", "epi") %in% dat2@compounds$compound), TRUE)

  # remove compound
  dat3 <- remove_compound(dat1, 1)
  n_cmpd <- nrow(dat3@compounds) # number of compounds
  n_samples <- length(dat3@runs) # number of samples
  # length = n_samples * n_cmpd
  expect_equal(n_samples * n_cmpd == nrow(dat3@peaks), TRUE)

  res <- remove_compound(dat3, "C22")
  expect_equal(all("epi" %in% res@compounds$compound), FALSE)
})


test_that("chromres_object_getters", {
  get_sample_names(main) |> class() |> expect_equal("data.frame")
  get_sample_ID(main, "2024-04-15_DB_NT_R1") |> length() |> expect_equal(1)
  get_compound_name(main, 1) |> expect_equal("GABA")
  get_compound_ID(main, "GABA") |> length() |> expect_equal(1)
  get_cmpd_IS(main, 1) |> length() |> expect_equal(1)
})


test_that("targetlynxXML_to_chrom_res", {
  # pass both chrom and targetlynx files.
  x <- system.file("sampleTargetLynx.xml", package = "PKbioanalysis")
  x <- read_experiment_results(x, vendor = "targetlynxXML", drop_prefix = T)
  x <- .peakresToDF(x)
  stop(
    "Must be a list of cmpds with conc, is_conc, area, IS_area, rel_area and std_conc"
  )
  # check against DB

  # reintegrate without peakfinding => areas/S&N
})

test_that("targetlynxCSV_to_chrom_res", {
  # pass both chrom and targetlynx files.
  x <- system.file("sampleTargetLynx.xml", package = "PKbioanalysis")
  x <- read_experiment_results(x, vendor = "targetlynxCSV", drop_prefix = T)
  x <- .peakresToDF(x)
  stop(
    "Must be a list of cmpds with conc, is_conc, area, IS_area, rel_area and std_conc"
  )
})
