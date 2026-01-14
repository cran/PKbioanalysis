test_that("dir or files", {
  path <- system.file("extdata", "waters_raw_ex", package = "PKbioanalysis")
  dir_or_files_to_files(path, "raw$") |> length() |> expect_equal(7)

  path <- list.files(
    path,
    full.names = TRUE,
    pattern = "raw$",
    all.files = TRUE
  )
  dir_or_files_to_files(path, "raw$") |>
    expect_identical(dir_or_files_to_files(path, "raw$"))
})

test_that("importing_with_sample_list_augment_from_db", {})

test_that("importing_without_sample_list_augment_from_db", {})

test_that("importing_chrom_with_transition_augment", {})

test_that("importing_chrom_watersraw", {
  skip()
  path <- system.file(
    "extdata",
    "waters_raw_ex_nodb",
    package = "PKbioanalysis"
  )
  read_chrom(path, 1) |>
    expect_no_error()
})


test_that("importing_chrom_mzml", {
  skip()
  path <- system.file("extdata", "waters_MZML_ex", package = "PKbioanalysis")
  read_chrom(path, format = "mzML", method = 1) |>
    expect_no_error()
})
