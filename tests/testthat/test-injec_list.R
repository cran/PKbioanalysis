test_that("plate_registered_before_seq", {
  generate_96() |>
    build_injec_seq(injec_vol = 2, tray = "2") |>
    expect_error("Plate is not registered")
})

test_that("build_injec_seq", {
  skip_on_cran()

  # .reset_samples_db()
  x <- generate_96() |>
    add_suitability("Suitability", conc = 2) |>
    add_blank(group = "GP1") |>
    add_DB(group = "GP1") |>
    add_samples(1:10, group = "GP1") |>
    add_cs_curve(c(1, 10, 30, 40, 100, 200), group = "GP1") |>
    add_QC(3, 65, 180, group = "GP1") |>
    add_blank(group = "GP2") |>
    add_DB(group = "GP2") |>
    add_samples(50:70, group = "GP2") |>
    add_cs_curve(c(1, 10, 30, 40, 100, 200), group = "GP2") |>
    add_QC(3, 65, 180, group = "GP2") |>
    add_DQC(1000, fac = 10, rep = 3, group = "GP2") |>
    register_plate()

  # register_plate()

  injseq <- build_injec_seq(
    x,
    injec_vol = 2,
    tray = "1",
    method = "method.q",
    n_explore = 4
  )
  print(injseq)

  
  expect_true("total_volume" %in% colnames(summary(injseq))) # generic summary test

  # assert all file names are unique
  length(unique(injseq$injec_list$FILE_NAME)) |>
    expect_equal(nrow(injseq$injec_list))

  # assert all file names has ending with upper alphabet 
  all(grepl("\\d+[A-Z]$", injseq$injec_list$FILE_NAME)) |>
    expect_true()
})


test_that("save_injecseq_csv", {
  skip_on_cran()

  # .reset_samples_db()
  x <- generate_96() |>
    add_samples(1:20) |>
    add_blank() |>
    plate_metadata("new") |>
    register_plate()

  mylist <- x |>
    build_injec_seq(
      injec_vol = 2,
      tray = "1",
      method = "method.ql",
      rep_suitability = 0
    )

  write_injec_seq(mylist) |> expect_no_error()

  x |>
    build_injec_seq(
      injec_vol = 2,
      suffix = "4",
      tray = "1",
      method = "method.q",
      rep_suitability = 0
    ) |> # suffix changed
    write_injec_seq() |>
    expect_no_error() # changed the suffix

  write_injec_seq(mylist) |> expect_error("Error writing") # already written

  .get_samplesdb_metadata() |> expect_no_error()
})

test_that("multiple_plates", {
  # expect 1 list
  skip_on_cran()

  # .reset_samples_db()
  x <- generate_96() |>
    add_samples(1:20) |>
    add_blank() |>
    plate_metadata("new") |>
    register_plate()

  y <- generate_96() |>
    add_samples(1:20) |>
    add_blank() |>
    plate_metadata("new") |>
    register_plate()

  x <- combine_plates(list(x, y))

  length(x) |> expect_equal(2)

  build_injec_seq(
    x,
    tray = c("1", "2"),
    injec_vol = 2,
    method = "method.q",
    rep_suitability = 0
  ) |>
    expect_no_error()
})

test_that("exploratory_samples_added", {
  skip_on_cran()

  # .reset_samples_db()
  x <- generate_96() |>
    add_cs_curve(c(1, 10, 30, 40, 100, 200)) |> # 1
    add_samples(1:20) |> # 2
    add_blank() |> # 3
    plate_metadata("new") |>
    register_plate()
  build_injec_seq(
    x,
    tray = "1",
    injec_vol = 2,
    n_explore = 2,
    method = "method.q1",
    rep_suitability = 0
  ) |>
    expect_no_error()
})


test_that("writing_increment_id", {
  skip_on_cran()
  skip_on_ci()

  # The metadata table is accommodating for the list
  .reset_samples_db()
  x <- generate_96() |>
    add_cs_curve(c(1, 10, 30, 40, 100, 200)) |> # 1
    add_samples(1:20) |> # 2
    add_blank() |> # 3
    plate_metadata("new") |>
    register_plate()
  build_injec_seq(
    x,
    method = "method.ql",
    tray = "1",
    injec_vol = 2,
    n_explore = 2,
    rep_suitability = 0
  ) |>
    write_injec_seq()

  .last_list_id() |> expect_equal(1)

  build_injec_seq(
    x,
    method = "method.ql",
    tray = "1",
    injec_vol = 2,
    n_explore = 4,
    suffix = "2",
    rep_suitability = 0
  ) |>
    write_injec_seq()

  .last_list_id() |> expect_equal(2)
})

test_that("export masslynx", {
  skip_on_cran()

  .reset_samples_db()
  x <- generate_96() |>
    add_cs_curve(c(1, 10, 30, 40, 100, 200)) |> # 1
    add_samples(1:20) |> # 2
    add_blank() |> # 3
    plate_metadata("new") |>
    register_plate()

  injlist <- build_injec_seq(
    x,
    method = "method.ql",
    tray = "1",
    injec_vol = 2,
    n_explore = 4,
    suffix = "2",
    rep_suitability = 0
  ) |>
    write_injec_seq()

  masslynxlist <- download_sample_list(injlist, "masslynx")

  all(
    c("Index", "FILE_NAME", "SAMPLE_LOCATION", "CONC_A") %in%
      colnames(masslynxlist)
  ) |>
    expect_true()
})
