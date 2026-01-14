test_that("generate_96", {
  x <- generate_96()
  expect_equal(dim(x@plate), c(8, 12))
  .is_registered(x) |> expect_equal(FALSE)
  plot(x) |> expect_no_error()
  .plate_subid(x) |> expect_equal(1)
})

test_that("plate_registration", {
  skip_on_cran()

  x <- generate_96() |> add_blank(TRUE, FALSE)

  x <- register_plate(x)
  .is_registered(x) |> expect_equal(TRUE)
  plot(x) |> expect_no_error()

  register_plate(x) |> expect_error()
})

test_that("reuse_plate", {
  skip_on_cran()
  skip_on_ci()

  x <- generate_96() |> add_blank(TRUE, FALSE) |> register_plate()
  .is_registered(x) |> expect_equal(TRUE)
  platesdb <- .get_plates_db()
  currid <- platesdb[1, "id"]
  currid <- unlist(strsplit(currid, "_"))[[1]]
  x <- reuse_plate(as.numeric(currid), 4)
  x |> add_blank() |> add_DB() |> expect_no_error()
  .is_registered(x) |> expect_equal(FALSE)

  validObject(x) |> expect_equal(TRUE)
})


test_that("suitability", {
  x <- generate_96() |> add_suitability("Suitability", conc = 2)
  as.vector(x@plate[1, 1]) |> expect_equal("Suitability")
})

test_that("plate_filled", {
  generate_96() |> add_samples(paste0("S", 1:96)) |> expect_no_error()
})


test_that("single QC per group", {
  a <- generate_96() |> add_cs_curve(1:10) |> add_QC(3, 4.5, 9)

  suppressWarnings(add_QC(a, 3, 4.5, 9)) |>
    expect_error("QC for this group already exists. Add new group")
})

test_that("multiple_stds_n_qcs", {
  # return 0 if no std or qcs
  generate_96() |>
    add_cs_curve(1:10) |>
    .last_entity("Standard") |>
    expect_equal(1)
  generate_96() |>
    add_cs_curve(1:10) |>
    add_cs_curve(1:10) |>
    .last_entity("Standard") |>
    expect_equal(2)

  # retun 0, even if there is cs without qc
  generate_96() |> add_cs_curve(1:10) |> .last_entity("QC") |> expect_equal(0)

  generate_96() |>
    add_cs_curve(1:10) |>
    add_QC(3, 4.5, 9) |>
    .last_entity("QC") |>
    expect_equal(1)
})

test_that("rep_stdTest", {
  generate_96() |>
    fill_scheme("v", tbound = "A", bbound = "C") |>
    # add_samples(rep("SLOQ/4", 3), conc = 0.25) |>
    fill_scheme("h", lbound = 2, rbound = 9) |>
    add_cs_curve(c(1, 3, 5, 10, 20, 50, 100, 200), rep = 3) |>

    fill_scheme("h", lbound = 9, rbound = 12) |>
    add_DB() |>
    add_blank() |>
    add_blank() |>
    fill_scheme("v", tbound = "D", bbound = "G") |>
    # fill_scheme("h", lbound = 1, rbound = 12) |>
    add_QC(3, 80, 180, reg = T, n_qc = 6, qc_serial = F) |>
    fill_scheme("h") |>
    # add_samples(rep("QLOQ/4", 6), conc = 0.25, prefix = "Q") |>
    plot(layoutOverlay = TRUE) |>
    expect_no_error()
})


test_that("Last position", {
  x <- generate_96(start_row = "H", start_col = 5) |>
    add_cs_curve(c(50, 20, 10, 5, 2, 1)) |>
    add_blank(IS = TRUE, analyte = FALSE) |>
    add_blank(IS = FALSE, analyte = FALSE, analytical = FALSE)
  x@plate[8, 12] |> unname() |> expect_equal("DB")
})

# test_that("test_factor_samples", {
#   x <- generate_96() |>
#     add_samples(samples = 1:5, time = 1:10 * 30, conc = 20)

#   x |> plot(color = "conc") |> expect_no_error()
#   x |> plot(color = "factor") |> expect_no_error()
#   x |> plot(color = "time") |> expect_no_error()
# })

test_that("qc_ranges", {
  suppressWarnings({
    generate_96() |>
      add_cs_curve(c(10, 50, 100, 250, 500, 1000, 1500, 2500)) |>
      add_QC(30, 750, 1750) |>
      expect_error()
  })

  ## errors with LQC >3
  suppressWarnings({
    generate_96() |>
      add_cs_curve(c(10, 50, 100, 250, 500, 1000, 1500, 2500)) |>
      add_QC(50, 750, 1750, reg = FALSE) |>
      expect_warning()
  })
})

test_that("fill_horizontalTest", {
  generate_96() |>
    fill_scheme("v", tbound = "A", bbound = "C") |>
    add_samples(1:10) |>
    expect_no_error()

  # avoid filling previously filled spots #TODO raise warning and continue
})

test_that("fill_verticalTest", {
  generate_96() |>
    add_cs_curve(c(10, 50, 100, 250, 500, 1000, 1500, 2500)) |>
    fill_scheme("h", lbound = 1, rbound = 12) |>
    add_QC(30, 758, 1880, reg = T) |>
    expect_no_error()
})


test_that("spot_maskTest", {
  generate_96() |>
    fill_scheme(fill = "v", tbound = "D", bbound = "E") |>
    .spot_mask() |>
    expect_no_error()
})


test_that("samples_vectorization", {
  # multiple samples, multiple time, one dose
  x <- generate_96() |>
    add_samples(samples = 1)

  # multiple samples, one time, multiple doses
  x <- generate_96() |>
    add_samples(samples = 1:10)
})

# test_that("samples_combination", {
#   x <- generate_96() |>
#     add_samples_c(
#       n_rep = 3,
#       time = 0:5 * 30,
#       conc = c(1, 2),
#       factor = c("M", "F")
#     )

#   sum(!is.na(x@df$samples)) |> expect_equal(72) # cartesian product

#   # 2 x 2 x 2
#   x <- generate_96() |>
#     add_samples_c(
#       n_rep = 2,
#       time = 1:10 * 30,
#       dose = c(100, 200),
#       factor = c("M", "F")
#     )

#   x <- x@df |>
#     dplyr::select("samples", "time", "dose", "factor") |>
#     dplyr::distinct()
#   expect_equal(nrow(x), 2 * 2 * 2 * 10 + 1)
#   expect_equal(length(unique(x$time)), 10 + 1)
#   expect_equal(length(unique(x$dose)), 2 + 1)
#   expect_equal(length(unique(x$samples)), 2 * 2 * 2 + 1)
#   expect_equal(length(unique(x$factor)), 2 + 1)
# })

test_that("plotPlateDesignTest", {
  generate_96() |>
    add_blank() |>
    add_cs_curve(c(200, 50, 20, 10, 5, 2, 1), group = "A", rep = 2) |>
    add_QC(3, 70, 180, reg = T, n_qc = 2, qc_serial = F, group = "A") |>
    add_blank() |>
    add_cs_curve(c(200, 50, 20, 10, 5, 2, 1), group = "B", rep = 2) |>
    add_QC(3, 70, 180, reg = T, n_qc = 2, qc_serial = F, group = "B") |>
    add_samples(1:5) |>
    # add_samples(6:10, dose = "A", factor = "F", time = 1:5*30) |>
    add_samples(11:15) |>
    # add_samples(16:18, dose = "B", factor = "F", time = 1:3*30) |>
    plate_tree(plot = T) |>
    expect_no_error()
})

test_that("DQC", {
  x <- generate_96() |>
    add_cs_curve(c(200, 50, 20, 10, 5, 2, 1)) |>
    add_QC(3, 70, 180, reg = T, n_qc = 6, qc_serial = F) |>
    add_DQC(conc = 500, fac = 10, rep = 10)

  plot(x, transform_dil = T) |> expect_no_error()
  plot(x, transform_dil = F) |> expect_no_error()

  generate_96() |>
    fill_scheme("h", lbound = 1, rbound = 10) |>
    add_cs_curve(c(50, 20, 10, 5, 2, 1)) |>
    add_DQC(conc = 500, fac = 10, rep = 10) |>
    add_DQC(conc = 500, fac = 100, rep = 10) |>
    add_samples(1:10, dil = 10) |>
    expect_no_error()
})

test_that("get empty layout", {
  a <- generate_96() |>
    fill_scheme("h", lbound = 1, rbound = 10, bbound = "B")

  length_empty_layout(a) |> expect_equal(20)

  b <- a |> add_blank()
  length_empty_layout(b) |> expect_equal(19)
})

test_that("add samples from db", {
  new_study <- create_new_study(data.frame(
    type = "SD",
    pkstudy = FALSE,
    title = "New Study",
    description = "Description of the new study",
    subject_type = "Human"
  ))
  add_sample_log(
    study_id = new_study$id,
    df = data.frame(
      subject_id = seq(1, 5),
      nominal_time = seq(0, 4),
      status = "Collected"
    )
  )
  df <- retrieve_sample_log(study_id = new_study$id)
  samples <- sample(df$log_id, size = length(df$log_id))

  x <- generate_96() |>
    add_samples_db(samples, namestyle = 1, group = "A")

  # keep the order
  expect_true(all(x@df$log_id[seq_along(samples)] == samples))

  plot(x) |> expect_no_error()
})

test_that("add_from_db2", {
  new_study <- create_new_study(data.frame(
    type = "SD",
    pkstudy = FALSE,
    title = "New Study",
    description = "Description of the new study",
    subject_type = "Human"
  ))

  add_sample_log(
    study_id = new_study$id,
    df = data.frame(
      subject_id = rep(1:3, each = 4),
      nominal_time = rep(c(0, 5, 10, 30), 3), 
      time_unit = "minutes"
    )
  )

  df <- retrieve_sample_log(study_id = new_study$id)
  df <- retrieve_full_study_log(study_id = new_study$id)
  suppressWarnings({
    x <- generate_96() |>
      add_blank() |>
      add_DB() |>
      add_cs_curve(c(1, 3, 10, 50, 80, 100, 200, 300), rep = 2) |>
      add_QC(3, 90, 190, reg = FALSE, n_qc = 6) |>
      add_samples_db(df$log_id, 10) |>
      add_DQC(conc = 500, fac = 10, rep = 1)
  })

  x@df$value[x@df$row == 5 & x@df$col == 7] |>
    unname() |>
    expect_equal("DQC_500_10X")

  # expect DQC str at E7.
  x@plate[["E", 7]] |> expect_equal("DQC_500_10X")
  x@plate[["F", 1]] |> expect_equal(NA_character_)

  # alter samples names
  x |> samples_naming_style() |> plot() |> expect_no_error()
  x |> samples_naming_style(study_name = FALSE) |> plot() |> expect_no_error()
  x |>
    samples_naming_style(study_name = FALSE, use_subject_id = FALSE) |>
    expect_error("group_replicate has NA values.")



  suppressWarnings({
    x <- generate_96() |>
      add_samples_db2(df$log_id, dil = c(1,10, 100)) 
  })

  plot(x, "dil") |> expect_no_error()

  x@df$value |> na.omit() |> length() |> expect_equal(length(df$log_id)*3)

})
