test_that("pkstudy test", {
  newstudy <- data.frame(
    type = "SD",
    pkstudy = TRUE,
    title = "New Study",
    description = "Description of the new study",
    subject_type = "Human"
  )
  new_study <- create_new_study(newstudy)

  new_study$id |> update_study(data.frame(title = "Updated Study Title"))

  new_study$id |>
    retrieve_study() |>
    pull("title") |>
    expect_equal("Updated Study Title")

  ## dosing & route
  add_dosing_db(
    new_study$id,
    data.frame(
      group_label = "Group 1",
      period_number = 1,
      dose_freq = 0,
      dose_addl = 0,
      dose_amount = 100,
      dose_unit = "mg",
      route = "PO",
      formulation = "Tablet"
    )
  )

  retrieve_dosing_db(study_id = new_study$id)

  update_dosing_db(
    study_id = new_study$id,
    df = data.frame(
      group_label = c("C1", "C2"),
      period_number = 1,
      dose_freq = 0,
      dose_addl = 0,
      dose_amount = 100,
      dose_unit = "mg",
      route = "PO",
      formulation = "Tablet"
    )
  )

  retrieve_dosing_db(study_id = new_study$id) |>
    dplyr::pull("group_label") |>
    expect_equal(c("C1", "C2"))

  retrieve_dosing_db(study_id = new_study$id) |>
    auto_add_row() |>
    update_dosing_db(study_id = new_study$id) |>
    expect_error("Group label cannot be empty")

  retrieve_dosing_db(study_id = new_study$id) |>
    auto_add_row() |>
    remove_all_empty_row() |>
    update_dosing_db(study_id = new_study$id) |>
    dplyr::pull("group_label") |>
    expect_equal(c("C1", "C2"))

  ## subjects
  add_subjects_db(
    new_study$id,
    data.frame(
      subject_id = "S1",
      study_id = new_study$id,
      sex = "M",
      age = 30,
      group_label = "C1"
    )
  )

  retrieve_subjects_db(study_id = new_study$id)

  update_subjects_db(
    study_id = new_study$id,
    df = data.frame(
      subject_id = c("S1", "S2"),
      study_id = new_study$id,
      sex = c("M", "F"),
      age = c(30, 25),
      group_label = c("C1", "C2")
    )
  )

  retrieve_subjects_db(study_id = new_study$id) |>
    dplyr::pull("subject_id") |>
    expect_equal(c("S1", "S2"))
  ## sample log

  add_sample_log(
    study_id = new_study$id,
    df = data.frame(
      subject_id = "S1",
      study_id = new_study$id,
      status = "Collected"
    )
  )

  retrieve_sample_log(study_id = new_study$id)

  update_sample_log(
    study_id = new_study$id,
    df = data.frame(
      subject_id = c("S1", "S2"),
      study_id = new_study$id,
      status = c("Collected", "Planned")
    )
  )

  retrieve_sample_log(study_id = new_study$id) |>
    dplyr::pull("status") |>
    expect_equal(c("Collected", "Planned"))
})

test_that("nonpkstudy test", {
  new_study <- create_new_study(data.frame(
    type = "SD",
    pkstudy = FALSE,
    title = "New Study",
    description = "Description of the new study",
    subject_type = "Human"
  ))
  expect_false(is_pk_study(new_study$id))

  # add NA subjects
  add_sample_log(
    study_id = new_study$id,
    df = data.frame(
      subject_id = NA,
      nominal_time = seq(0, 4),
      status = "Collected"
    )
  ) |>
    expect_error()

  add_sample_log(
    study_id = new_study$id,
    df = data.frame(
      subject_id = seq(1, 5),
      nominal_time = seq(0, 4),
      status = "Collected"
    )
  ) |>
    expect_no_error()

  df <- retrieve_sample_log(study_id = new_study$id)

  retrieve_full_study_log(study_id = new_study$id)

  plot_study_design(new_study$id)

  # no log id
  update_sample_log(
    study_id = new_study$id,
    df = data.frame(
      subject_id = seq(1, 5),
      nominal_time = seq(0, 4),
      status = "Processed"
    )
  )

  df2 <- update_sample_log(
    study_id = new_study$id,
    df = data.frame(
      log_id = df$log_id,
      subject_id = seq(1, 5),
      nominal_time = seq(0, 4),
      status = "Processed"
    )
  )
  all(df2$log_id == df$log_id) |> expect_true()

  c("subject_id", "nominal_time", "status") %in%
    colnames(retrieve_full_study_log(new_study$id)) |>
    all() |>
    expect_true()

  retrieve_full_log_by_id(df$log_id)
})


test_that("check if study is pk study", {
  new_study <- create_new_study(data.frame(
    type = "SD",
    pkstudy = TRUE,
    title = "New Study",
    description = "Description of the new study",
    subject_type = "Human"
  ))

  expect_true(is_pk_study(new_study$id))

  new_study <- create_new_study(data.frame(
    type = "SD",
    pkstudy = FALSE,
    title = "New Study",
    description = "Description of the new study",
    subject_type = "InVitro"
  ))

  expect_false(is_pk_study(new_study$id))
})


test_that("invitro metabolic study", {
  cmpds <- c("C1", "C2", "C3")
  time_points <- c(0, 5, 10, 15, 30, 45, 60, 75, 90, 120)

  newstudy <- make_metabolic_study(
    study = "In Vitro Metabolic Study",
    cmpds = cmpds,
    time_points = time_points,
    dose = 100,
    n_NAD = 3,
    n_noNAD = 2
  ) |>
    expect_no_error()

  plot_study_design(newstudy$id) |> expect_no_error()
})

test_that("cell stability study", {
  cmpds <- c("C1", "C2", "C3")
  time_points <- c(0, 1, 6)
  arms <- c("DMSO", "Saline")
  conditions <- c("Condition1", "Condition2")

  newstudy <- make_cell_stability_study(
    study_title = "Cell Stability Study",
    cmpds = cmpds,
    time_points = time_points,
    arms = arms,
    conditions = conditions,
    n_replicates = 2
  ) |>
    expect_no_error()

  plot_study_design(newstudy$id) |> expect_no_error()

  
  cmpds <- "C1"
  time_points <- c(0, 1, 6)
  arms <- c("DMSO", "Saline")
  conditions <- c("A", "B", "C", "D")
  n_replicates <- 3
  newstudy <- make_cell_stability_study(
    study_title = "Cell Stability Study",
    time_points = time_points,
    cmpds = cmpds,
    arms = arms,
    conditions = conditions,
    n_replicates = n_replicates,
    ctrl = "Standard"
  ) |>
    expect_no_error()

  df <- retrieve_full_study_log(newstudy$id)
  nrow(df) |> expect_equal(96)




})
