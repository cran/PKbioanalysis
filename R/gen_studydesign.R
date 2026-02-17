add_column_if_missing <- function(df, colname, default = NA_character_) {
  if (!colname %in% names(df)) {
    df[[colname]] <- default
  }
  df
}

#' Create a new study in the database
#' @param df A data frame with one row containing study details:
#' type, title, description, pkstudy (logical), subject_type
#' @returns A data frame with the created study details including generated id, start_date, and status
#' @export
create_new_study <- function(df) {
  checkmate::assertNames(
    names(df),
    must.include = c("type", "title", "description", "pkstudy", "subject_type")
  )
  checkmate::assertDataFrame(df, nrows = 1, ncols = 5)
  checkmate::assertLogical(df$pkstudy, len = 1)
  checkmate::assertChoice(df$type, choices = c("SD", "MD", "FE", "BE", "NA"))
  checkmate::assertString(df$title, min.chars = 1)

  df$id <- uuid::UUIDgenerate()
  df$start_date <- Sys.Date()
  df$status <- "Planned"

  .check_sample_db()

  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  tryCatch(
    {
      DBI::dbBegin(db)
      DBI::dbAppendTable(db, "study", df)
      DBI::dbCommit(db)
    },
    error = function(e) {
      DBI::dbRollback(db)
      stop(e)
    }
  )

  df
}

retrieve_study <- function(study_id) {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  study <- DBI::dbGetQuery(
    db,
    paste0("SELECT * FROM study WHERE id = '", study_id, "'")
  )
  if (nrow(study) == 0) {
    stop("Study not found")
  }
  study
}

update_study <- function(study_id, df) {
  checkmate::assertDataFrame(df, nrows = 1, min.cols = 1)

  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  tryCatch(
    {
      DBI::dbBegin(db)
      for (col in names(df)) {
        DBI::dbExecute(
          db,
          paste0(
            "UPDATE study SET ",
            col,
            " = '",
            df[[col]],
            "' WHERE id = '",
            study_id,
            "'"
          )
        )
      }
      DBI::dbCommit(db)
    },
    error = function(e) {
      DBI::dbRollback(db)
      stop(e)
    }
  )
}


add_dosing_db <- function(study_id, df) {
  checkmate::assertDataFrame(df, min.rows = 1)
  checkmate::assertNames(
    names(df),
    must.include = c(
      "group_label",
      "period_number",
      "dose_freq",
      "dose_addl",
      "dose_amount",
      "dose_unit",
      "route",
      "formulation"
    )
  )

  if (
    any(df$group_label == "") |
      any(is.na(df$group_label)) |
      any(duplicated(df$group_label)) |
      any(is.null(df$group_label))
  ) {
    stop("Group label cannot be empty")
  }

  df <- add_column_if_missing(df, "arm_id")
  df$arm_id[is.na(df$arm_id)] <- uuid::UUIDgenerate(n = sum(is.na(df$arm_id)))

  df$study_id <- study_id
  .check_sample_db()
  db <- .connect_to_db()
  on.exit(.close_db(db, TRUE), add = TRUE)
  tryCatch(
    {
      DBI::dbBegin(db)
      DBI::dbAppendTable(db, "dosing", df)
      DBI::dbCommit(db)
    },
    error = function(e) {
      DBI::dbRollback(db)
      stop(e)
    }
  )
  df
}

retrieve_dosing_db <- function(study_id) {
  db <- .connect_to_db()
  on.exit(.close_db(db, TRUE), add = TRUE)
  dosing <- DBI::dbGetQuery(
    db,
    paste0("SELECT * FROM dosing WHERE study_id = '", study_id, "'")
  )
  dosing
}

update_dosing_db <- function(study_id, df) {
  checkmate::assertDataFrame(df, min.rows = 1, min.cols = 1)

  if (any(df$group_label == "") | any(is.na(df$group_label))) {
    stop("Group label cannot be empty")
  }

  df <- fill_uuid(df, "arm_id")

  # check if all group labels in samples_df are dose db, stop so won't delete
  samples_df <- retrieve_sample_log(study_id)
  if (!all(is.na(unique(samples_df$group_label)))) {
    if (any(!(samples_df$group_label %in% df$group_label))) {
      stop("Some group labels in sample log does not exist in dosing table")
    }
  }

  db <- .connect_to_db()
  # on.exit(.close_db(db), add = TRUE)
  DBI::dbBegin(db)
  DBI::dbExecute(
    db,
    paste0("DELETE FROM dosing WHERE study_id = '", study_id, "'")
  )
  DBI::dbCommit(db)
  .close_db(db, TRUE) # close before connection

  df <- add_dosing_db(study_id, df)
  df
}

add_subjects_db <- function(study_id, df) {
  checkmate::assertDataFrame(df, min.rows = 1)
  checkmate::assertNames(
    names(df),
    must.include = c("subject_id", "study_id", "group_label")
  )

  df <- fill_uuid(df, "uuid_subject")

  if (any(df$group_label == "") | any(is.na(df$group_label))) {
    stop("Group label cannot be empty")
  }

  df <- add_column_if_missing(df, "sex")
  df <- add_column_if_missing(df, "age", default = NA_integer_)
  df <- add_column_if_missing(df, "race")
  df <- add_column_if_missing(df, "extra_factors")

  ##### check group_replicate #####
  df$group_replicate <- NULL
  if (!"group_replicate" %in% names(df)) {
    df <- df |>
      dplyr::group_by(
        .data$group_label,
        .data$sex,
        .data$age,
        .data$race,
        .data$extra_factors
      ) |>
      dplyr::mutate(group_replicate = dplyr::row_number()) |>
      dplyr::ungroup()
  }

  # assert group_replicate is numeric, starts from 1 till n without missing
  checkmate::assertNumeric(df$group_replicate, lower = 1)
  grp_reps <- df |>
    dplyr::group_by(
      .data$group_label,
      .data$sex,
      .data$age,
      .data$race,
      .data$extra_factors
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      min_rep = min(.data$group_replicate),
      max_rep = max(.data$group_replicate),
      unique_reps = length(unique(.data$group_replicate))
    ) |>
    dplyr::mutate(expected_n = .data$max_rep - .data$min_rep + 1)
  if (
    any(grp_reps$n != grp_reps$expected_n) |
      any(grp_reps$n != grp_reps$unique_reps)
  ) {
    stop(
      "group_replicate must start from 1 and be continuous without missing values for each group_label
          and combination of sex, age, race, and extra_factors"
    )
  }

  ################################

  df$study_id <- study_id
  .check_sample_db()

  gp_labs <- retrieve_dosing_db(study_id)[["group_label"]]
  if (length(gp_labs) < 1) {
    stop("No group labels found for dosing. Add dosing information first.")
  }

  if (!all(df$group_label %in% gp_labs)) {
    stop("Some group labels in subjects do not match dosing.")
  }

  db <- .connect_to_db()
  on.exit(.close_db(db, TRUE), add = TRUE)
  tryCatch(
    {
      DBI::dbBegin(db)
      DBI::dbAppendTable(db, "subject", df)
      DBI::dbCommit(db)
    },
    error = function(e) {
      DBI::dbRollback(db)
      stop(e)
    }
  )
  df
}

retrieve_subjects_db <- function(study_id) {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  subjects <- DBI::dbGetQuery(
    db,
    paste0("SELECT * FROM subject WHERE study_id = '", study_id, "'")
  )
  subjects
}

update_subjects_db <- function(study_id, df) {
  checkmate::assertDataFrame(df, min.rows = 1, min.cols = 1)

  db <- .connect_to_db()
  on.exit(.close_db(db, TRUE), add = TRUE)
  DBI::dbBegin(db)
  DBI::dbExecute(
    db,
    paste0("DELETE FROM subject WHERE study_id = '", study_id, "'")
  )
  DBI::dbCommit(db)

  df <- add_subjects_db(study_id, df)
  df
}

add_sample_log <- function(study_id, df) {
  checkmate::assertDataFrame(df, min.rows = 1)
  checkmate::assertNames(names(df), must.include = c("subject_id"))

  df$study_id <- study_id
  df <- fill_uuid(df, "log_id")
  stopifnot(!any(duplicated(df$log_id)) | !any(is.na(df$log_id)))

  # if is pk_study, subject_id must be in subject table
  if (is_pk_study(study_id)) {
    subjects <- retrieve_subjects_db(study_id)$subject_id
    if (!all(df$subject_id %in% subjects)) {
      stop("Some subject IDs in sample log do not exist in subject table.")
    }
  }

  .check_sample_db()

  db <- .connect_to_db()
  on.exit(.close_db(db, TRUE), add = TRUE)
  tryCatch(
    {
      DBI::dbBegin(db)
      DBI::dbAppendTable(db, "sample_log", df)
      DBI::dbCommit(db)
    },
    error = function(e) {
      DBI::dbRollback(db)
      stop(e)
    }
  )
  df
}

retrieve_sample_log <- function(study_id) {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  sample_log <- DBI::dbGetQuery(
    db,
    paste0("SELECT * FROM sample_log WHERE study_id = '", study_id, "'")
  )
  sample_log
}

update_sample_log <- function(study_id, df) {
  checkmate::assertDataFrame(df, min.rows = 1, min.cols = 1)
  checkmate::assertNames(names(df), must.include = c("subject_id"))

  db <- .connect_to_db()
  on.exit(.close_db(db, TRUE), add = TRUE)
  DBI::dbBegin(db)
  DBI::dbExecute(
    db,
    paste0("DELETE FROM sample_log WHERE study_id = '", study_id, "'")
  )
  DBI::dbCommit(db)

  df <- add_sample_log(study_id, df)
  df
}


# TODO either DF or import from csv
update_sample_quant <- function(
  study_id,
  df,
  type = c("targetlynxCSV", "targetlynxXML", "csv")
) {
  checkmate::assertDataFrame(df, min.rows = 1, min.cols = 1)
}

list_all_studies <- function() {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  studies <- DBI::dbGetQuery(db, "SELECT * FROM study")

  studies
}

get_study_arms <- function(study_id) {
  retrieve_dosing_db(study_id)$group_label
}

get_study_subjects <- function(study_id) {
  retrieve_subjects_db(study_id)$subject_id
}


get_n_arms <- function(study_id) {
  retrieve_dosing_db(study_id) |> nrow()
}

get_n_subjects <- function(study_id) {
  retrieve_subjects_db(study_id) |> nrow()
}


get_n_samples <- function(study_id) {
  retrieve_sample_log(study_id) |> nrow()
}


remove_all_empty_row <- function(df) {
  df[!apply(is.na(df) | df == "", 1, all), ]
}

auto_add_row <- function(df) {
  # add if empty dataframe or last row is complete
  if (nrow(df) == 0 || all(complete.cases(df[nrow(df), ]))) {
    nadf <- data.frame(matrix(NA, ncol = ncol(df), nrow = 1))
    colnames(nadf) <- colnames(df)
    df <- rbind(df, nadf)
  }
  df
}

last_row_empty <- function(df) {
  cond1 <- all(df[nrow(df), ] == "")
  cond2 <- all(is.na(df[nrow(df), ]))
  cond1 | cond2
}


#' Fill missing UUIDs in a specified column
#' @param df A data frame
#' @param col The column name to fill with UUIDs
#' @noRd
fill_uuid <- function(df, col) {
  if (!col %in% names(df)) {
    df[[col]] <- NA
  }
  is_na <- is.na(df[[col]]) | df[[col]] == ""
  n_na <- sum(is_na)
  if (n_na > 0) {
    df[is_na, col] <- uuid::UUIDgenerate(n = n_na)
  }
  df
}

is_pk_study <- function(study_id) {
  retrieve_study(study_id)$pkstudy
}


retrieve_full_study_log <- function(study_id) {
  studydb <- retrieve_study(study_id)
  subjectsdb <- retrieve_subjects_db(study_id)
  sample_log <- retrieve_sample_log(study_id)
  dosingdb <- retrieve_dosing_db(study_id)

  sample_log <- sample_log |>
    dplyr::left_join(
      subjectsdb,
      by = c("subject_id", "study_id"),
      suffix = c("", ".subj")
    ) |>
    dplyr::left_join(
      dosingdb,
      by = c("group_label", "study_id"),
      suffix = c("", ".dose")
    )
  if (nrow(sample_log) == 0) {
    stop("No sample log found for study ID ", study_id)
  }
  sample_log
}

retrieve_full_log_by_id <- function(log_ids) {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)

  DBI::dbWriteTable(
    db,
    "temp_log_ids",
    data.frame(log_id = log_ids),
    temporary = TRUE
  ) # tmp table
  query <- "
  SELECT sl.*
  FROM sample_log sl
  INNER JOIN temp_log_ids tli ON sl.log_id = tli.log_id
  "

  full_log <- DBI::dbGetQuery(db, query)

  if (nrow(full_log) == 0) {
    stop("No matching log entries found.")
  }
  if (nrow(full_log) != length(log_ids)) {
    stop("Some log IDs were not found in the database.")
  }

  # for each unique study id, join all tables
  full_log <- lapply(unique(full_log$study_id), function(study_id) {
    studydb <- retrieve_study(study_id)
    subjectsdb <- retrieve_subjects_db(study_id)
    dosingdb <- retrieve_dosing_db(study_id)

    # Only join if the data frames are not empty
    log_subset <- full_log[full_log$study_id == study_id, , drop = FALSE]
    log_subset <- log_subset |>
      dplyr::left_join(
        subjectsdb,
        by = c("subject_id", "study_id"),
        suffix = c("", ".subj")
      ) |>
      dplyr::left_join(
        dosingdb,
        by = c("group_label", "study_id"),
        suffix = c("", ".dose")
      ) |>
      dplyr::left_join(
        studydb,
        by = c("study_id" = "id"),
        suffix = c("", ".study")
      )
    log_subset
  })
  full_log <- do.call(rbind, full_log)
  full_log
}

#' Retrieve full log information by injection IDs 
#' Retrieve injection id, then join log and all related tables to get full study design info for the given injection ids
#' @param injec_ids vector of injection ids
#' @return data.frame with subject info and dil info
#' @noRd
retrieve_log_by_injecid <- function(injec_ids) {
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)

  # Retrieve samples table for given injection IDs
  injec_df <- DBI::dbGetQuery(
    db,
    sprintf(
      "SELECT injec_id, log_id, dil, file_name FROM samples WHERE injec_id IN (%s)",
      paste(sprintf("'%s'", injec_ids), collapse = ",")
    )
  )

  if (nrow(injec_df) == 0) {
    stop("No matching injection IDs found in samples table.")
  }

  # Retrieve full log info for the associated log_ids
  full_log <- retrieve_full_log_by_id(injec_df$log_id)

  # Join injection info to full log by log_id
  result <- dplyr::left_join(
    injec_df,
    full_log,
    by = "log_id"
  )

  result
}

plot_study_design <- function(study_id, plot = TRUE) {
  df <- retrieve_full_study_log(study_id)
  df$pathString <- paste5(
    paste0(get_study_name(study_id), " (", substr(df$study_id, 1, 4), ")"),
    paste("arm:", df$group_label),
    paste("sex:", df$sex),
    paste("extra_factors:", df$extra_factors),
    paste("dose:", df$dose_amount),
    paste("route:", df$route),
    paste("formulation:", df$formulation),
    paste("subject_id:", df$subject_id),
    paste("sample_type:", df$sample_type),
    paste("T:", df$nominal_time),
    sep = "/",
    na.rm = TRUE
  )

  tree <- data.tree::as.Node(df, na.rm = TRUE)

  if (plot) {
    data.tree::SetGraphStyle(tree, rankdir = "LR")
    data.tree::SetEdgeStyle(
      tree,
      arrowhead = "vee",
      color = "grey35",
      penwidth = 2
    )
    data.tree::SetNodeStyle(
      tree,
      style = "filled,rounded",
      shape = "box",
      fillcolor = "black",
      fontname = "helvetica",
      tooltip = data.tree::GetDefaultTooltip
    )

    plot(tree)
  } else {
    tree
  }
}

get_study_name <- function(study_id) {
  study <- retrieve_study(study_id)
  study$title
}


#' Create a metabolic study layout
#' @param study study name
#' @param cmpds vector of compounds, including any standards
#' @param time_points vector of time points
#' @param dose dose amount. Default is NA
#' @param n_NAD number of NAD positive samples. Default is 3
#' @param n_noNAD number of NAD negative samples. Default is 2
#'
#' @details Note that this function does not require plate object. It will create a plate object automatically and return MultiPlate object
#' @returns MultiPlate object
#' @export
make_metabolic_study <- function(
  study = "Metabolic Study",
  cmpds,
  time_points = c(0, 5, 10, 15, 30, 45, 60, 75, 90, 120),
  dose = NA,
  n_NAD = 3,
  n_noNAD = 2
) {
  checkmate::assertVector(cmpds)
  checkmate::assertVector(time_points)
  checkmate::assertNumeric(n_NAD)
  checkmate::assertNumeric(n_noNAD)

  # Generate unique subject IDs across all compounds and factors
  total_subjects <- n_NAD + n_noNAD
  subject_ids <- seq_len(total_subjects * length(cmpds))
  df_list <- lapply(seq_along(cmpds), function(i) {
    cmpd <- cmpds[i]
    nad_subjects <- subject_ids[seq(
      (i - 1) * total_subjects + 1,
      length.out = n_NAD
    )]
    nonad_subjects <- subject_ids[seq(
      (i - 1) * total_subjects + n_NAD + 1,
      length.out = n_noNAD
    )]
    # NAD samples
    nad_df <- expand.grid(
      cmpd = cmpd,
      time_points = time_points,
      factor = "NAD",
      subjects = nad_subjects
    )
    # noNAD samples
    nonad_df <- expand.grid(
      cmpd = cmpd,
      time_points = time_points,
      factor = "noNAD",
      subjects = nonad_subjects
    )
    rbind(nad_df, nonad_df)
  })
  df <- do.call(rbind, df_list)
  df <- dplyr::arrange(
    df,
    .data$cmpd,
    .data$factor,
    .data$subjects,
    .data$time_points
  )

  # n_plates <- ceiling(nrow(df) / 96)
  # plates_ids <- .compile_cached_plates()
  # plates_ids <- str_split(plates_ids, "_") |>
  #   sapply(function(x) x |> _[1]) |> # get plate_id, ignore exp id
  #   as.numeric() |>
  #   {
  #     \(x) max(x)
  #   }()

  # plates_ids <- plates_ids + c(1:n_plates)

  # plate <- lapply(1:n_plates, function(x) {
  #   curr_plate <- generate_96()
  #   if (x == 1) {
  #     # first plate
  #     vec <- 1:96
  #     curr_plate <- add_samples(
  #       curr_plate,
  #       time = df$time_points[vec],
  #       samples = df$cmpd[vec],
  #       prefix = "",
  #       factor = as.character(df$factor[vec])
  #     )
  #   } else if (x == n_plates) {
  #     # last plate
  #     y <- x - 1
  #     vec <- (y * 96 + 1):nrow(df)
  #     current_df <- df[vec, ]

  #     stopifnot(nrow(current_df) <= 96)

  #     curr_plate <- add_samples(
  #       curr_plate,
  #       time = current_df$time_points,
  #       samples = current_df$cmpd,
  #       prefix = "",
  #       factor = as.character(current_df$factor)
  #     )
  #     curr_plate@plate_id <- paste0(plates_ids[x], "_1")
  #   } else {
  #     y <- x - 1
  #     vec <- (y * 96 + 1):(y * 96 + 96)
  #     current_df <- df[vec, ]

  #     stopifnot(nrow(current_df) == 96)

  #     curr_plate <- add_samples(
  #       curr_plate,
  #       time = current_df$time_points,
  #       samples = current_df$cmpd,
  #       prefix = "",
  #       factor = as.character(current_df$factor)
  #     )
  #     curr_plate@plate_id <- paste0(plates_ids[x], "_1")
  #   }
  #   curr_plate
  # })

  # plate <- new("MultiPlate", plates = plate)
  # plate

  newstudy <- create_new_study(
    data.frame(
      type = "SD",
      title = study,
      description = paste("Metabolic study with", length(cmpds), "compounds"),
      pkstudy = FALSE,
      subject_type = "InVitro"
    )
  )
  cli::cli_alert_info("Created study with ID: {newstudy$id}")
  cli::cli_alert_info("Adding dosing and subjects information...")

  add_dosing_db(
    study_id = newstudy$id,
    df = data.frame(
      group_label = unique(df$cmpd),
      period_number = 1,
      dose_freq = NA_real_,
      dose_addl = NA_integer_,
      dose_amount = dose,
      dose_unit = NA_character_,
      route = NA_character_,
      formulation = NA_character_
    )
  )
  cli::cli_alert_success("Dosing information added.")

  cli::cli_alert_info("Adding subjects information...")

  subjectsdf <- df |> dplyr::select(-"time_points") |> dplyr::distinct()
  stopifnot(
    length(unique(subjectsdf$subjects)) == total_subjects * length(cmpds)
  )
  add_subjects_db(
    study_id = newstudy$id,
    df = data.frame(
      subject_id = subjectsdf$subjects,
      study_id = newstudy$id,
      group_label = subjectsdf$cmpd,
      sex = NA_character_,
      age = NA_integer_,
      extra_factors = subjectsdf$factor
    )
  )
  cli::cli_alert_success("Subjects information added.")
  cli::cli_alert_info("Adding sample log information...")

  add_sample_log(
    study_id = newstudy$id,
    df = data.frame(
      subject_id = df$subjects,
      nominal_time = as.character(df$time_points),
      status = "Planned"
    )
  )
  cli::cli_alert_success("Sample log information added.")
  newstudy
}

get_study_subject_type <- function(study_id) {
  study <- retrieve_study(study_id)
  study$subject_type
}

make_cell_stability_study <- function(
  study_title = "Cell Stability Study",
  time_points = c(0, 1, 6),
  time_unit = "hours",
  cmpds = "A",
  arms = c("DMSO", "Saline"),
  conditions = c("-80C", "4C", "-20C", "RT"),
  n_replicates = 3,
  ctrl = "Standard"
) {
  nsubjects <- length(cmpds) * length(arms) * length(conditions) * n_replicates
  subject_ids <- seq_len(nsubjects)

  res <- expand.grid(
    time_points = time_points,
    cmpds = cmpds,
    arms = arms,
    conditions = conditions,
    replicates = seq_len(n_replicates)
  )
  subject_ids <- rep(subject_ids, each = nrow(res) / nsubjects)
  res$subject_id <- subject_ids

  ctrlres <- expand.grid(
    cmpds = ctrl,
    arms = arms,
    conditions = conditions,
    replicates = seq_len(n_replicates)
  ) 
  ctrlres$time_points <- NA
  ctrlres$subject_id <- seq(
    max(res$subject_id) + 1,
    max(res$subject_id) + nrow(ctrlres)
  )

  res <- rbind(res, ctrlres) |>
    dplyr::arrange(.data$time_points, .data$arms, .data$conditions, .data$replicates)

  subjectsdf <- res |>
    dplyr::select(-"time_points") |>
    dplyr::distinct() |>
    dplyr::mutate(
      study_id = "test_study",
      group_label = paste0(.data$arms, "_", .data$cmpds),
      group_replicate = .data$replicates,
      extra_factors = .data$conditions,
      subject_id = .data$subject_id
    ) |>
    dplyr::select(
      "subject_id",
      "study_id",
      "group_label",
      "group_replicate",
      "extra_factors"
    )

  samplelogdf <- res |>
    dplyr::select("subject_id", "time_points") |>
    dplyr::mutate(
      nominal_time = as.character(.data$time_points),
      time_unit = time_unit,
      status = "Planned"
    ) |>
    dplyr::select(-"time_points")

  new_study <- create_new_study(
    data.frame(
      type = "SD",
      title = study_title,
      description = paste(
        "Cell stability study with",
        length(cmpds),
        "compounds",
        length(arms),
        "arms and",
        length(conditions),
        "conditions",
        "Compounds:",
        paste(cmpds, collapse = ", "),
        n_replicates,
        "replicates each"
      ),
      pkstudy = FALSE,
      subject_type = "InVitro"
    )
  )
  cli::cli_alert_info("Created study with ID: {new_study$id}")
  add_dosing_db(
    study_id = new_study$id,
    df = data.frame(
      group_label = unique(subjectsdf$group_label),
      period_number = 1,
      dose_freq = NA_real_,
      dose_addl = NA_integer_,
      dose_amount = NA_real_,
      dose_unit = NA_character_,
      route = NA_character_,
      formulation = NA_character_
    )
  )
  cli::cli_alert_success("Dosing information added.")
  cli::cli_alert_info("Adding subjects information...")
  add_subjects_db(study_id = new_study$id, df = subjectsdf)

  cli::cli_alert_success("Subjects information added.")
  cli::cli_alert_info("Adding sample log information...")
  add_sample_log(study_id = new_study$id, df = samplelogdf)

  cli::cli_alert_success("Sample log information added.")
  new_study
}

# make_cell_stability_study()
