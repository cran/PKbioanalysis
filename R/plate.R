# Define the constructor function for PlateObj
PlateObj <- function(
  m,
  df,
  samples_metadata = data.frame(),
  plate_id,
  empty_rows = NULL,
  filling_scheme,
  last_modified = Sys.time(),
  descr = ""
) {
  df <- df

  # get last filled well within the active rows
  last_filled_i <- which(
    is.na(m[empty_rows, ]),
    arr.ind = TRUE,
    useNames = TRUE
  )

  # if there is only one active row, R will return a vector not a matrix
  if (length(last_filled_i) == 0) {
    # no empty spots
    last_filled = NA_character_
  } else if (length(empty_rows) > 1) {
    # multiple active rows (matrix)
    last_filled <- last_filled_i[
      which(last_filled_i[, 1] == min(last_filled_i[, 1])),
      ,
      drop = FALSE
    ] # smallest row
    last_filled <- last_filled[
      which(last_filled[, 2] == min(last_filled[, 2])),
      ,
      drop = FALSE
    ] # smallest col
    last_filled <- paste(rownames(last_filled), last_filled[, 2], sep = ",")
  } else if (length(empty_rows) == 1) {
    # one active row (vector)
    last_filled <- paste(empty_rows, min(last_filled_i) - 1, sep = ",")
  }

  new(
    "PlateObj",
    plate = m,
    df = df,
    samples_metadata = samples_metadata,
    empty_rows = empty_rows,
    filling_scheme = filling_scheme,
    last_filled = last_filled,
    last_modified = last_modified,
    plate_id = plate_id,
    descr = descr
  )
}


#' Generate 96 well plate
#'
#' @param descr plate description.
#' @param start_row A letter corresponding to empty rows in a 96 well plate. Default is A.
#' @param start_col A number indicating a column number to start with, given the start row. Default is 1.
#'
#' Generate a typical 96 well plate. User need to specify the empty rows which a going to be used across the experiment.
#'
#' @importFrom dplyr slice_tail
#' @importFrom tidyr pivot_longer
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96()
#' plot(plate)
#'
#' plate <- generate_96("calibration", start_row = "C", start_col = 11)
#' plot(plate)
#'
generate_96 <- function(descr = "", start_row = "A", start_col = 1) {
  checkmate::assertSubset(start_row, choices = LETTERS[1:8])
  checkmate::assertString(start_row)
  checkmate::assertNumber(start_col, lower = 1, upper = 12)

  # find position of start_row
  empty_rows <- match(start_row, LETTERS[1:8])
  empty_rows <- LETTERS[empty_rows:8]

  m <- matrix(NA, nrow = 8, ncol = 12)
  rownames(m) <- LETTERS[1:8]

  extra_fill <- start_col - 1
  m[which(!(rownames(m) %in% empty_rows)), ] <- "X"
  m[empty_rows[1], seq_len(extra_fill)] <- "X"

  df <- as.data.frame(m) |>
    dplyr::mutate(row = 1:8) |>
    tidyr::pivot_longer(-row, names_to = "col", values_to = "value") |>
    dplyr::mutate(col = as.integer(str_remove(.data$col, "V"))) |>
    dplyr::mutate(
      SAMPLE_LOCATION = paste0(LETTERS[.data$row], ",", .data$col)
    ) |>
    dplyr::mutate(samples = as.character(NA)) |>
    dplyr::mutate(log_id = as.character(NA)) |>
    dplyr::mutate(study_id = as.character(NA)) |>
    dplyr::mutate(conc = as.numeric(NA)) |>
    dplyr::mutate(dil = as.numeric(NA)) |>
    # dplyr::mutate(time = as.character(NA)) |>
    # dplyr::mutate(sex = as.character(NA)) |>
    # dplyr::mutate(factor = as.character(NA)) |>
    # dplyr::mutate(dose = as.character(NA)) |>
    # dplyr::mutate(dose_unit = as.character(NA)) |>
    # dplyr::mutate(II = as.numeric(NA)) |>
    # dplyr::mutate(addl = as.numeric(NA)) |>
    # dplyr::mutate(route = as.character(NA)) |>
    # dplyr::mutate(cmt = as.character(NA)) |>
    dplyr::mutate(TYPE = as.character(NA)) |>
    dplyr::mutate(a_group = as.character(NA)) |>
    dplyr::mutate(std_rep = as.numeric(NA)) |>
    dplyr::mutate(e_rep = as.numeric(NA))

  plates_ids <- .compile_cached_plates()

  if (length(plates_ids) == 0) {
    plate_id <- "1_1"
  } else {
    plate_id <- str_split(plates_ids, "_") |>
      sapply(function(x) x |> _[1]) |> # get plate_id, ignore exp id
      as.numeric() |>
      {
        \(x) max(x) + 1
      }() |>
      as.character() |>
      paste0("_1")
  }

  # .plate(m, df, plate_id, empty_rows, descr = descr)
  PlateObj(
    m = m,
    df = df,
    samples_metadata = data.frame(),
    plate_id = plate_id,
    empty_rows = empty_rows,
    descr = descr,
    filling_scheme = list(
      scheme = "h",
      tbound = "A",
      bbound = "H",
      lbound = 1,
      rbound = 12
    )
  )
}


.add_samples_dispatch <- function(
  samples_df,
  plate_obj,
  samples,
  rep = 1,
  prefix = "S"
) {
  checkmate::assertVector(samples, unique = FALSE)

  checkmate::assertNumber(rep, lower = 1, upper = 100)
  checkmate::assertString(prefix, na.ok = TRUE)
  checkmate::assertClass(plate_obj, "PlateObj")

  df <- plate_obj@df
  plate <- plate_obj@plate
  empty_rows <- plate_obj@empty_rows

  samples_df <- samples_df |>
    dplyr::arrange(
      .data$group,
      .data$samples,
      .data$dil
    )

  samples_df <- samples_df[rep(seq_len(nrow(samples_df)), times = rep), ]

  samples_df <- samples_df |>
    dplyr::mutate(
      samples = as.character(.data$samples),
      group = as.character(.data$group),
      dil = as.numeric(.data$dil)
    ) |>
    rowwise() |>
    dplyr::mutate(
      value = ifelse(
        !is.null(prefix),
        paste0(prefix, .data$samples),
        .data$samples
      )
    ) |>

    dplyr::mutate(
      value = ifelse(
        !is.na(.data$dil),
        paste0(.data$value, "_", .data$dil, "X"),
        .data$value
      )
    )

  empty_spots <- .spot_mask(plate_obj)

  new_df <- df[FALSE, ]
  for (i in seq_along(nrow(samples_df))) {
    plate[empty_spots[i, 1], empty_spots[i, 2]] <- samples_df$samples[i]
    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value = samples_df$value[i],
        SAMPLE_LOCATION = paste0(
          LETTERS[empty_spots[i, 1]],
          ",",
          empty_spots[i, 2]
        ),
        samples = samples_df$samples[i],
        a_group = samples_df$group[i],
        conc = NA,
        std_rep = NA,
        e_rep = .last_entity(plate_obj, "Analyte") + 1
      )
    )
  }

  # keep only the samples, other NA
  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj
}


#' Add samples to plate with pharmacokinetic attributes
#'
#' @param plate PlateObj
#' @param samples A vector representing samples names. Must be unique.
#' @param prefix A prefix to be added before samples names. Default is "Sub".
#' @param dil A vector representing samples' dilution factor. Must be same length as samples.
#' @param group A vector representing samples' bioanalytical group. Must be same length as samples.
#' @param rep Number of technical replicates for each combination. Default is 1.
#'
#'
#' @details final name will be of form. Prefix-SampleName-Time-Concentration-Factor
#' samples must be a unique vector and did not exist in the plate before.
#' Time is either a vector or a single value. If it is a vector, it will be repeated for each sample.
#' Conc, dil, factor and dose are either a vector or a single value. If it is a vector, it must be the corrosponding length of samples.
#'
#' Allowed routes are "IV", "IM", "IP", "SC", "PO", "INH" which are short for Intravenous, Intramuscular, Intraperitoneal, Subcutaneous, Per Os (oral), Inhalation.
#'
#' Factor is an arbitrary factor used in the design like food vs fasted, healthy vs diseased, positive genotype ... etc.
#'
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#'  add_samples(paste0("T", 1:12))
add_samples <- function(
  plate,
  samples,
  prefix = NA,
  dil = NA,
  group = NA,
  rep = 1
) {
  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate
  empty_rows <- plate_obj@empty_rows

  # ensure new IDs does not exist in the plate
  if (any(samples %in% df$samples)) {
    # print them
    stop(paste(
      "Samples already exist in the plate.",
      samples[samples %in% df$samples],
      "... Please use different names or remove them from the plate."
    ))
  }

  samples_df <- data.frame(samples = samples, group = group, dil = dil)

  .add_samples_dispatch(
    samples_df = samples_df,
    plate_obj = plate_obj,
    samples = samples,
    prefix = prefix,
    rep = rep
  )
}


# #' Cartesian product of sample factors to a plate
# #' @param plate PlateObj
# #' @param n_rep number of samples to be added
# #' @param time A vector representing time points
# #' @param conc A vector representing concentration
# #' @param factor A vector representing factor
# #' @param dose A vector representing dose
# #' @param prefix A prefix to be added before samples names. Default is "S"
# #'
# #' @returns PlateObj
# #' @details This function is a variation of `add_samples()` where size of inputs does not matter.
# #' The function will automatically create a combination of all sample names with time, concentration and factor.
# #' final name will be of form. Prefix-SampleName-Time-Concentration-Factor
# #' @export
# add_samples_c <- function(
#   plate,
#   n_rep,
#   time = NA,
#   conc = NA,
#   factor = NA,
#   dose = NA,
#   prefix = "S"
# ) {
#   checkmate::assertNumber(n_rep, lower = 1, finite = TRUE)
#   checkmate::assertNumeric(time, null.ok = FALSE)
#   checkmate::assertNumeric(conc, null.ok = FALSE)
#   checkmate::assertVector(factor, null.ok = FALSE)
#   checkmate::assertVector(dose, null.ok = FALSE)

#   last_unique <- plate@df$samples |> as.numeric() |> unique() |> length()
#   samples <- seq_len(n_rep) + last_unique
#   combined <- expand.grid(
#     samples = samples,
#     time = time,
#     conc = conc,
#     factor = factor,
#     dose = dose
#   ) |>
#     dplyr::arrange(
#       .data$samples,
#       .data$dose,
#       .data$factor,
#       .data$conc,
#       .data$time
#     ) |>
#     dplyr::group_by(.data$samples, .data$factor, .data$dose, .data$conc) |>
#     dplyr::mutate(samples = dplyr::cur_group_id() + last_unique)

#   plate |>
#     add_samples(
#       samples = unique(combined$samples),
#       prefix = prefix
#     )
# }

#' Add samples from the sample log to the plate
#' @param plate PlateObj
#' @param logIds A vector of log IDs from the sample log.
#' @param dil A vector with length corresponding number of logIds. See details.
#' @param namestyle A numeric value indicating the naming style. 1 for long names, 2 for short names.
#' @param group A string for bioanalytical group.
#' @details This function will retrieve sample information from the sample log database using the provided log IDs.
#' It constructs sample names based on the specified naming style and adds them to the plate.
#' The `dil` parameter allows specifying dilution factors for each sample, which will be appended to the sample names.
#' If a single dilution factor is provided, it will be applied to all samples.
#' @export
#' @returns PlateObj
add_samples_db <- function(plate, logIds, dil = 1, namestyle = 1, group = NA) {
  checkmate::assertVector(logIds, min.len = 1, any.missing = FALSE)
  checkmate::assertNumeric(dil, lower = 1, finite = TRUE)
  if (length(dil) == 1) {
    dil <- rep(dil, length(logIds))
  }
  stopifnot(length(dil) == length(logIds))

  samplesdf <- retrieve_full_log_by_id(logIds) |>
    dplyr::mutate(
      nominal_time = ifelse(
        !is.na(.data$nominal_time),
        paste0("T", .data$nominal_time),
        .data$nominal_time
      )
    ) |>
    dplyr::mutate(
      dose_amount = ifelse(
        !is.na(.data$dose_amount),
        paste0(.data$dose_amount, .data$dose_unit),
        .data$dose_amount
      )
    ) |>
    dplyr::mutate(dilStr = paste0(dil, "X"))

  # reorder to original order
  samplesdf <- samplesdf[match(logIds, samplesdf$log_id), ]

  plateobj <- plate
  df <- plate@df
  plate <- plate@plate
  empty_rows <- plateobj@empty_rows

  if (namestyle == 1) {
    order <- c(
      "title",
      "group_label", # ARM
      "subject_id",
      "sex",
      "extra_factors",
      "nominal_time",
      "sample_type",
      "dose_amount",
      "dose_freq",
      "route",
      "formulation",
      "dilStr"
    )
  } else if (namestyle == 2) {
    order <- c(
      "group_label",
      "subject_id",
      "sex",
      "extra_factors",
      "nominal_time",
      "dose_amount",
      "dilStr"
    )
  } else {
    stop("namestyle must be either 1 or 2")
  }

  samplesdf$values <- apply(samplesdf[, order], 1, function(row) {
    paste(na.omit(row), collapse = "_")
  })

  empty_spots <- .spot_mask(plateobj)
  .check_feasible_adding(plateobj, empty_spots, nrow(samplesdf))

  new_df <- df[FALSE, ]
  for (i in seq_len(nrow(samplesdf))) {
    plate[empty_spots[i, 1], empty_spots[i, 2]] <- samplesdf$values[i]

    new_row <- data.frame(
      row = empty_spots[i, 1],
      col = empty_spots[i, 2],
      value = samplesdf$values[i],
      study_id = samplesdf$study_id[i],
      log_id = samplesdf$log_id[i],
      a_group = group,
      dil = dil[i],
      TYPE = "Analyte",
      e_rep = .last_entity(plateobj, "Analyte") + 1
    )
    new_df <- rbind(new_df, new_row)
  }
  stopifnot(nrow(new_df) == length(logIds))
  df <- .bind_new_samples(df, new_df)

  plateobj@df <- df
  plateobj@plate <- plate
  plateobj@samples_metadata <- rbind(plateobj@samples_metadata, samplesdf)

  validObject(plateobj)
  plateobj
}

#' Add samples from the sample log to the plate with multiplication
#' @param plate PlateObj
#' @param logIds A vector of log IDs from the sample log.
#' @param dil A vector with length corresponding number of repeats. See details.
#' @param namestyle A numeric value indicating the naming style. 1 for long names, 2 for short names.
#' @param group A string for bioanalytical group.
#' @details This function is wrapper around `add_samples_db()` that allows for quick replication of samples by dilution factor vector. 
#' For instance, it dil = c(1,10), the samples will repeated twice with one fold and 10 fold dilution factor each time.
add_samples_db2 <- function(plate, logIds, dil = c(1,1), namestyle = 1, group = NA) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertVector(logIds, min.len = 1, any.missing = FALSE)
  checkmate::assertNumeric(dil, lower = 1, finite = TRUE, any.missing = FALSE)
  checkmate::assertString(group, na.ok = TRUE)
  checkmate::assertChoice(namestyle, choices = c(1, 2))

  for (i in seq_along(dil)) {
    plate <- add_samples_db(plate, logIds, dil = dil[i], namestyle = namestyle, group = group)
  }

  plate

}

#' Add blank to the plate
#' Can be either double blank (DB), CS0IS+ or CS+IS0
#' @param plate PlateObj object
#' @param IS logical. If TRUE, add IS to the well.
#' @param analyte logical. If TRUE, add analyte to the well.
#' @param analytical logical. If FALSE, the blank is analytical, if TRUE it is bioanalytical.
#' @param group A string for bioanalytical group.
#'
#' @import stringr
#' @returns PlateObj
#' @export
add_blank <- function(
  plate,
  IS = TRUE,
  analyte = FALSE,
  analytical = FALSE,
  group = NA
) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertLogical(IS)
  checkmate::assertLogical(analyte)
  checkmate::assertString(group, na.ok = TRUE)

  if (IS == FALSE & analyte == FALSE) {
    blank_vec <- "DB" # CS0IS0
  }
  if (IS == TRUE & analyte == FALSE) {
    blank_vec <- "CS0IS+"
  }
  if (IS == FALSE & analyte == TRUE) {
    blank_vec <- "CS1IS-"
  }
  if (IS == TRUE & analyte == TRUE) {
    stop("You cannot have both IS and analyte as TRUE")
  }
  if (analytical) {
    blank_vec <- paste0(blank_vec, "M-")
  }

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate

  empty_spots <- .spot_mask(plate_obj)

  plate[empty_spots[1, 1], empty_spots[1, 2]] <- blank_vec
  new_df <- data.frame(
    row = empty_spots[1, 1],
    col = empty_spots[1, 2],
    value = blank_vec,
    SAMPLE_LOCATION = paste0(
      LETTERS[empty_spots[1, 1]],
      ",",
      empty_spots[1, 2]
    ),
    conc = as.numeric(case_when(
      stringr::str_detect(blank_vec, "DB") ~ 0,
      stringr::str_detect(blank_vec, "CS0IS+") ~ 0,
      stringr::str_detect(blank_vec, "CS1IS-") ~ 1
    )),
    TYPE = case_when(
      stringr::str_detect(blank_vec, "DB") ~ "DoubleBlank",
      stringr::str_detect(blank_vec, "CS0IS+") ~ "Blank",
      stringr::str_detect(blank_vec, "CS1IS-") ~ "ISBlank",
    ),
    dil = 1,
    a_group = group,
    std_rep = NA,
    e_rep = .last_entity(plate_obj, "Blank") + 1
  )

  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj
}

#' Add double blank (DB) to a plate
#' @param plate PlateObj object
#' @param analytical logical. If TRUE, the blank is bioanalytical, if FALSE it is analytical.
#' @param group A string for bioanalytical group.
#'
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#' add_DB()
add_DB <- function(plate, analytical = FALSE, group = NA) {
  checkmate::assertClass(plate, "PlateObj")

  add_blank(
    plate,
    IS = FALSE,
    analyte = FALSE,
    analytical = analytical,
    group = group
  )
}

#' Add calibration curve to the plate
#'
#' @param plate PlateObj
#' @param plate_std character
#' @param rep numeric. Number of technical replicates. Default is 1.
#' @param group A string for bioanalytical group.
#'
#' @export
#'
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#'  add_cs_curve(c(1, 3, 5, 10, 50, 100, 200))
#' plot(plate)
add_cs_curve <- function(plate, plate_std, rep = 1, group = NA) {
  checkmate::assertNumeric(plate_std, lower = 0.01, finite = TRUE)
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertNumber(rep, lower = 1, upper = 20)
  checkmate::assertString(group, na.ok = TRUE)

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate

  std_rep <- rep(seq(rep), length(plate_std)) |> sort()

  plate_std <- paste0("CS", seq_along(plate_std), "_", plate_std)
  plate_std <- rep(plate_std, rep)

  empty_spots <- .spot_mask(plate_obj)
  .check_feasible_adding(plate_obj, empty_spots, length(plate_std))

  new_df <- df[FALSE, ]

  for (i in seq_along(plate_std)) {
    plate[empty_spots[i, 1], empty_spots[i, 2]] <- plate_std[i]
    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value = plate_std[i],
        SAMPLE_LOCATION = paste0(
          LETTERS[empty_spots[i, 1]],
          ",",
          empty_spots[i, 2]
        ),
        conc = as.numeric(str_extract(plate_std[i], "(\\d*\\.?\\d+)$")),
        dil = 1,
        TYPE = "Standard",
        a_group = group,
        std_rep = std_rep[i],
        e_rep = .last_entity(plate_obj, "Standard") + 1
      )
    )
  }

  # add sample to the df
  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj
}


#' Add dilution quality control (DQC) to the plate
#' @param plate PlateObj object
#' @param conc numeric. Concentration of the DQC well.
#' @param fac numeric. Factor of the DQC well.
#' @param rep numeric. Number of replicates. Default is 5.
#' @param group A string for bioanalytical group.
#'
#' The current implementation does not check ULOQ or LLOQ boundaries.
#'
#' @export
add_DQC <- function(plate, conc, fac, rep = 5, group = NA) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertNumeric(conc, finite = TRUE, lower = 0)
  checkmate::assertNumeric(fac, finite = TRUE, lower = 1.1)
  checkmate::assertNumber(rep, lower = 1, upper = 100)
  checkmate::assertString(group, na.ok = TRUE)

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate

  empty_spots <- .spot_mask(plate_obj)
  .check_feasible_adding(plate_obj, empty_spots, 5)

  new_df <- df[FALSE, ]

  for (i in seq_len(rep)) {
    val_label <- paste0("DQC_", conc, "_", fac, "X")
    plate[empty_spots[i, 1], empty_spots[i, 2]] <- val_label
    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value = val_label,
        SAMPLE_LOCATION = paste0(
          LETTERS[empty_spots[i, 1]],
          ",",
          empty_spots[i, 2]
        ),
        conc = as.numeric(conc),
        factor = as.character(fac),
        TYPE = "DQC",
        dil = fac,
        std_rep = i,
        a_group = group,
        e_rep = .last_entity(plate_obj, "DQC") + 1
      )
    )
  }

  # add sample to the df
  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj
}

#' Get last filled rank of entity in the plate
#' @param plate PlateObj object
#' @param entity character. Name of the entity to be checked.
#' @returns integer. Last filled rank of the entity.
#' @noRd
.last_entity <- function(plate, entity) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertCharacter(entity)

  suppressWarnings({
    n <- plate@df |>
      dplyr::filter(.data$TYPE == entity) |>
      pull(.data$e_rep) |>
      max(na.rm = TRUE)
  })

  ifelse(is.finite(n), n, 0)
}


#' Add suitability sample to the plate
#' @param plate PlateObj object.
#' @param conc numeric. Concentration of the suitability well.
#' @param label character. Label for the suitability well. Default is "suitability".
#' @param group A string for bioanalytical group.
#' @importFrom dplyr bind_rows mutate slice_tail
#' @returns PlateObj
#' @export
add_suitability <- function(plate, conc, label = "suitability", group = NA) {
  checkmate::assertCharacter(label)
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertNumeric(conc, finite = TRUE, lower = 0)

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate

  empty_spots <- .spot_mask(plate_obj)

  new_df <- df[FALSE, ]

  plate[empty_spots[1, 1], empty_spots[1, 2]] <- label
  new_df <- dplyr::bind_rows(
    new_df,
    data.frame(
      row = empty_spots[1, 1],
      col = empty_spots[1, 2],
      value = paste0(label, "_", conc),
      SAMPLE_LOCATION = paste0(
        LETTERS[empty_spots[1, 1]],
        ",",
        empty_spots[1, 2]
      ),
      conc = as.numeric(conc),
      factor = as.character(1),
      a_group = group,
      TYPE = "Suitability"
    )
  )

  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj
}

#' Check the quality control samples valid
#' The function will be strict for LQC, but will give a warning only for MQC and HQC
#' @param std_vec vector of calibration standards
#' @param loq_conc limit of quantification
#' @param lqc_conc low quality control concentration
#' @param mqc_conc  medium quality control concentration
#' @param hqc_conc high quality control concentration
#' @param reg logical. Indicates if restrictions should be applied to the QC samples. Default is TRUE
#' @returns PlateObj
#' @noRd
.check_qcs <- function(std_vec, loq_conc, lqc_conc, mqc_conc, hqc_conc, reg) {
  checkmate::assertNumeric(loq_conc, lower = 0)
  checkmate::assertNumeric(lqc_conc, lower = loq_conc)
  checkmate::assertNumeric(mqc_conc, lower = lqc_conc)
  checkmate::assertNumeric(hqc_conc, lower = mqc_conc)

  # find the 30%, 50% and 75% cut on the calibration range
  min_val <- as.numeric(loq_conc)
  max_val <- max(as.numeric(std_vec))
  quantrange <- quantile(c(min_val, max_val), c(0.30, 0.50, 0.75))

  e_func <- ifelse(reg, stop, warning)

  if (!(lqc_conc <= loq_conc * 3)) {
    e_func(paste("LQC should be less or equal 3xLOQ (<", loq_conc * 3), ")")
  }

  if (!(mqc_conc >= quantrange[1] & mqc_conc <= quantrange[2])) {
    e_func(paste(
      "MQC should be between 30% (",
      quantrange[1],
      ")and 50% (",
      quantrange[2],
      ") of the calibration range"
    ))
  }
  if (!(hqc_conc >= quantrange[3] & hqc_conc <= max_val)) {
    e_func(paste(
      "HQC should be equal or greater than 75% (>=",
      quantrange[3],
      ") of the calibration range. Max is ", max_val
    ))
  }
}


#' Add quality control samples to the plate
#' @param plate PlateObj object
#' @param lqc_conc low quality control concentration
#' @param mqc_conc medium quality control concentration
#' @param hqc_conc high quality control concentration
#' @param extra numeric vector of extra QC concentrations.
#' @param n_qc number of QC sets. Default is 3
#' @param qc_serial logical. If TRUE, QCs are placed serially
#' @param reg logical. Indicates if restrictions should not be applied to the QC samples. Default is TRUE
#' @param group A string for bioanalytical group.
#' @description
#' A function to add QCs to plate. This function assumes adherence to
#' ICH guideline M10 on bioanalytical method validation and study sample analysis Geneva, Switzerland (2022).
#' If you are not following this guideline, you can set `reg = TRUE` to ignore the restrictions.
#' @returns PlateObj
#' @export
add_QC <- function(
  plate,
  lqc_conc,
  mqc_conc,
  hqc_conc,
  extra = NULL,
  n_qc = 3,
  qc_serial = TRUE,
  reg = TRUE,
  group = NA
) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertLogical(qc_serial)
  checkmate::assertLogical(reg)
  checkmate::assertNumeric(n_qc, lower = 1, finite = TRUE)
  checkmate::assertNumeric(extra, null.ok = TRUE, lower = 0)
  checkmate::assertString(group, na.ok = TRUE)

  # assert there was a standard call, and get the last call
  grp_std <- .last_entity(plate, "Standard")

  if (grp_std == 0) {
    stop("The plate does not have any standards. Use add_cs_curve")
  }

  # assert there is only no qc associated with last standard
  grp_qc <- .last_entity(plate, "QC")
  if (grp_qc == grp_std) {
    warning("There is already a QC associated with the last standard")
  }

  # stop if QC for that group exist
  grps_with_qcs <- plate@df |>
    dplyr::filter(.data$TYPE == "QC") |>
    dplyr::pull(.data$a_group) |>
    unique()
  if (group %in% grps_with_qcs) {
    stop("QC for this group already exists. Add new group")
  }

  # get the lloq from the last call
  plate_std <- plate@df |>
    dplyr::filter(.data$TYPE == "Standard", .data$e_rep == grp_std) |>
    dplyr::pull(.data$conc)
  loq_conc <- plate_std |>
    as.numeric() |>
    min(na.rm = TRUE)

  stopifnot(is.numeric(loq_conc) & loq_conc > 0)

  .check_qcs(plate_std, loq_conc, lqc_conc, mqc_conc, hqc_conc, reg)
  checkmate::assertLogical(qc_serial)
  checkmate::assertNumeric(n_qc, lower = 0)

  plate_obj <- plate
  df <- plate@df
  plate <- plate@plate
  empty_rows <- plate_obj@empty_rows

  empty_spots <- .spot_mask(plate_obj)
  .check_feasible_adding(plate_obj, empty_spots, 4 * n_qc)

  new_df <- df[FALSE, ]

  dil <- 1
  dil_label <- function(x) {
    ifelse(dil == 1, x, paste0(dil, "X_", x))
  }

  vec_qc_names <- c(
    glue::glue("QC1_LLOQ_{dil_label(loq_conc)}"),
    glue::glue("QC2_LQC_{dil_label(lqc_conc)}"),
    glue::glue("QC3_MQC_{dil_label(mqc_conc)}"),
    glue::glue("QC4_HQC_{dil_label(hqc_conc)}")
  )

  if (qc_serial) {
    vec_qc_names <- rep(vec_qc_names, each = n_qc)
    qc_reps <- rep(seq_len(n_qc), times = 4)
  } else {
    vec_qc_names <- rep(vec_qc_names, n_qc)
    qc_reps <- rep(seq_len(n_qc), each = 4)
  }

  target <- empty_spots[1:(4 * n_qc), ]
  target <- target[order(target[, 1]), ]

  for (i in seq_along(target[, 1])) {
    plate[target[i, 1], target[i, 2]] <- vec_qc_names[i]

    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value = vec_qc_names[i],
        SAMPLE_LOCATION = paste0(
          LETTERS[empty_spots[i, 1]],
          ",",
          empty_spots[i, 2]
        ),
        conc = as.numeric(str_extract(vec_qc_names[i], "(\\d*\\.?\\d+)$")),
        dil = 1,
        TYPE = "QC",
        a_group = group,
        std_rep = qc_reps[i],
        e_rep = .last_entity(plate_obj, "QC") + 1
      )
    )
  }

  df <- .bind_new_samples(df, new_df)

  plate_obj@df <- df
  plate_obj@plate <- plate

  validObject(plate_obj)
  plate_obj
}


#' Create a calibration study with calibration standards and QCs
#'
#' @param plate PlateObj object
#' @param plate_std vector of calibration standards
#' @param lqc_conc LQC concentration
#' @param mqc_conc MQC concentration
#' @param hqc_conc HQC concentration
#' @param n_qc number of QC sets
#' @param qc_serial logical. If TRUE, QCs are placed serially
#' @param n_CS0IS0 number of CS0IS0 (double) blanks
#' @param n_CS0IS1 number of CS0IS1 blanks
#' @param n_CS1IS0 number of CS1IS0 blanks
#' @param group A string for bioanalytical group.
#'
#' @import stringr
#'
#' @returns PlateObj
#' @export
make_calibration_study <-
  function(
    plate,
    plate_std,
    lqc_conc = NULL,
    mqc_conc = NULL,
    hqc_conc = NULL,
    n_qc = NULL,
    qc_serial = FALSE,
    n_CS0IS0 = 1,
    n_CS0IS1 = 2,
    n_CS1IS0 = 1,
    group = NA
  ) {
    checkmate::assertClass(plate, "PlateObj")
    checkmate::assertVector(plate_std)

    checkmate::assertNumeric(n_CS0IS0)
    checkmate::assertNumeric(n_CS0IS1)
    checkmate::assertNumeric(n_CS1IS0)

    for (i in seq(n_CS0IS0)) {
      plate <- add_blank(plate, IS = FALSE, analyte = FALSE, group = group)
    }

    for (i in seq(n_CS1IS0)) {
      plate <- add_blank(plate, IS = FALSE, analyte = TRUE, group = group)
    }

    for (i in seq(n_CS0IS1)) {
      plate <- add_blank(plate, IS = TRUE, analyte = FALSE, group = group)
    }

    plate <- add_cs_curve(plate, plate_std, rep = 1, group = group)

    if (
      !is.null(lqc_conc) &
        !is.null(mqc_conc) &
        !is.null(hqc_conc) &
        !is.null(n_qc)
    ) {
      if (n_qc != 0) {
        plate <- plate |>
          add_QC(
            lqc_conc = lqc_conc,
            mqc_conc = mqc_conc,
            hqc_conc = hqc_conc,
            n_qc = n_qc,
            qc_serial = qc_serial
          )
      }
    }

    plate
  }


#' Plotting 96 well plate
#'
#' @param x PlateObj
#' @param color character. Coloring variable. Choices: "conc", "group", "dil", "study", "time", "factor", "samples", "arm", "sex", "dose", "route", "matrix". Default is "conc"
#' @param Instrument A string placed at subtitle
#' @param caption A string place at plate caption
#' @param label_size numeric. Size of the label. Default is 15
#' @param transform_dil logical. If TRUE, transform the dilution factor to the label
#' @param watermark character. If "auto", a watermark is added to the plot. If "none", no watermark is added. Default is "auto"
#' @param layoutOverlay logical. If TRUE, overlay the plot layout. Default is FALSE
#' @param path If not null, must be a path to save plate image
#' @param ... additional arguments passed to ggplot2::ggsave
#'
#' @importFrom ggplot2 coord_equal scale_fill_discrete scale_x_continuous scale_y_continuous geom_text labs theme_minimal theme expand_limits
#' @importFrom ggforce geom_circle
#' @importFrom glue glue
#' @export
#' @returns ggplot object
#'
#' @examples
#' plate <- generate_96("new_plate", "C", 11) |>
#'   add_blank(IS = FALSE, analyte = FALSE) |>
#'   add_blank(IS = TRUE, analyte = FALSE) |>
#'   add_samples(c(
#'     "RD_per1", "RD_in1", "RD_T30", "RD_T60", "RD_T90", "RD_per2", "RD_in2",
#'     "EE_in0", "EE_T30", "EE_in30", "EE_T60", "EE_in60", "EE_T90", "EE_in90"
#'   ))
#' plot(plate)
plot.PlateObj <- function(
  x,
  color = "conc",
  Instrument = "",
  caption = "",
  label_size = 1,
  transform_dil = FALSE,
  watermark = "auto",
  layoutOverlay = FALSE,
  path = NULL,
  ...
) {
  plate <- x
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertChoice(
    color,
    c(
      "conc",
      "group",
      "dil",
      "study",
      "time",
      "factor",
      "samples",
      "arm",
      "sex",
      "dose",
      "route",
      "matrix"
    )
  )
  checkmate::assertCharacter(Instrument)
  checkmate::assertCharacter(caption)
  checkmate::assertCharacter(path, null.ok = TRUE)

  descr <- plate@descr
  plate_df <- plate@df |> # zero if blanks, NA if empty cell. Conc otherwise
    mutate(conc = as.character(.data$conc))

  # mutate(time = as.character(.data$time))  |>
  # mutate(dose = as.character(.data$dose)) |>
  # mutate(factor = as.character(.data$factor))

  samples_on_plate <- na.omit(unique(plate@df$log_id))
  if (length(samples_on_plate) > 0) {
    plate_df <- left_join(
      plate_df |> dplyr::mutate(dilStr = paste0(.data$dil, "X")),
      plate@samples_metadata,
      by = c("log_id", "study_id", "dilStr"),
      relationship = "one-to-one"
    )
  }
  color_actual <- switch(
    color,
    "conc" = "conc",
    "group" = "a_group",
    "dil" = "dil",

    "study" = "title",
    "time" = "nominal_time",
    "factor" = "extra_factors",
    "samples" = "subject_id",
    "arm" = "group_label",
    "sex" = "sex",
    "dose" = "dose_amount",
    "route" = "route",
    "matrix" = "sample_type"
  )

  # remove bottle if there
  plate_df$SAMPLE_LOCATION <-
    gsub("^.*:", "", plate_df$SAMPLE_LOCATION)

  date <- plate@last_modified |> as.Date()

  if (transform_dil) {
    plate_df$new_value <- str_replace_all(
      plate_df$value,
      "(\\d+X)",
      paste0(">", as.numeric(plate_df$conc) / plate_df$dil)
    )
  } else {
    plate_df$new_value <- str_replace_all(
      plate_df$value,
      "(\\d+X)",
      paste0(">", "\\1")
    )
  }
  plate_df <- plate_df |> 
    dplyr::mutate(new_value = str_replace_all(.data$new_value, "_", "\n")) |> 
    dplyr::mutate(dil = factor(.data$dil, levels = unique(.data$dil[order(as.numeric(.data$dil))])))

  currentLayout <- x@filling_scheme
  rbound <- currentLayout$rbound
  lbound <- currentLayout$lbound
  tbound <- currentLayout$tbound # char
  bbound <- currentLayout$bbound # char
  scheme <- currentLayout$scheme
  fig <- ggplot2::ggplot(data = plate_df) +
    list(
      if (layoutOverlay) {
        ggplot2::annotate(
          "rect",
          xmin = rbound,
          xmax = lbound,
          ymin = which(LETTERS == tbound),
          ymax = which(LETTERS == bbound),
          alpha = 0.2,
          color = 'yellow',
          fill = 'yellow'
        )
      },
      if (layoutOverlay & scheme == 'h') {
        y_positions <- (which(LETTERS == tbound):which(LETTERS == bbound)) - 0.5
        ggplot2::annotate(
          'segment',
          x = rep(lbound, length(y_positions)),
          y = y_positions,
          xend = rep(rbound, length(y_positions)),
          yend = y_positions,
          linetype = "dashed",
          arrow = ggplot2::arrow(type = "open", length = unit(0.2, "inches"))
        )
      },
      if (layoutOverlay & scheme == 'v') {
        x_positions <- (lbound:rbound) - 0.5
        ggplot2::annotate(
          'segment',
          x = x_positions,
          y = rep((which(LETTERS == tbound)) - 0.5, length(x_positions)),
          xend = x_positions,
          yend = rep((which(LETTERS == bbound)) - 0.5, length(x_positions)),
          linetype = "dashed",
          arrow = ggplot2::arrow(type = "open", length = unit(0.2, "inches"))
        )
      }
    ) +
    ggforce::geom_circle(
      aes(
        x0 = .data[["col"]],
        y0 = .data[["row"]],
        r = 0.45,
        fill = .data[[color_actual]],
        color = .data[["TYPE"]]
      ),
      linewidth = 1,
      alpha = 0.8,
      linetype = "solid"
    ) +
    # make unique colors for fill vs color
    ggplot2::scale_color_viridis_d(na.translate = FALSE) +
    ggplot2::scale_fill_discrete(na.translate = TRUE) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(
      breaks = 1:12,
      expand = expansion(mult = c(0.01, 0.01)),
      sec.axis = sec_axis(~., breaks = 1:12)
    ) +
    ggplot2::scale_y_continuous(
      breaks = 1:8,
      labels = LETTERS[1:8],
      sec.axis = sec_axis(
        ~.,
        name = "row",
        labels = LETTERS[1:8],
        breaks = 1:8
      ),
      expand = expansion(mult = c(0.01, 0.01)),
      trans = "reverse"
    ) + # reverse the y-axis
    # text
    ggplot2::geom_text(
      aes(
        x = .data$col,
        y = .data$row,
        label = .data$new_value,
      ),
      size = rel(label_size * 4),
      color = "#f5f5f5" ##"1a1a1a" 
    ) +
    ggplot2::geom_text(
      aes(x = .data$col, y = .data$row, label = .data$SAMPLE_LOCATION),
      size = rel(label_size * 2.5),
      # size.unit = "pt",
      nudge_x = 0.45,
      nudge_y = -0.4,
      check_overlap = TRUE
    ) +
    labs(
      title = descr,
      subtitle = paste(date, Instrument, "Plate ID:", plate@plate_id),
      caption = caption,
      fill = color,
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = rel(label_size * 1.5), face = "bold"),
      axis.text.y = element_text(size = rel(label_size * 1.5), face = "bold"),
      axis.title.y = element_blank(),
      plot.margin = unit(c(0, 0, 0, 0), "null"),
      panel.spacing = unit(c(0, 0, 0, 0), "null"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
    ) +
    expand_limits(x = c(0.5, 12.5))

  if (!.is_registered(plate) & watermark == "auto") {
    fig <- fig +
      ggplot2::annotate(
        "text",
        x = 12,
        y = 8,
        label = "Not Registered",
        color = "grey",
        size = rel(label_size * 10),
        alpha = 0.8,
        fontface = "bold",
        hjust = 1,
        vjust = -3
      )
    message("Plate not registered. To register, use register_plate()")
  }

  # w = 1.5 * h
  if (!is.null(path)) {
    ggplot2::ggsave(
      path,
      fig,
      width = 12,
      height = 8,
      dpi = 300,
      limitsize = FALSE,
      ...
    )
  }

  fig
}


# #' Create a multi-plate study
# #' @param samples vector of samples names
# #'
# #'@noRd
# multi_plate_study <- function(samples){
#   checkmate::assertVector(samples)

#   NULL
# }

#' Print PlateObj
#' @param x PlateObj
#' @param ... additional arguments passed to print
#' @export
#' @noRd
print.PlateObj <- function(x, ...) {
  cat(
    "96 Well Plate \n \n Active Rows:",
    x@empty_rows,
    "\n",
    "Last Fill:",
    x@last_filled,
    "\n"
  ) |>
    cat("Remaining Empty Spots:", sum(is.na(x@plate)), "\n") |>
    cat("Description:", x@descr, "\n") |>
    cat("Last Modified:", x@last_modified |> as.character(), "\n") |>
    cat("Scheme", x@filling_scheme$scheme, "\n") |>
    cat("Plate ID:", x@plate_id, "\n") |>
    cat("Registered:", .is_registered(x), "\n") |>
    print(...) |>
    invisible()
}


#' Check if a plate is registered
#' @param plate PlateObj
#' @noRd
.is_registered <- function(plate) {
  checkmate::testClass(plate, "RegisteredPlate")
}


#'@noRd
.register_plate_logic <- function(plate, force = FALSE) {
  checkmate::assertClass(plate, "PlateObj")
  plate_id <- plate@plate_id

  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")

  plates_vec <- .compile_cached_plates()

  ids <- str_split(plates_vec, "_")[1]
  subids <- str_split(plates_vec, "_")[2]
  if (plate_id %in% ids) {
    stop("Plate ID already saved in the database")
  }

  # check if file path does not exit, or stop
  save_path <- file.path(db_path, plate_id)

  if (!force) {
    if (.is_registered(plate)) {
      stop("Plate already registered")
    }
    if (file.exists(save_path)) stop("Plate already saved in the database")
  }

  plate <- new(
    "RegisteredPlate",
    plate = plate@plate,
    df = plate@df,
    samples_metadata = plate@samples_metadata,
    plate_id = plate_id,
    empty_rows = plate@empty_rows,
    last_filled = plate@last_filled,
    filling_scheme = plate@filling_scheme,
    last_modified = Sys.time(),
    descr = plate@descr
  )

  saveRDS(plate, save_path)
  plate
}

#' @noRd
.compile_cached_plates <- function() {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")

  plates <- list.files(db_path, full.names = FALSE)
  plates
}

#' Get all plates in the database
#' @noRd
.get_plates_db <- function() {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")
  plates <- list.files(db_path, full.names = TRUE)

  parse_fun <- function(x) {
    x <- readRDS(x)
    id <- x@plate_id
    date <- x@last_modified
    descr <- x@descr

    data.frame(id = id, date = date, descr = descr)
  }
  plates <- lapply(plates, parse_fun)

  df <- do.call(rbind, plates) |>
    dplyr::arrange(desc(date))

  # return df with plate id, last modified, descr, associated lists
  df
}


#' Extract the subid from a plate
#' @param plate PlateObj
#' @noRd
.plate_subid <- function(plate) {
  checkmate::assertClass(plate, "PlateObj")
  plate@plate_id |>
    str_split("_") |>
    _[[1]][2] |>
    as.numeric()
}

#' Extract the plate id from a plate
#' @param plate PlateObj
#' @import checkmate
#' @noRd
.plate_id <- function(plate) {
  checkmate::assertClass(plate, "PlateObj")
  plate@plate_id |>
    str_split("_") |>
    _[[1]][1] |>
    as.numeric()
}

#' Retrive a plate
#' @param id_full character. Plate ID
#' @noRd
.retrieve_plate <- function(id_full) {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")

  plate <- readRDS(file.path(db_path, id_full))
  plate
}

#' Reuse and refill a plate with the same ID
#' @param id numeric. Plate ID
#' @param extra_fill numeric. Additional spots to be ignored
#' @returns PlateObj
#' @noRd
reuse_plate <- function(id, extra_fill = 0) {
  checkmate::assertNumeric(id)
  checkmate::assertNumeric(extra_fill)

  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")
  plates <- list.files(db_path, pattern = paste0(id, "_"))
  plates <- plates[plates %>% str_detect(paste0(id, "_"))]
  if (length(plates) == 0) {
    stop("Plate not found")
  }
  # get plate with the highest subid
  plate_subid <- plates |>
    str_split("_") |>
    sapply(function(x) x |> _[2]) |>
    as.numeric() |>
    max()

  plate <- readRDS(file.path(db_path, paste0(id, "_", plate_subid)))
  plate@plate_id <- paste0(id, "_", plate_subid + 1)

  plate <- new(
    "PlateObj", # reset the plate
    plate = plate@plate,
    df = plate@df,
    plate_id = plate@plate_id,
    empty_rows = plate@empty_rows,
    last_filled = plate@last_filled,
    filling_scheme = plate@filling_scheme,
    last_modified = Sys.time(),
    descr = plate@descr
  )

  # clear all samples and replace with "X"
  plate@plate[!is.na(plate@plate)] <- "X"

  # add extra fill
  if (extra_fill > 0) {
    plate <- add_samples(plate, rep("X", extra_fill), prefix = "")
  }

  # clear all metadata
  plate@df$value <- as.character(NA)
  plate@df$conc <- as.numeric(NA)
  plate@df$TYPE <- as.character(NA)

  plate
}


#' Set plate description
#' @param plate PlateObj
#' @param descr character. Description of the plate
#' @export
#' @returns PlateObj
plate_metadata <- function(plate, descr) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertCharacter(descr)

  plate@descr <- descr

  if (.is_registered(plate)) {
    .register_plate_logic(plate, force = TRUE)
  }
  plate
}


#' Combine plates in MultiPlate object
#' @param plates list of PlateObj objects
#' @import checkmate
#' @returns MultiPlate object
#' @export
combine_plates <- function(plates) {
  checkmate::assertList(plates)
  lapply(plates, function(x) checkmate::assertClass(x, "PlateObj"))

  plates <- new("MultiPlate", plates = plates)

  plates
}


# Bind new samples to the plate df
#' @noRd
.bind_new_samples <- function(df, new_df) {
  dplyr::bind_rows(df, new_df) |>
    dplyr::mutate(
      SAMPLE_LOCATION = paste0(LETTERS[.data$row], ",", .data$col)
    ) |>
    dplyr::slice_tail(by = c(row, col))
}

#' Filling orientation of the plate
#' @param plate PlateObj
#' @param fill character. Filling scheme. Either "h" for horizontal, "v" for vertical.
#' @param tbound character. Top bound of the filling scheme. Default is "A"
#' @param bbound character. Bottom bound of the filling scheme. Default is "H"
#' @param lbound numeric. Left bound of the filling scheme. Default is 1
#' @param rbound numeric. Right bound of the filling scheme. Default is 12
#' @description
#' This function sets the filling scheme of the plate. The filling scheme is used to determine the order in which the samples are filled in the plate.
#' The default filling scheme is horizontal, which means that the samples are filled from left to right and top to bottom.
#' The vertical filling scheme means that the samples are filled from top to bottom and left to right.
#' @returns PlateObj
#' @export
fill_scheme <- function(
  plate,
  fill = "h",
  tbound = "A",
  bbound = "H",
  lbound = 1,
  rbound = 12
) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertChoice(fill, c("h", "v", "hv"))
  checkmate::assertCharacter(tbound)
  checkmate::assertCharacter(bbound)
  checkmate::assertNumeric(lbound)
  checkmate::assertNumeric(rbound)

  tbound <- match(toupper(tbound), LETTERS)
  bbound <- match(toupper(bbound), LETTERS)

  if (tbound > bbound) {
    stop("Top bound should be less than bottom bound")
  }
  if (lbound > rbound) {
    stop("Left bound should be less than right bound")
  }

  tbound <- LETTERS[tbound]
  bbound <- LETTERS[bbound]

  plate@filling_scheme <- list(
    scheme = fill,
    tbound = tbound,
    bbound = bbound,
    lbound = lbound,
    rbound = rbound
  )

  validObject(plate)
  plate
}

.check_feasible_adding <- function(plate, empty_spots, n) {
  total_empty_spots <- nrow(which(is.na(plate@plate), arr.ind = TRUE))
  if (n > nrow(empty_spots)) {
    stop(
      "Not enough empty spots within boundary.  
                                  Region has ",
      nrow(empty_spots),
      " empty spots while ",
      n,
      " are needed.  
                                  Plate has ",
      total_empty_spots,
      " empty spots in total."
    )
  }
}

.spot_mask <- function(plate) {
  # get empty spots
  empty_rows <- plate@empty_rows
  empty_spots <- which(is.na(plate@plate), arr.ind = TRUE) # empty spots
  total_empty_spots <- nrow(empty_spots)

  empty_spots <- empty_spots[
    empty_spots[, 1] >= match(plate@filling_scheme$tbound, LETTERS) &
      empty_spots[, 1] <= match(plate@filling_scheme$bbound, LETTERS) &
      empty_spots[, 2] >= plate@filling_scheme$lbound &
      empty_spots[, 2] <= plate@filling_scheme$rbound,
  ]

  if (is.matrix(empty_spots)) {
    if (plate@filling_scheme$scheme == "h") {
      empty_spots <- empty_spots[order(empty_spots[, 1], empty_spots[, 2]), ]
    } else if (plate@filling_scheme$scheme == "v") {
      empty_spots <- empty_spots[order(empty_spots[, 2], empty_spots[, 1]), ]
    } else if (plate@filling_scheme$scheme == "hv") {
      empty_spots <- empty_spots
      empty_spots <- empty_spots[order(empty_spots[, 1], empty_spots[, 2]), ]
    }
    if (nrow(empty_spots) == 0) {
      stop(
        "No empty spots in the specified boundary. Plate has ",
        total_empty_spots,
        " empty spots."
      )
    }
  } else {
    empty_spots <- matrix(empty_spots, nrow = 1)
  }

  empty_spots
}


# copied from https://stackoverflow.com/questions/43803949/create-and-print-a-product-hierarchy-tree-without-na-from-data-frame-in-r-with
paste5 <- function(..., sep = " ", collapse = NULL, na.rm = TRUE) {
  if (na.rm == F) {
    paste(..., sep = sep, collapse = collapse)
  } else if (na.rm == T) {
    paste.na <- function(x, sep) {
      x <- gsub("^\\s+|\\s+$", "", x)
      ret <- paste(na.omit(x), collapse = sep)
      is.na(ret) <- ret == ""
      return(ret)
    }
    df <- data.frame(..., stringsAsFactors = F)
    ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))

    if (is.null(collapse)) {
      ret
    } else {
      paste.na(ret, sep = collapse)
    }
  }
}

#' Plot the design of the plate
#' @param plate PlateObj object
#' @param plot logical. If TRUE, plot the tree
#' @returns data.tree Node object or DiagrammeR object
#' plot_tree will focus only on bioanalytical vial types, namely blanks, analytes, standards, QCs.
#' The tree order will be plate_id, then group, then vial type, then entity, then number of technical replicates.
#' @export
plate_tree <- function(plate, plot = TRUE) {
  df <- plate@df |>
    mutate(a_group = ifelse(is.na(.data$a_group), "No Group", .data$a_group))
  df <- df[rowSums(is.na(df)) < ncol(df), ]

  df$pathString <- paste5(
    plate@plate_id,
    df$a_group,
    df$TYPE,
    df$e_rep,
    df$std_rep,
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


#' Get plate groups
#' @param plate PlateObj object
#' @returns vector of unique groups in the plate
#' @noRd
plate_groups <- function(plate) {
  checkmate::assertClass(plate, "PlateObj")
  plate@df |>
    dplyr::mutate(
      a_group = ifelse(is.na(.data$a_group), "No Group", .data$a_group)
    ) |>
    dplyr::pull("a_group") |>
    unique()
}

#' Get empty vials given the active layout
#' @param plate PlateObj object
#' @returns number of empty vials in the plate
#' @noRd
length_empty_layout <- function(plate) {
  checkmate::assertClass(plate, "PlateObj")

  empty_spots <- .spot_mask(plate)
  nrow(empty_spots)
}

gen_plate_positions <- function() {
  gtools::mixedsort(levels(interaction(LETTERS[1:8], 1:12, sep = "")))
}


#' Naming style for samples
#' @param plate PlateObj object
#' @param use_subject_id logical. If TRUE, use subject_id instead of subject replicate. Default is FALSE
#' @returns plate with samples renamed
#' @noRd
samples_naming_style <- function(
  plate,
  study_name = TRUE,
  arm = TRUE,
  time = TRUE,
  factor = TRUE,
  sex = FALSE,
  dose = FALSE,
  use_subject_id = TRUE,
  dilution = TRUE
) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertLogical(study_name)
  checkmate::assertLogical(arm)
  checkmate::assertLogical(time)
  checkmate::assertLogical(factor)
  checkmate::assertLogical(sex)
  checkmate::assertLogical(dose)
  checkmate::assertLogical(use_subject_id)

  df <- plate@df
  # reconstruct the df$value in the plate for only TYPE == "Analyte". Do this by matching with samples_metadata

  if (nrow(df[!is.na(df$log_id),]) == 0) {
    message("No samples metadata found. Cannot rename samples")
    return(plate)
  }
  
  samplesmetadata <- dplyr::left_join(
      plate@df |> select("log_id", "study_id", "dil") |> dplyr::filter(!is.na(.data$log_id)),
      plate@samples_metadata,
      by = c("log_id", "study_id")
    )

    
  if (nrow(samplesmetadata) == 0) {
    message("No samples metadata found. Cannot rename samples")
    return(plate)
  }

  if (study_name) {
    samplesmetadata <- samplesmetadata |>
      mutate(new_value = .data$title)
  } else {
    samplesmetadata <- samplesmetadata |>
      mutate(new_value = "")
  }

  if (arm) {
    samplesmetadata <- samplesmetadata |>
      mutate(
        new_value = paste5(
          .data$new_value,
          .data$group_label,
          sep = "_",
          na.rm = TRUE
        )
      )
  }

  if (use_subject_id) {
    samplesmetadata <- samplesmetadata |>
      mutate(
        new_value = paste5(
          .data$new_value,
          .data$subject_id,
          sep = "_",
          na.rm = TRUE
        )
      )
  } else {
    if (anyNA(samplesmetadata$group_replicate)) {
      stop("group_replicate has NA values. Try subject_id = TRUE")
    }

    samplesmetadata <- samplesmetadata |>
      mutate(
        new_value = paste5(
          .data$new_value,
          .data$group_replicate,
          sep = "_",
          na.rm = TRUE
        )
      )
  }
  if (time) {
    samplesmetadata <- samplesmetadata |>
      mutate(
        new_value = paste5(
          .data$new_value,
          paste5(
            .data$nominal_time,
            .data$time_unit,
            sep = "",
            na.rm = TRUE
          ),
          sep = "_",
          na.rm = TRUE
        )
      )
  }
  if (factor) {
    samplesmetadata <- samplesmetadata |>
      mutate(
        new_value = paste5(
          .data$new_value,
          .data$extra_factors,
          sep = "_",
          na.rm = TRUE
        )
      )
  }
  if (sex) {
    samplesmetadata <- samplesmetadata |>
      mutate(
        new_value = paste5(.data$new_value, .data$sex, sep = "_", na.rm = TRUE)
      )
  }
  if (dose) {
    samplesmetadata <- samplesmetadata |>
      mutate(
        new_value = paste5(
          .data$new_value,
          .data$dose_amount,
          sep = "_",
          na.rm = TRUE
        )
      ) |>
      mutate(
        new_value = paste5(
          .data$new_value,
          .data$route,
          sep = "_",
          na.rm = TRUE
        )
      )
  }
  if (dilution) {
    samplesmetadata <- samplesmetadata |>
      mutate(
        new_value = paste5(
          .data$new_value,
          paste0(.data$dil, "X"),
          sep = "_",
          na.rm = TRUE
        )
      )
  }

  df <- df |>
    dplyr::left_join(
      samplesmetadata |> select("log_id", "study_id", "new_value"),
      by = c("log_id", "study_id")
    ) |>
    dplyr::mutate(
      value = ifelse(.data$TYPE == "Analyte", .data$new_value, .data$value)
    ) |>
    dplyr::select(-"new_value")

  plate@df <- df
  validObject(plate)
  plate
}



#' Convert a comma-separated string to a vector
#' @param x character. Comma-separated string
#' @param numeric logical. If TRUE, convert to numeric vector
#' @noRd
str_to_vec <- function(x, numeric = FALSE) {
  res <- unlist(strsplit(x, split = ","))
  res <- trimws(res)
  if (numeric) {
    res <- as.numeric(res)
  }
  res
}
