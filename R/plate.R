
#' Plate object constructor
#'
#' @param m a 96-well matrix
#' @param df data.frame contains plate's metadata
#' @param empty_rows a vector for current active rows
#' @param last_modified last modified date
#' @param plate_id plate id
#' @param descr plate description
#' 
#' @importFrom dplyr mutate slice_tail
#' @noRd
.plate <- function(m, df, plate_id, empty_rows = NULL,
  last_modified = Sys.time(), descr = "") {

  df <- df

  # get last filled well within the active rows
  last_filled_i <- which(is.na(m[empty_rows,]), arr.ind = TRUE, useNames = TRUE)

  # if there is only one active row, R will return a vector not a matrix
  if(length(last_filled_i) == 0) { # no empty spots
    last_filled = NA
  } else if(length(empty_rows) > 1){ # multiple active rows (matrix)
    last_filled <-  last_filled_i[which(last_filled_i[,1] == min(last_filled_i[,1])),  , drop= FALSE]  # smallest row
    last_filled <-  last_filled[which(last_filled[,2] == min(last_filled[,2])),  , drop= FALSE]  # smallest col
    last_filled <- paste(rownames(last_filled), last_filled[,2], sep = ",")
    # last_filled_i <- last_filled_i[order(last_filled_i[,1], last_filled_i[,2]), ]
    # last_filled <- paste(rownames(last_filled_i)[1], last_filled_i[1,2]-1, sep = ",")
  } else if(length(empty_rows) == 1){ # one active row (vector)
    last_filled <- paste(empty_rows, min(last_filled_i)-1, sep = ",")
  }

  # if(grepl(",0$", last_filled)) last_filled <- NA

  s <- list(
    plate = m,
    df = df,
    empty_rows = empty_rows, class = "PlateObj",
    last_filled =  last_filled,
    last_modified = last_modified,
    plate_id = plate_id,
    descr = descr
  )
  class(s) <- "PlateObj"
  s
}


#' Generate 96 Plate
#' Generate a typical 96 well plate. User need to specify the empty rows which a going to be used across the experiment.
#' @param descr plate description.
#' @param empty_rows vector of letters corresponding to empty rows in a 96 well plate.
#' @param extra_fill additional spots to be ignored from the first empty row.
#'
#' @importFrom dplyr slice_tail
#' @importFrom tidyr pivot_longer
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96()
#' plot(plate)
#'
#' plate <- generate_96("calibration", empty_rows = c("C", "D", "E"), extra_fill = 11)
#' plot(plate)
#'
generate_96 <- function(descr = "", empty_rows = NULL,
                        extra_fill = 0) {
  checkmate::assertSubset(empty_rows, choices = LETTERS)
  if (is.null(empty_rows)) {
    empty_rows <- LETTERS[1:8]
  }

  m <- matrix(NA, nrow = 8, ncol = 12)
  rownames(m) <- LETTERS[1:8]

  m[which(!(rownames(m) %in% empty_rows)), ] <- "X"
  m[empty_rows[1], seq_len(extra_fill)] <- "X"

  df <- as.data.frame(m) |>
    dplyr::mutate(row = 1:8) |>
    tidyr::pivot_longer(-row, names_to = "col", values_to = "value") |>
    dplyr::mutate(col = as.integer(str_remove(.data$col, "V"))) |>
    dplyr::mutate(SAMPLE_LOCATION = paste0(LETTERS[.data$row], ",", .data$col)) |>
    dplyr::mutate(samples = as.character(NA)) |>
    dplyr::mutate(conc = as.character(NA)) |>
    dplyr::mutate(time = as.numeric(NA)) |>
    dplyr::mutate(factor = as.character(NA)) |>
    dplyr::mutate(TYPE = as.character(NA))  |>
    dplyr::mutate(std_rep = as.numeric(NA))

  plates_ids <- .compile_cached_plates()

  if(length(plates_ids) == 0) {
    plate_id <- "1_1"
  } else {
    plate_id <- str_split(plates_ids, "_") |> sapply(function(x) x |> _[1]) |> # get plate_id, ignore exp id
      as.numeric() |>
      {\(x) max(x) + 1}() |>
      as.character() |>
      paste0("_1")
  }

  .plate(m, df, plate_id, empty_rows, descr = descr)
}


#' Add unknown samples to a plate
#'
#' @param plate PlateObj
#' @param samples A vector representing samples names
#' @param time A vector representing time points
#' @param conc A vector representing concentration
#' @param factor A vector representing factor
#' @param prefix A prefix to be added before samples names. Default is "S"
#'
#' @details final name will be of form. Prefix-SampleName-Time-Concentration-Factor
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#'  add_samples(paste0("T", 1:12))
add_samples <- function(plate, samples, time  = NA, conc = NA, factor = NA , prefix = "S") {
  checkmate::assertVector(samples)
  checkmate::assertNumeric(time, null.ok = FALSE)
  checkmate::assertNumeric(conc, null.ok = FALSE)
  checkmate::assertVector(factor, null.ok = FALSE)
  checkmate::assertClass(plate, "PlateObj")


  df <- plate$df
  empty_rows <- plate$empty_rows
  plate_id <- plate$plate_id
  descr <- plate$descr
  plate <- plate$plate

  # check if the length of the samples samples are equal
  if(length(samples) != length(time)){
    stopifnot(length(time) == 1)
    time <- rep(time, length(samples))
  }
  if(length(samples) != length(conc)){
    stopifnot(length(conc) == 1)
    conc <- rep(conc, length(samples))
  }
  if(length(samples) != length(factor)){
    stopifnot(length(factor) == 1)
    factor <- rep(factor, length(samples))
  }


  samples <- as.character(samples)
  values <- samples
  if(!is.null(prefix)) values <- paste0(prefix, values)
  if(!is.na(time[1])) values <- paste0(values, "_T", time)
  if(!is.na(conc[1])) values <- paste0(values, "_", conc)
  if(!is.na(factor[1])) values <- paste0(values, "_", factor)

  empty_spots <-
    which(is.na(plate) &
      rownames(is.na(plate)) %in% empty_rows, arr.ind = TRUE)
  empty_spots <-
    empty_spots[order(empty_spots[, 1], empty_spots[, 2]), ]
  masked_spots <-
    which(!(rownames(plate) %in% empty_rows), arr.ind = TRUE)

  new_df <- df[FALSE, ]
  for (i in seq_along(samples)) {
    plate[empty_spots[i, 1], empty_spots[i, 2]] <- samples[i]
    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value = values[i],
        SAMPLE_LOCATION = paste0(LETTERS[empty_spots[i, 1]], ",", empty_spots[i, 2]),
        samples = samples[i],
        conc = ifelse(is.na(conc), "x", as.character(conc[i])),
        TYPE = "Analyte",
        time = ifelse(is.na(time), NA, time[i]),
        factor = ifelse(is.na(factor), NA, factor[i])
      )
    )
  }
  # keep only the samples, other NA
  df <- .bind_new_samples(df, new_df)

  .plate(plate, df, plate_id,  empty_rows, descr = descr)
}


#' Cartesian product of sample factors to a plate
#' @param plate PlateObj
#' @param samples A vector representing samples names
#' @param time A vector representing time points
#' @param conc A vector representing concentration
#' @param factor A vector representing factor
#' @param prefix A prefix to be added before samples names. Default is "S"
#'
#' @returns PlateObj
#' @details This function is a variation of `add_samples()` where size of inputs does not matter.
#' The function will automatically create a combination of all sample names with time, concentration and factor.
#' final name will be of form. Prefix-SampleName-Time-Concentration-Factor
#' @export
add_samples_c <- function(plate, samples, time  = NA, conc = NA, factor = NA , prefix = "S") {
  checkmate::assertVector(samples)
  checkmate::assertNumeric(time, null.ok = FALSE)
  checkmate::assertNumeric(conc, null.ok = FALSE)
  checkmate::assertVector(factor, null.ok = FALSE)

  combined <- expand.grid(samples = samples, time = time, conc = conc, factor = factor) |>
    dplyr::arrange(.data$samples, .data$factor, .data$time, .data$conc)

  plate |> add_samples(samples = combined$samples,
    time = combined$time,
    conc = combined$conc,
    factor = as.character(combined$factor),
    prefix = prefix)
}


#' Add blank to the plate
#' Can be either double blank (DB), CS0IS+ or CS1IS-
#' @param plate PlateObj object
#' @param IS logical. If TRUE, add IS to the well.
#' @param analyte logical. If TRUE, add analyte to the well.
#'
#' @import stringr
#' @returns PlateObj
#' @export
add_blank <- function(plate, IS = TRUE, analyte = FALSE) {
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertLogical(IS)
  checkmate::assertLogical(analyte)

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

  df <- plate$df
  empty_rows <- plate$empty_rows
  descr <- plate$descr
  plate_id <- plate$plate_id
  plate <- plate$plate

  # empty_spots <- which(is.na(plate) & rownames(is.na(plate)) %in% empty_rows, arr.ind = TRUE)
  empty_spots <- which(is.na(plate), arr.ind = TRUE)
  # Ordering one spot gives error. Prevent casting to vector
  if (nrow(empty_spots) > 1) {
    empty_spots <- empty_spots[order(empty_spots[, 1], empty_spots[, 2]), ]
  }

  plate[empty_spots[1, 1], empty_spots[1, 2]] <- blank_vec


  new_df <- data.frame(
    row = empty_spots[1, 1],
    col = empty_spots[1, 2],
    value = blank_vec,
    SAMPLE_LOCATION = paste0(LETTERS[empty_spots[1, 1]], ",", empty_spots[1, 2]),
    conc = as.character(case_when(
      stringr::str_detect(blank_vec, "DB") ~ 0,
      stringr::str_detect(blank_vec, "CS0IS+") ~ 0,
      stringr::str_detect(blank_vec, "CS1IS-") ~ 1
    )),
    TYPE = case_when(
      stringr::str_detect(blank_vec, "DB") ~ "DoubleBlank",
      stringr::str_detect(blank_vec, "CS0IS+") ~ "Blank",
      stringr::str_detect(blank_vec, "CS1IS-") ~ "ISBlank",
    )
  )


  df <- .bind_new_samples(df, new_df)

  .plate(plate, df, plate_id, empty_rows, descr = descr)
}

#' Add double blank (DB) to a plate
#' @param plate PlateObj object
#'
#' @import checkmate
#' @export
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#' add_DB()
add_DB <- function(plate){
  checkmate::assertClass(plate, "PlateObj")

  add_blank(plate, IS = FALSE, analyte = FALSE)

}

#' Add calibration curve to the plate
#'
#' @param plate PlateObj
#' @param plate_std character
#'
#' @export
#' 
#' @returns PlateObj
#' @examples
#' plate <- generate_96() |>
#'  add_cs_curve(c(1, 3, 5, 10, 50, 100, 200))
#' plot(plate)
add_cs_curve <- function(plate, plate_std) {
  checkmate::assertNumeric(plate_std, lower = 0.01, finite = TRUE)
  checkmate::assertClass(plate, "PlateObj")

  std_rep <- .last_std(plate) + 1

  df <- plate$df
  empty_rows <- plate$empty_rows
  plate_id <- plate$plate_id
  descr <- plate$descr
  plate <- plate$plate

  plate_std <- paste0("CS", seq_along(plate_std), "_", plate_std)

  empty_spots <- which(is.na(plate), arr.ind = TRUE)
  empty_spots <-
    empty_spots[order(empty_spots[, 1], empty_spots[, 2]), ]

  new_df <- df[FALSE, ]

  for (i in seq_along(plate_std)) {
    plate[empty_spots[i, 1], empty_spots[i, 2]] <- plate_std[i]
    new_df <- dplyr::bind_rows(
      new_df,
      data.frame(
        row = empty_spots[i, 1],
        col = empty_spots[i, 2],
        value = plate_std[i],
        SAMPLE_LOCATION = paste0(LETTERS[empty_spots[i, 1]], ",", empty_spots[i, 2]),
        conc =  as.character(str_extract(plate_std[i], "(\\d*\\.?\\d+)$")),
        TYPE = "Standard",
        std_rep = std_rep
      )
    )
  }

  # add sample to the df
  df <- .bind_new_samples(df, new_df)

  .plate(plate, df, plate_id, empty_rows, descr = descr)
}

#' Get last standard repetition
#'@noRd
.last_std <- function(plate){
  suppressWarnings({
  n <- plate$df |> dplyr::filter(.data$TYPE == "Standard") |>
    pull(.data$std_rep) |>
    max(na.rm = TRUE)
  })

  ifelse(is.finite(n), n, 0)
}

#' Get last quality control repetition
#' @noRd
.last_qc <- function(plate){

  suppressWarnings({
  n <- plate$df |> dplyr::filter(.data$TYPE == "QC") |>
    pull(.data$std_rep) |>
    max()
  })

  ifelse(is.finite(n), n, 0)
}

#' Add suitability sample to the plate
#' @param plate PlateObj object.
#' @param conc numeric. Concentration of the suitability well.
#' @param label character. Label for the suitability well. Default is "suitability".
#' @importFrom dplyr bind_rows mutate slice_tail
#' @returns PlateObj
#' @export
add_suitability <- function(plate, conc, label = "suitability") {
  checkmate::assertCharacter(label)
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertNumeric(conc, finite = TRUE, lower = 0)

  df <- plate$df
  empty_rows <- plate$empty_rows
  plate_id <- plate$plate_id
  descr <- plate$descr
  plate <- plate$plate

  empty_spots <- which(is.na(plate), arr.ind = TRUE)
  empty_spots <-
    empty_spots[order(empty_spots[, 1], empty_spots[, 2]), ]

  new_df <- df[FALSE, ]

  plate[empty_spots[1, 1], empty_spots[1, 2]] <- label
  new_df <- dplyr::bind_rows(
    new_df,
    data.frame(
      row = empty_spots[1, 1],
      col = empty_spots[1, 2],
      value = paste0(label, "_", conc),
      SAMPLE_LOCATION = paste0(LETTERS[empty_spots[1, 1]], ",", empty_spots[1, 2]),
      conc = as.character(conc),
      TYPE = "Suitability"
    )
  )

  df <- .bind_new_samples(df, new_df)

  .plate(plate, df, plate_id, empty_rows, descr = descr)
}

#' Check the quality control samples valid
#' The function will be strict for LQC, but will give a warning only for MQC and HQC
#' @param std_vec vector of calibration standards
#' @param loq_conc limit of quantification
#' @param lqc_conc low quality control concentration
#' @param mqc_conc  medium quality control concentration
#' @param hqc_conc high quality control concentration
#' @returns PlateObj
#' @noRd
.check_qcs <- function(std_vec, loq_conc, lqc_conc, mqc_conc, hqc_conc) {
  checkmate::assertNumeric(loq_conc, lower = 0)
  checkmate::assertNumeric(lqc_conc, lower = loq_conc)
  checkmate::assertNumeric(mqc_conc, lower = lqc_conc)
  checkmate::assertNumeric(hqc_conc, lower = mqc_conc)

  # find the 30%, 50% and 75% cut on the calibration range
  min_val <- as.numeric(loq_conc)
  max_val <- max(as.numeric(std_vec))
  quantrange <- quantile(c(min_val, max_val), c(0.30, 0.50, 0.70)) 

  if(!(lqc_conc <= lqc_conc*3)) stop(paste("LQC should be less or equal 3xLOQ (<", loq_conc*3), ")")
  if(!(mqc_conc >= quantrange[1] & mqc_conc <= quantrange[2])) warning(paste("MQC should be between 30% (",
    quantrange[1], ")and 50%", quantrange[2] ,"of the calibration range"))
  if(!(hqc_conc >= quantrange[3])) warning(paste("HQC should be equal or greater than 75% (>=", quantrange[3], ")of the calibration range"))

}


#' Add quality control samples to the plate
#' @param plate PlateObj object
#' @param lqc_conc low quality control concentration
#' @param mqc_conc medium quality control concentration
#' @param hqc_conc high quality control concentration
#' @param n_qc number of QC sets. Default is 3
#' @param qc_serial logical. If TRUE, QCs are placed serially
#'
#' @returns PlateObj
#' @export
add_qcs <- function(plate, lqc_conc, mqc_conc, hqc_conc, n_qc=3, qc_serial=TRUE){
  checkmate::assertClass(plate, "PlateObj")

  # assert there was a standard call, and get the last call
  grp_std <- .last_std(plate)

  if(grp_std == 0){
    stop("The plate does not have any standards. Use add_cs_curve")
  }

  # assert there is only no qc associated with last standard
  grp_qc <- .last_qc(plate)
  if(grp_qc == grp_std){
    stop("There is already a QC associated with the last standard")
  }

  # get the lloq from the last call
  plate_std <- plate$df |> dplyr::filter(.data$TYPE == "Standard", .data$std_rep == grp_std) |>
    dplyr::pull(.data$conc) 
  loq_conc <- plate_std |>
    as.numeric() |> min(na.rm = TRUE)

  stopifnot(is.numeric(loq_conc) & loq_conc > 0)

  .check_qcs(plate_std, loq_conc, lqc_conc, mqc_conc, hqc_conc)
  checkmate::assertLogical(qc_serial)
  checkmate::assertNumeric(n_qc, lower = 0)

  df <- plate$df
  empty_rows <- plate$empty_rows
  plate_id <- plate$plate_id
  descr <- plate$descr
  plate <- plate$plate

  empty_spots <-
    which(is.na(plate) &
      rownames(is.na(plate)) %in% empty_rows, arr.ind = TRUE)
  empty_spots <-
    empty_spots[order(empty_spots[, 1], empty_spots[, 2]), ]
  if (nrow(empty_spots) < 4 * n_qc) {
    stop("Not enough empty spots for QC")
  }

  new_df <- df[FALSE,]

  if (qc_serial) {
    vec_qc_names <-
      rep(c(
        glue::glue("QC1_LLOQ_{loq_conc}"),
        glue::glue("QC2_LQC_{lqc_conc}"),
        glue::glue("QC3_MQC_{mqc_conc}"),
        glue::glue("QC4_HQC_{hqc_conc}")
      ), each = n_qc)
  } else {
    vec_qc_names <-
      rep(c(
        glue::glue("QC1_LLOQ_{loq_conc}"),
        glue::glue("QC2_LQC_{lqc_conc}"),
        glue::glue("QC3_MQC_{mqc_conc}"),
        glue::glue("QC4_HQC_{hqc_conc}")
      ), n_qc)
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
        value =  vec_qc_names[i],
        SAMPLE_LOCATION = paste0(LETTERS[empty_spots[i, 1]], ",", empty_spots[i, 2]),
        conc =  as.character(str_extract(vec_qc_names[i], "(\\d*\\.?\\d+)$")),
        TYPE = "QC",
        std_rep = grp_std # same as standard used (assuming one QC set per CS set)
      )
    )
  }

  df <- .bind_new_samples(df, new_df)

  .plate(plate, df, plate_id, empty_rows, descr = descr)

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
#'
#' @import stringr
#' 
#' @returns PlateObj
#' @export
make_calibration_study <-
 function(plate,
          plate_std,
          lqc_conc = NULL,
          mqc_conc = NULL,
          hqc_conc = NULL,
          n_qc = NULL,
          qc_serial = FALSE,
          n_CS0IS0 = 1,
          n_CS0IS1 = 2,
          n_CS1IS0 = 1
          ) {
   checkmate::assertClass(plate, "PlateObj")
   checkmate::assertVector(plate_std)

   checkmate::assertNumeric(n_CS0IS0)
   checkmate::assertNumeric(n_CS0IS1)
   checkmate::assertNumeric(n_CS1IS0)

   for (i in seq(n_CS0IS0)) {
     plate <- add_blank(plate, IS = FALSE, analyte = FALSE)
   }


   for (i in seq(n_CS1IS0)) {
     plate <- add_blank(plate, IS = FALSE, analyte = TRUE)
   }

   for (i in seq(n_CS0IS1)) {
     plate <- add_blank(plate, IS = TRUE, analyte = FALSE)
   }

   plate <- add_cs_curve(plate, plate_std)

   if (!is.null(lqc_conc) & !is.null(mqc_conc) & !is.null(hqc_conc) & !is.null(n_qc)) {
       if(n_qc != 0){
         plate <- plate |> add_qcs(lqc_conc, mqc_conc, hqc_conc, n_qc, qc_serial)
       }
   }


 #   conc_mat <- str_extract(plate, "\\d+$")
 #   labels_mat <- str_extract(plate, "^\\D+")
 #
 #   # totally new df
 #   df <- as.data.frame(plate) |>
 #     dplyr::mutate(row = 1:8) |>
 #     tidyr::pivot_longer(-row, names_to = "col", values_to = "value") |>
 #     dplyr::mutate(col = as.integer(str_remove(col, "V"))) |>
 #     dplyr::mutate(SAMPLE_LOCATION = paste0(LETTERS[row], ",", col)) |>
 #     dplyr::mutate(conc = str_extract(value,  "(\\d*\\.?\\d+)$")) |>
 #     dplyr::mutate(
 #       TYPE = case_when(
 #         stringr::str_detect(value, "DB") ~ "DoubleBlank",
 #         stringr::str_detect(value, "CS0IS+") ~ "Blank",
 #         stringr::str_detect(value, "CS1IS-") ~ "ISBlank",
 #         stringr::str_detect(value, "CS") ~ "Standard",
 #         stringr::str_detect(value, "QC") ~ "QC"
 #       )
 #     )
 #
 # .plate(plate, df, plate_id, empty_rows)
   plate
 }


#' Plotting 96 well plate
#'
#' @param x PlateObj
#' @param color character. Coloring variable. Either "conc", "time", "factor", "samples", "TYPE"
#' @param Instrument A string placed at subtitle
#' @param caption A string place at plate caption
#' @param label_size numeric. Size of the label. Default is 15
#' @param path Default is NULL, if not null, must be a path to save plate image
#' @param ... additional arguments passed to ggplot2::ggsave
#'
#' @importFrom ggplot2 coord_equal scale_fill_discrete scale_x_continuous scale_y_continuous geom_text labs theme_minimal theme expand_limits
#' @importFrom ggforce geom_circle
#' @importFrom glue glue
#' @export
#' @returns ggplot object
#'
#' @examples
#' plate <- generate_96("new_plate", c("C", "D", "E"), 11) |>
#'   add_blank(IS = FALSE, analyte = FALSE) |>
#'   add_blank(IS = TRUE, analyte = FALSE) |>
#'   add_samples(c(
#'     "RD_per1", "RD_in1", "RD_T30", "RD_T60", "RD_T90", "RD_per2", "RD_in2",
#'     "EE_in0", "EE_T30", "EE_in30", "EE_T60", "EE_in60", "EE_T90", "EE_in90"
#'   ))
#' plot(plate)
plot.PlateObj <- function(x,
                          color = "conc",
                          Instrument = "",
                          caption = "",
                          label_size = 15,
                          path = NULL, ...
                          ) {

  plate <- x
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertChoice(color, c("conc", "time", "factor", "samples", "TYPE"))
  checkmate::assertCharacter(Instrument)
  checkmate::assertCharacter(caption)
  checkmate::assertCharacter(path, null.ok = TRUE)


  descr <- plate$descr
  plate_df <- plate$df |> # zero if blanks, NA if empty cell. Conc otherwise
    mutate(conc = ifelse(is.na(.data$conc), ifelse(.data$value == "X", NA, 0), .data$conc)) |>
    mutate(time = as.character(.data$time))  |>
    mutate(factor = as.character(.data$factor))

  # remove bottle if there
  plate_df$SAMPLE_LOCATION <-
    gsub("^.*:", "", plate_df$SAMPLE_LOCATION)

  date <- plate$last_modified |> as.Date()

  fig <- ggplot2::ggplot(data = plate_df) +
    ggforce::geom_circle(aes(
      x0 = .data[["col"]],
      y0 = .data[["row"]],
      r = 0.45,
      fill = .data[[color]]
    )) +
    ggplot2::coord_equal() +
    # scale_fill_discrete(na.value = "transparent") +
    scale_fill_discrete(na.translate = FALSE) +
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
        label = str_replace_all(.data$value, "_", "\n")
      ),
      size = label_size,
      size.unit = "pt",
      color = "white"
    ) +
    ggplot2::geom_text(
      aes(x = .data$col, y = .data$row, label = .data$SAMPLE_LOCATION),
      size = 10,
      size.unit = "pt",
      nudge_x = 0.45,
      nudge_y = -0.4,
      check_overlap = TRUE
    ) +
    labs(
      title = descr,
      subtitle = paste(date, Instrument, "Plate ID:", plate$plate_id),
      caption = caption,
      x = "",
      y = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.title.y = element_blank() ,
      plot.margin = unit(c(0, 0, 0, 0), "null"),
      panel.spacing = unit(c(0, 0, 0, 0), "null"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
    ) + expand_limits(x = c(0.5,12.5))

  if(!.is_registered(plate)) {
    fig <- fig +
      ggplot2::annotate("text", x = 12, y = 8, label = "Not Registered",
       color = "grey", size = 18, alpha = 0.8, fontface = "bold",
       hjust = 1, vjust = -3)
    message("Plate not registered. To register, use register_plate()")
  }

  if (!is.null(path)) ggplot2::ggsave(path, fig, width = 12, height =8, dpi = 300, limitsize = FALSE, ...)
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


#' Create a metabolic study layout
#' @param cmpds vector of compounds, including any standards
#' @param time_points vector of time points
#' @param n_NAD number of NAD positive samples. Default is 3
#' @param n_noNAD number of NAD negative samples. Default is 2
#'
#' @details Note that this function does not require plate object. It will create a plate object automatically and return MultiPlate object
#' @returns MultiPlate object
#' @export
make_metabolic_study <- function(cmpds,
time_points = c(0, 5,10, 15, 30, 45, 60, 75, 90, 120), n_NAD =3 , n_noNAD = 2){
  checkmate::assertVector(cmpds)
  checkmate::assertVector(time_points)
  checkmate::assertNumeric(n_NAD)
  checkmate::assertNumeric(n_noNAD)

  time_points <- rep(time_points, n_NAD)
  # Create a data frame with all combinations of cmpd and time_points

  df <- expand.grid(cmpd = cmpds, time_points = time_points, factor = "NAD")  |>
    dplyr::arrange(.data$time_points, .data$cmpd)

  time_points <- rep(time_points, n_noNAD)
  df2 <- expand.grid(cmpd = cmpds, time_points = time_points, factor = "noNAD")  |>
    arrange(.data$time_points, .data$cmpd)

  df <- rbind(df, df2)

  n_plates <- ceiling(nrow(df) / 96)

  plates_ids <- .compile_cached_plates()

  plates_ids <- str_split(plates_ids, "_") |>
    sapply(function(x) x |> _[1]) |> # get plate_id, ignore exp id
    as.numeric() |>
    {\(x) max(x)}()

  plates_ids <- plates_ids + c(1:n_plates)

  plate <- lapply(1:n_plates, function(x){
    curr_plate <- generate_96()
    if(x == 1){ # first plate
      vec <- 1:96
      curr_plate <- add_samples(curr_plate, time = df$time_points[vec],
        samples = df$cmpd[vec],
        prefix = "", factor = as.character(df$factor[vec]))
    } else if (x == n_plates){ # last plate
        y <- x - 1
        vec <- (y*96+1):nrow(df)
        current_df <- df[vec, ]
        curr_plate <- add_samples(curr_plate, time = current_df$time_points,
          samples = current_df$cmpd,
          prefix = "", factor = as.character(current_df$factor))
        curr_plate$plate_id <- paste0(plates_ids[x], "_1")
    } else {
        y <- x - 1
        vec <- (y*96+1):(y*96+96)
        current_df <- df[vec, ]
        curr_plate <- add_samples(curr_plate, time = current_df$time_points,
          samples = current_df$cmpd,
          prefix = "", factor = as.character(current_df$factor))
        curr_plate$plate_id <- paste0(plates_ids[x], "_1")
    }
    curr_plate
  })
  class(plate) <- c("MultiPlate", "PlateObj")
  plate

}


#' Print PlateObj
#' @param x PlateObj
#' @param ... additional arguments passed to print
#' @export
#' @noRd
print.PlateObj <- function(x, ...) {
  cat("96 Well Plate \n \n Active Rows:", x$empty_rows, "\n", "Last Fill:", x$last_filled, "\n") |>
  cat("Remaining Empty Spots:", sum(is.na(x$plate)), "\n") |>
  cat("Description:", x$descr, "\n") |>
  cat("Last Modified:", x$last_modified |> as.character(), "\n") |>
  cat("Plate ID:", x$plate_id, "\n") |> 
  cat("Registered:", .is_registered(x), "\n") |>  
  print(...) |> invisible()
}


#' Check if a plate is registered
#' @param plate PlateObj
#' @noRd
.is_registered <- function(plate){
  checkmate::testClass(plate, c("RegisteredPlate", "PlateObj"))
}



#' Register a plate
#' This will save the plate to the database
#' @param plate PlateObj object or MultiPlate object
#' @returns PlateObj object or list of PlateObj objects
#' @export
register_plate <- function(plate){
  UseMethod("register_plate")
}

#' @export
register_plate.default <- function(plate){
  stop("Object not supported")
}


#'@export
register_plate.PlateObj <- function(plate){
  .register_plate_logic(plate)
}

#'@export
register_plate.MultiPlate <- function(plate){
  lapply(plate, .register_plate_logic)
}


#'@noRd
.register_plate_logic <- function(plate, force = FALSE){
  checkmate::assertClass(plate, "PlateObj")
  plate_id <- plate$plate_id


  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")

  plates_vec <- .compile_cached_plates()

  ids <- str_split(plates_vec, "_")[1]
  subids <- str_split(plates_vec, "_")[2]
  if(plate_id %in% ids) stop("Plate ID already saved in the database")

  # check if file path does not exit, or stop
  save_path <- file.path(db_path, plate_id)

  if(!force){
    if(.is_registered(plate)) stop("Plate already registered")
    if(file.exists(save_path)) stop("Plate already saved in the database")
  }

  class(plate) <- c("RegisteredPlate", "PlateObj")
  saveRDS(plate, save_path)
  plate
}

#' @noRd
.compile_cached_plates <- function(){
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")

  plates <- list.files(db_path, full.names = FALSE)
  plates
}

#' Get all plates in the database
#' @noRd
.get_plates_db <- function(){
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")
  plates <- list.files(db_path, full.names = TRUE)

  parse_fun <- function(x){
    x <- readRDS(x)
    id <- x$plate_id
    date <- x$last_modified
    descr <- x$descr


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
.plate_subid <- function(plate){
  checkmate::assertClass(plate, "PlateObj")
  plate$plate_id |>
    str_split("_") |> _[[1]][2] |> as.numeric()

}

#' Extract the plate id from a plate
#' @param plate PlateObj
#' @import checkmate
#' @noRd
.plate_id <- function(plate){
  checkmate::assertClass(plate, "PlateObj")
  plate$plate_id |>
    str_split("_") |> _[[1]][1] |> as.numeric()
}

#' Retrive a plate
#' @param id_full character. Plate ID 
#' @noRd 
.retrieve_plate <- function(id_full){
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
reuse_plate <- function(id, extra_fill = 0){
  checkmate::assertNumeric(id)
  checkmate::assertNumeric(extra_fill)

  db_path <- PKbioanalysis_env$data_dir |>
    file.path("plates_cache")
  plates <- list.files(db_path, pattern = paste0(id, "_"))
  plates <- plates[plates %>% str_detect(paste0(id, "_"))]
  if(length(plates) == 0) stop("Plate not found")
  # get plate with the highest subid
  plate_subid <- plates |>
    str_split("_") |>
    sapply(function(x) x |> _[2]) |>
    as.numeric() |>
    max()

  plate <- readRDS(file.path(db_path, paste0(id, "_", plate_subid)))
  plate$plate_id <- paste0(id, "_", plate_subid + 1)
  class(plate) <- "PlateObj" # reset registration

  # clear all samples and replace with "X"
  plate$plate[!is.na(plate$plate)] <- "X"

  # add extra fill
  if(extra_fill > 0){
    plate <- add_samples(plate, rep("X", extra_fill), prefix = "")
  }

  # clear all metadata
  plate$df$value <- as.character(NA)
  plate$df$conc <- as.character(NA)
  plate$df$TYPE <- as.character(NA)

  plate

}


#' Set plate description
#' @param plate PlateObj
#' @param descr character. Description of the plate
#' @export
#' @returns PlateObj
plate_metadata <- function(plate, descr){
  checkmate::assertClass(plate, "PlateObj")
  checkmate::assertCharacter(descr)

  plate$descr <- descr

  if(.is_registered(plate)){
    .register_plate_logic(plate, force = TRUE)
  }
  plate
}


#' Combine plates in MultiPlate object
#' @param plates list of PlateObj objects
#' @import checkmate
#' @returns MultiPlate object
#' @export
combine_plates <- function(plates){
  checkmate::assertList(plates)
  lapply(plates, function(x) checkmate::assertClass(x, "PlateObj"))


  class(plates) <- c("MultiPlate", "PlateObj")
  plates
}


# Bind new samples to the plate df
#' @noRd
.bind_new_samples <- function(df, new_df) {
  dplyr::bind_rows(df, new_df) |>
    dplyr::mutate(SAMPLE_LOCATION = paste0(LETTERS[.data$row], ",", .data$col)) |>
    dplyr::slice_tail(by = c(row, col))
}
