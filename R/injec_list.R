#' Create unique sequence names
#' @param df dataframe
#' Use numbers for technical replicates and letters for injection replicates
#' Skip technical replicates for analytes as expected to be unique
#' @noRd
make_sequence_names <- function(df){
  df <- df |>
    dplyr::group_by(.data$SAMPLE_LOCATION) |>
    dplyr::mutate(FILE_NAME = ifelse(.data$TYPE != "Analyte", paste0(.data$FILE_NAME, "_", dplyr::row_number()), .data$FILE_NAME)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$FILE_NAME) |>
    dplyr::mutate(FILE_NAME = paste0(.data$FILE_NAME, LETTERS[dplyr::row_number()])) |>
    dplyr::ungroup()

  stopifnot(length(unique(df$FILE_NAME)) == nrow(df))
  df
}

.get_item <- function(platedf, type, nrep, group) {
  res <- dplyr::filter(
    platedf,
    .data$TYPE == type,
    (is.na(group) & is.na(.data$a_group)) |
      (!is.na(group) & .data$a_group == group)
  )
  rep_vec <- rep(seq_len(nrow(res)), each = nrep)
  res <- res[rep_vec, ]
  res$injec_rep <- stats::ave(rep_vec, rep_vec, FUN = base::seq_along)
  res <- dplyr::arrange(res, .data$injec_rep, .data$e_rep, .data$std_rep)

  if (nrow(res) == 0) {
    NULL
  } else {
    res
  }
}

.get_xplore <- function(platedf, n) {
  sample_from <- c("Blank", "Standard", "QC", "DQC")[seq_len(n)]
  platedf |>
    filter(.data$TYPE %in% !!sample_from) |>
    dplyr::slice_sample(n = 1, by = "TYPE")
}

.get_suitability <- function(platedf, nrep) {
  res <- dplyr::filter(platedf, .data$TYPE == "Suitability")
  res <- res[rep(seq_len(nrow(res)), each = nrep), ]
  if (nrow(res) == 0) {
    return(NULL)
  } else {
    return(res)
  }
}

.add_blank_at_end <- function(tmpseq) {
  if (is.null(tmpseq$Blank)) {
    stop("A group does not have any blanks. Cannot add blank at the end.")
  }
  blanks_df <- tmpseq$Blank |>
    dplyr::filter(.data$injec_rep == min(.data$injec_rep) & .data$e_rep == max(.data$e_rep))

  tmpseq$last_blank <- blanks_df
  tmpseq
}

.add_blank_after_high_conc <- function(tmpseq) {
  # getting the last added one regardless of injec_rep

  if (is.null(tmpseq$Blank)) {
    stop(
      "A group does not have any blanks. Cannot add blank after high concentration."
    )
  }

  blanks_df <- tmpseq$Blank |>
    filter(.data$injec_rep == min(.data$injec_rep) & .data$e_rep == max(.data$e_rep))
  if (!is.null(tmpseq$CS)) {
    dummy <- paste0(
      tmpseq$CS$e_rep,
      "_",
      tmpseq$CS$std_rep,
      "_",
      tmpseq$CS$injec_rep
    )
    cs_split <- tmpseq$CS |> split(dummy)

    cs_split <- lapply(cs_split, function(x) {
      rbind(x, blanks_df)
    })
    tmpseq$CS <- do.call(rbind, cs_split)
  }

  if (!is.null(tmpseq$Analyte_QC)) {
    if ("QC" %in% tmpseq$Analyte_QC$TYPE) {
      max_conc <- tmpseq$Analyte_QC |>
        dplyr::filter(.data$TYPE == "QC") |>
        dplyr::pull("conc") |>
        max()
      idx <- which(tmpseq$Analyte_QC$conc == max_conc)

      increment <- 0
      for (i in idx) {
        tmpseq$Analyte_QC <- dplyr::add_row(
          tmpseq$Analyte_QC,
          blanks_df,
          .after = i + increment
        )
        increment <- increment + 1
      }
    }
  }

  tmpseq
}

.clean_list <- function(tmpseq) {
  Filter(function(x) !all(sapply(x, is.null)), tmpseq)
}

.merge_qc_analyte <- function(tmpseq) {
  if (!is.null(tmpseq$QC)) {
    qc_list <- split(
      tmpseq$QC,
      paste(tmpseq$QC$std_rep, tmpseq$QC$injec_rep, tmpseq$QC$e_rep)
    )
    qc_levels <- unique(tmpseq$QC$conc)
    n_qc_levels <- length(qc_levels)
    total_qcs <- nrow(tmpseq$QC) / n_qc_levels
    stopifnot(length(qc_list) == total_qcs) # Cannot have different QCs in same group
  }
  if (!is.null(tmpseq$Analyte)) {
    total_samples <- nrow(tmpseq$Analyte)
  }
  if (!is.null(tmpseq$QC) & !is.null(tmpseq$Analyte)) {
    break_every <- floor(total_samples / n_qc_levels)

    # Break analyte_df into chunks of size break_every
    analyte_df <- tmpseq$Analyte
    merged_df <- analyte_df[0, ] # empty dataframe with same columns
    # Split analyte_df into n_qc_levels approximately equal chunks
    split_indices <- cut(
      seq_len(nrow(analyte_df)),
      breaks = n_qc_levels,
      labels = FALSE
    )
    analyte_splits <- split(analyte_df, split_indices)

    # Interleave qc_list and analyte_splits
    max_len <- max(length(qc_list), length(analyte_splits))
    merged_list <- vector("list", length(analyte_splits) + length(qc_list))

    idx <- 1
    for (i in seq_len(max_len)) {
      if (i <= length(qc_list)) {
        merged_list[[idx]] <- qc_list[[i]]
        idx <- idx + 1
      }
      if (i <= length(analyte_splits)) {
        merged_list[[idx]] <- analyte_splits[[i]]
        idx <- idx + 1
      }
    }
    merged_df <- do.call(rbind, merged_list)

    stopifnot(nrow(merged_df) == nrow(tmpseq$Analyte) + nrow(tmpseq$QC))
  } else {
    merged_df <- rbind(tmpseq$Analyte, tmpseq$QC)
  }

  tmpseq$Analyte <- NULL
  tmpseq$QC <- NULL
  tmpseq$Analyte_QC <- merged_df
  tmpseq
}


#' injection List Object
#' @param df dataframe
#' @param plates vector of plate IDs
#' @noRd
.injecList <- function(df, plates) {
  s <- list(
    injec_list = df,
    plates = plates
  )

  class(s) <- "InjecListObj"
  s
}


#' Interject dataframe every Nth position
#'
#' @param df original dataframe
#' @param add_df dataframe to add
#' @param every_n number of rows to interject add_df
#' @noRd
.add_every_n <- function(df, add_df, every_n) {
  dflen <- 1:nrow(df)
  df |>
    mutate(grp = (dflen %/% every_n)) |>
    group_by(.data$grp) |>
    group_modify(~ bind_rows(.x, add_df)) |>
    ungroup() |>
    select(-.data$grp)
}


#' Create Sample List with rigorous design
#'
#' @param sample_lists a list of sample lists
#' @param n_equi number of equilibriation injections
#' @param equi_prefix prefix for equilibriation injections
#' @param equi_suffix suffix for equilibriation injections
#' @param equi_pos position of equilibriation injections. For format check details
#' @param equi_injec_vol volume of equilibriation injection
#'
#' @details The equi_pos format will be Row:Column format. E.g: "A,1"
#'
#' @importFrom checkmate assertList assertNumber assertString assertNumeric
#' @export
#' @returns InjecListObj object
#'
combine_injec_lists <-
  function(
    sample_lists,
    n_equi = 10,
    equi_pos,
    equi_prefix = Sys.Date(),
    equi_suffix = "equi",
    equi_injec_vol = 0.5
  ) {
    checkmate::assertList(sample_lists, min.len = 2)
    checkmate::assertNumber(n_equi, lower = 1)

    equi_prefix <- as.character(equi_prefix)
    checkmate::assertString(equi_prefix)
    checkmate::assertString(equi_suffix)
    checkmate::assertString(equi_pos)
    checkmate::assertNumeric(equi_injec_vol, lower = 0)

    idx <- strsplit(equi_pos, ",")
    row_i <- idx[[1]][1]
    col_i <- as.numeric(idx[[1]][2])
    checkmate::assertNumber(col_i)
    checkmate::assert(row_i %in% LETTERS)

    # create equilibriation row
    equi_df <-
      dplyr::filter(
        sample_lists[[1]]$injec_list,
        col == col_i & row == which(LETTERS == row_i)
      ) |>
      dplyr::slice_head(n = 1) |>
      dplyr::mutate(INJ_VOL = equi_injec_vol) |>
      dplyr::mutate(
        FILE_NAME = paste0(equi_prefix, "_", .data$value, "_", equi_suffix)
      ) |>
      dplyr::mutate(FILE_TEXT = paste0("equilibriate"))

    stopifnot(
      "Equilibriation position not found in the plate. Please check equi_pos." = nrow(
        equi_df
      ) >=
        1
    )

    equi_df <- equi_df |> dplyr::slice(rep(seq(n()), n_equi))

    df <- sample_lists[[1]]$injec_list[FALSE, ] # placeholder for new list
    current_plates_ids <- c()
    for (i in seq_along(sample_lists)) {
      checkmate::assertClass(sample_lists[[i]], "InjecListObj")
      df <- rbind(df, sample_lists[[i]]$injec_list)
      if (i != length(sample_lists)) {
        df <- rbind(df, equi_df)
      }
      current_plates_ids <- c(current_plates_ids, sample_lists[[i]]$plates)
    }

    df <- df |> mutate(Index = row_number())

    x <- .injecList(df, current_plates_ids)
    print(x)
  }

#' Write injection sequence to database
#'
#' @param injec_seq InjecListObj object
#' @return dataframe
#' @export
write_injec_seq <- function(injec_seq) {
  checkmate::assertClass(injec_seq, "InjecListObj")

  # Modify sample list
  sample_list <- injec_seq$injec_list |> 
    dplyr::rename_all(tolower) |>
    select(-matches("index")) |> 
    mutate(injec_id = uuid::UUIDgenerate(n =  nrow(injec_seq$injec_list)))

  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")

  .check_sample_db()
  db <- .connect_to_db()

  # find last unqiue ID and add 1
  max_id_query <- "SELECT MAX(list_id) AS max_id FROM platesdb"
  max_id_result <- DBI::dbGetQuery(db, max_id_query)
  max_id <- max_id_result$max_id
  max_id <- ifelse(is.na(max_id), 1, as.numeric(max_id) + 1)

  # platesdb table
  sample_list <- sample_list |> dplyr::mutate(list_id = as.integer(max_id))
  platesdb <- data.frame(
    list_id = max_id,
    date = as.character(Sys.Date()),
    description = injec_seq$injec_list$FILE_TEXT[1],
    assoc_plates = paste(injec_seq$plates, collapse = ",")
  )

  # check and for duplicates
  ## against itself
  stopifnot(anyDuplicated(sample_list$file_name) == 0)

  tryCatch(
    {
      DBI::dbBegin(db)

      DBI::dbAppendTable(db, "platesdb", platesdb)
      DBI::dbAppendTable(db, "samples", sample_list)

      DBI::dbCommit(db)
    },
    error = function(e) {
      DBI::dbRollback(db)
      stop(
        "Error writing to database: Might be due to duplicates. Try changing the suffix or plate."
      )
    },
    finally = {
      .close_db(db, TRUE)
    }
  )

  sample_list
}


#' Download sample list from database to local spreadsheet with vendor specific format
#'@param sample_list dataframe of sample list either from db or from write_injec_seq
#'@param vendor currently only 'masslynx', 'masshunter' and 'analyst' are supported
#'
#'@details
#' For all current vendors, the exported format will be in csv format, compatible with the respective software.
#'@export
#'@returns dataframe
download_sample_list <- function(sample_list, vendor) {
  checkmate::assertDataFrame(sample_list)
  checkmate::assertSubset(vendor, c("masslynx", "masshunter", "analyst"), FALSE)

  if (vendor == "masslynx") {
    sample_list <- sample_list |>
      dplyr::rename(ID = "log_id") |>
      dplyr::rename_all(toupper) |>
      dplyr::select(-"CONC") |>
      dplyr::select(
        "FILE_NAME",
        "SAMPLE_LOCATION",
        "FILE_TEXT",
        "TYPE",
        "INJ_VOL",
        starts_with("CONC"),
        starts_with("COMPOUND"),
        "ID"
      ) |>
      dplyr::mutate(FILE_NAME = make.unique(.data$FILE_NAME)) |>
      dplyr::mutate(FILE_NAME = gsub("\\.", "-", .data$FILE_NAME)) |> # replace dots with dash
      dplyr::mutate(Index = dplyr::row_number()) |>
      dplyr::mutate(
        TYPE = dplyr::case_when(
          .data$TYPE == "DQC" ~ "QC",
          TRUE ~ .data$TYPE
        )
      )
  } else if (vendor == "masshunter") {
    sample_list <- sample_list |>
      dplyr::rename_all(toupper) |>
      dplyr::rename(`Sample Name` = "FILE_NAME") |>
      dplyr::rename(`Data File` = "FILE_NAME") |>
      dplyr::rename(Description = "FILE_TEXT") |>
      dplyr::rename(Vial = "SAMPLE_LOCATION") |>
      dplyr::rename(Volume = "INJ_VOL") |>
      dplyr::rename(`Sample Type` = "TYPE") |>
      dplyr::rename(`Dil. factor 1` = "CONC_A") |>
      dplyr::select(
        matches("Data file"),
        matches("Description"),
        matches("Vial"),
        matches("Volume"),
        starts_with("Dil. factor")
      ) |>
      dplyr::mutate(Vial = \(x) {
        # to
        x <- strsplit(x, ":")[[1]]
        tray <- paste0("P", x[1])
        well <- gsub(",", "", x[2])
        paste0(tray, "-", well)
      })
  } else if (vendor == "analyst") {
    sample_list <- sample_list |>
      dplyr::rename_all(toupper) |>
      dplyr::rename(`Sample Name` = "FILE_NAME") |>
      dplyr::rename(`Data File` = "FILE_NAME") |>
      dplyr::rename(Description = "FILE_TEXT") |>
      dplyr::rename(Vial = "SAMPLE_LOCATION") |>
      dplyr::rename(Volume = "INJ_VOL") |>
      dplyr::rename(`Sample Type` = "TYPE") |>
      dplyr::rename(`Dil. factor 1` = "CONC_A") |>
      dplyr::select(
        matches("Data file"),
        matches("Description"),
        matches("Vial"),
        matches("Volume"),
        starts_with("Dil. factor")
      ) |>
      dplyr::mutate(Vial = \(x) {
        # to
        x <- strsplit(x, ":")[[1]]
        tray <- paste0("P", x[1])
        well <- gsub(",", "", x[2])
        paste0(tray, "-", well)
      })
  } else {
    stop("Vendor not supported")
  }
  sample_list
}

#'@export
summary.InjecListObj <- function(object, ...) {
  object$injec_list |>
    summarise(
      total_volume = sum(.data$INJ_VOL),
      .by = c("SAMPLE_LOCATION", "value")
    ) |>
    arrange(desc(.data$total_volume))
}

#'@export
print.InjecListObj <- function(x, ...) {
  cat(
    "Check if total volume is OK. Volume will depend on injection and filtration modes"
  )
  sprintf("Total number of injections %s", nrow(x$injec_list))
  summary(x) |> print()

  return(invisible(x))
}


## get max list_id from db
#'@noRd
.last_list_id <- function() {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")

  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  max_id_query <- "SELECT MAX(list_id) AS max_id FROM platesdb"
  max_id_result <- DBI::dbGetQuery(db, max_id_query)
  max_id <- max_id_result$max_id
  duckdb::dbDisconnect(db, shutdown = TRUE)
  max_id
}
