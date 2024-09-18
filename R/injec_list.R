#' injection List Object
#' @param df dataframe
#' @param plates vector of plate IDs
.injecList <- function(df, plates) {
  s <- list(
    injec_list = df,
    plates = plates
  )

  class(s) <- "InjecListObj"
  s
}


#' Create Injection Sequence
#'
#' @param plate PlateObj object
#' @param inlet_method file path specifying the inlet method.
#' @param repeat_std number of re-injections for calibration standards. Default is 1.
#' @param repeat_analyte number of re-injections for unknown samples. Default is 1
#' @param repeat_qc number of re-injections for QC wells. Default is 1
#' @param blank_after_top_conc If TRUE, adding blank after high concentrations of standards and QCS.
#' @param blank_at_end If True, adding blank at the end of queue.
#' @param system_suitability Number of re-injections for suitability vial.
#' @param blank_every_n If no QCs, frequency of injecting blanks between analytes.
#' @param inject_vol volume of injection in micro liters.
#' @param descr Run description.
#' @param suffix string to be added to the end of the filename. Default is "1".
#' @param prefix string at the beginning of the filename. Default is today's date.
#' @param explore_mode options either TRUE or FALSE. Default if FALSE.
#' @param tray Location in sample manager.
#' @param conc_df data.frame matching compound name to a scaling factor. Maximum 20 compounds allowed.
#'
#' @details
#' explore_mode controls if exploratory samples are to be injected. A random sample from each CS and QC group will be sampled along with 1 blank sample.
#' @returns InjecListObj object
#'@export
build_injec_seq <- function(plate,
                        inlet_method,
                        repeat_std = 1,
                        repeat_qc = 1,
                        repeat_analyte = 1,
                        blank_after_top_conc = TRUE,
                        blank_at_end = TRUE,
                        system_suitability = 0,
                        blank_every_n = NULL,
                        inject_vol,
                        descr = "",
                        prefix = Sys.Date(),
                        suffix = "1",
                        tray = 1,
                        explore_mode = FALSE,
                        conc_df = NULL) {
    UseMethod("build_injec_seq")
}


#'@export
#'@returns InjecListObj object
build_injec_seq.MultiPlate <- function( plate, inlet_method,
  repeat_std = 1, repeat_qc = 1, repeat_analyte = 1,
  blank_after_top_conc = TRUE, blank_at_end = TRUE, system_suitability = 0,
  blank_every_n = NULL, inject_vol, descr = "",
  prefix = Sys.Date(), suffix = "1", tray, explore_mode = FALSE, conc_df = NULL) {

  checkmate::assertCharacter(tray, min.len = 1, max.len = 12, unique = TRUE)

  if(length(plate) == 1){
    plate <- plate[[1]]
  } else{
    ## assert length of tray is equal to number of plates
    if(length(tray) != length(plate)){
      stop("Number of tray slots must be equal to number of plates")
    }
    ## assert all plates are registered
    if(!all(sapply(plate, .is_registered))){
      stop("All plates are not registered. Please register the plates first.")
    }

    m <- lapply(plate, function(x) x$plate)
    m <- do.call(rbind, m)

    df <- lapply(1:length(plate), function(i){
      x <- plate[[i]]$df
      x$tray <- tray[i]
      x
    })

    df <- do.call(rbind, df)

    plate_id <- sapply(plate, function(x) x$plate_id)

    descr <- sapply(plate, function(x) x$descr) |> paste0(collapse = ", ")

    empty_rows <- sapply(plate, function(x) x$empty_rows)

    last_modified <- sapply(plate, function(x) x$last_modified)

    plate <- .plate(m, df, plate_id, empty_rows, last_modified, descr = )
    class(plate) <- c("RegisteredPlate", "PlateObj")

  }

  build_injec_seq(plate, inlet_method = inlet_method,
                  repeat_std = repeat_std, repeat_qc = repeat_qc, repeat_analyte = repeat_analyte,
                  blank_after_top_conc = blank_after_top_conc, blank_at_end = blank_at_end,
                  system_suitability = system_suitability, blank_every_n = blank_every_n,
                  inject_vol = inject_vol, descr = descr, prefix = prefix, suffix = suffix,
                  tray = tray, explore_mode = explore_mode, conc_df = conc_df)

}

#' @importFrom dplyr bind_rows bind_cols mutate add_row filter arrange count group_by group_modify ungroup select
#' @export
#' @returns InjecListObj object
build_injec_seq.PlateObj <- function(plate,
                        inlet_method,
                        repeat_std = 1,
                        repeat_qc = 1,
                        repeat_analyte = 1,
                        blank_after_top_conc = TRUE,
                        blank_at_end = TRUE,
                        system_suitability = 0,
                        blank_every_n = NULL,
                        inject_vol,
                        descr = "",
                        prefix = Sys.Date(),
                        suffix = "1",
                        tray = 1,
                        explore_mode = FALSE,
                        conc_df = NULL) {
  # checkmate::assertFile(inlet_method)
  checkmate::assertNumber(repeat_std, finite = TRUE, lower = 1)
  checkmate::assertNumber(repeat_qc, finite = TRUE, lower = 1)
  checkmate::assertNumber(repeat_analyte, finite = TRUE, lower = 1)
  checkmate::assertNumeric(inject_vol, finite = TRUE, lower = 0.1)
  checkmate::assertNumber(blank_every_n, null.ok = TRUE, lower = 1, finite = TRUE)
  checkmate::assertNumber(system_suitability, lower = 0, finite = TRUE)
  checkmate::assertChoice(explore_mode, choices = c(TRUE, FALSE))
  checkmate::checkString(descr, null.ok = TRUE)
  # checkmate::assertString(prefix)
  checkmate::assertString(suffix)
  checkmate::assertCharacter(tray, min.len = 1, max.len = 12, unique = TRUE)
  # checkmate::assertString(tray)
  checkmate::assertDataFrame(conc_df,
    null.ok = TRUE,
    min.rows = 1,
    max.rows = 20,
    type = c("character", "numeric"),
    col.names = "named", ncols = 2, any.missing =  FALSE)

  # assert plate is registered
  if(!.is_registered(plate)){
    stop("Plate is not registered. Please register the plate first.")
  }

  current_plate_id <- plate$plate_id

  # add tray column if single tray (previous call will make it non-NULL if multiplate)
  if(!("tray" %in% colnames(plate$df))){
    stopifnot(length(tray) == 1)
    if(length(tray) != 1){
      stop("Tray must be a single value for single plate")
    }
    plate$df$tray <- tray
  }
  plate <-
    plate$df |> dplyr::mutate(SAMPLE_LOCATION = paste0(tray, ":", .data$SAMPLE_LOCATION))

  df <- plate[FALSE, ] # empty df, same dims

  double_blanks <- dplyr::filter(plate, .data$TYPE == "DoubleBlank")
  IS_blanks <- dplyr::filter(plate, .data$TYPE == "ISBlank")
  # locate positive blanks
  blank_list <- dplyr::filter(plate, .data$TYPE == "Blank")
  # find top conc in std
  std_list <- dplyr::filter(plate, .data$TYPE == "Standard") |> dplyr::arrange(as.numeric(.data$std_rep), as.numeric(.data$conc))
  # find top conc in qc
  qc_list <- dplyr::filter(plate, .data$TYPE == "QC") |>  dplyr::arrange(as.numeric(.data$std_rep), .data$value)
  analyte_list <- dplyr::filter(plate, .data$TYPE == "Analyte") |> dplyr::arrange(.data$samples)

  suitability_list <- filter(plate, .data$TYPE == "Suitability")



  no_qc <- ifelse(nrow(qc_list) == 0, TRUE, FALSE) #
  no_analyte <- ifelse(nrow(analyte_list) == 0, TRUE, FALSE)


  if (!no_qc) {
    stopifnot(nrow(qc_list) %% 4 == 0)
    qc_replicates <-
      qc_list |>
      dplyr::count(.data$value, .by = "value") |>
      dplyr::pull(n) |>
      unique()
    stopifnot(length(qc_replicates) == 1)
  }

  ## 1. xplore mode
  if(explore_mode){
    xplore_df <- df[FALSE, ] # empty df
    # add random sample from each group
    if(nrow(std_list) > 0){
      std_xplore <- std_list |>
        dplyr::group_by(.data$std_rep) |>
        dplyr::sample_n(1) |>
        dplyr::ungroup()
        xplore_df <-  rbind(xplore_df, std_xplore)
    }

    if(nrow(qc_list) > 0){
      qc_xplore <- qc_list |>
        dplyr::group_by(.data$std_rep) |>
        dplyr::sample_n(1) |>
        dplyr::ungroup()
        xplore_df <-  rbind(xplore_df, qc_xplore)
    }

    if(nrow(analyte_list) > 0){
      analyte_xplore <- analyte_list |>
        dplyr::sample_n(1)
        xplore_df <-  rbind(xplore_df, analyte_xplore)
    }

    if(nrow(blank_list) > 0){
      blank_xplore <- blank_list |>
        dplyr::sample_n(1)
      xplore_df <-  rbind(xplore_df, blank_xplore)
    }

    xplore_df <- xplore_df |>
      mutate(value = paste0(.data$value, "_explore"))

    df <- bind_rows(df, xplore_df)
  }

  for(i in 1:2){
  # double blank
    df <- add_row(df, double_blanks)
    # IS blank
    df <- add_row(df, IS_blanks)
  }

  # add suitability
  if (system_suitability > 0) {
    stopifnot("There is no suitability well in the plate. Please add it using add_suitability()" = nrow(suitability_list) >= 1)
    # n_blanks <- nrow(blank_list)
    # stopifnot(n_blanks >=2) # FIXME

    # df <- add_row(df,
    #     mutate(blank_list, value = paste0(value, "-suitability"))[rep(1, system_suitability),]
    #  )
    # df <- add_row(df, blank_list[-1,])

    for (i in seq(system_suitability)) {
      df <- add_row(df, suitability_list)
    }
  }

  #add blanks
  df <- add_row(df, blank_list)

  # add standards
  for (i in seq(repeat_std)) {
    df <- bind_rows(df, std_list)

    if (blank_after_top_conc) {
      df <- bind_rows(df, blank_list)
    }
  }

  if (no_qc & !no_analyte) {
    # inject analyte if no QCs
    for (i in seq(repeat_analyte)) {
      if (!is.null(blank_every_n)) {
        analyte_list <- .add_every_n(analyte_list, blank_list, blank_every_n)
      }

      df <- bind_rows(df, analyte_list)

      if (blank_after_top_conc) {
        df <- bind_rows(df, blank_list)
      }
    }
  }

  if (!no_qc) {
    # TODO repeat analytes and qcs  with n_analyte and n_qc
    if (!no_analyte) {
      # divide analyte list by number of QCs
      fac <- round(nrow(analyte_list) / qc_replicates)
      fac <-
        sort(rep(
          1:qc_replicates,
          by = fac,
          length.out = nrow(analyte_list)
        ))
      analyte_list <- analyte_list |> split(fac)
    }

    group <- rep(1:qc_replicates, length.out = nrow(qc_list))
    qc_list <- qc_list |> split(group)

    # add qc
    for (i in seq_along(qc_list)) {
      df <- bind_rows(df, qc_list[[i]])
      if (!no_analyte) {
        df <- bind_rows(df, analyte_list[[i]])
      }

      if (blank_after_top_conc) {
        df <- bind_rows(df, blank_list)
      }
    }



    if (!blank_after_top_conc & blank_at_end) {
      df <- bind_rows(df, blank_list)
    }
  }



  if(!is.null(conc_df)){
    # add conc_df to plate
    conc_df <- t(conc_df)
    cmpd_vec <- conc_df[1,]
    cmpd_names <- paste0("COMPOUND_", LETTERS[seq_along(cmpd_vec)])
    conc_vec <- conc_df[2,]
    conc_names <- paste0("CONC_", LETTERS[seq_along(conc_vec)])
    conc_df = data.frame(matrix(nrow = 1, ncol = length(cmpd_vec)*2))
    colnames(conc_df) <- c(cmpd_names, conc_names)
    conc_df[1:length(cmpd_vec)] <- cmpd_vec
    conc_df[(length(cmpd_vec)+1):(length(cmpd_vec)*2)] <- conc_vec

    # min_conc <- min(as.numeric(df$conc))
    df <- df |> dplyr::bind_cols(conc_df) |>  # bind conc_df
          dplyr::mutate(dplyr::across(starts_with("CONC_"),
                                \(x) (as.numeric(x) * as.numeric(.data$conc)))) # multiply conc_df with conc and divide by min conc

  } else{
    df <- dplyr::mutate(df, CONC_A = .data$conc)

  }

  # create filename
  ## Date
  df <- df |>
    dplyr::mutate(
      Index = dplyr::row_number(),
      FILE_NAME = paste0(prefix, "_", .data$value, "_", suffix),
      INJ_VOL = inject_vol,
      # CONC_A = conc,
      FILE_TEXT = descr,
      INLET_METHOD = inlet_method
    )


  # TODO
  # if(!is.null(conc_df)){
  #   names <- names(conc_df)
  #   for(i in seq_along(names)){
  #     df <- df |> mutate( {{LETTERS[i]}} = names[i])
  #   }
  #   df |> mutate("Compound_A" = names[1], "Compound_B" = names[2])
  # }

  x <- .injecList(df, current_plate_id)
  print(x)
}


#' Interject dataframe every Nth position
#'
#' @param df original dataframe
#' @param add_df dataframe to add
#' @param every_n number of rows to interject add_df
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
  function(sample_lists, n_equi = 10, equi_pos, equi_prefix = Sys.Date(), equi_suffix = "equi", equi_injec_vol =0.5) {
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
      dplyr::filter(sample_lists[[1]]$injec_list, col == col_i & row == which(LETTERS == row_i)) |>
      dplyr::slice_head(n = 1) |>
      dplyr::mutate(INJ_VOL = equi_injec_vol) |>
      dplyr::mutate(FILE_NAME = paste0(equi_prefix, "_", .data$value, "_", equi_suffix)) |>
      dplyr::mutate(FILE_TEXT = paste0("equilibriate"))

    stopifnot( "Equilibriation position not found in the plate. Please check equi_pos." = nrow(equi_df) >= 1)

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


# create it if not exists
.check_sample_db <- function() {

  db_path <- rappdirs::user_data_dir() |>
    file.path("PKbioanalysis/samples.db")

  # Check if the database file exists
  db <- duckdb::dbConnect(duckdb::duckdb(), db_path)
  DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS samples (
    file_name TEXT PRIMARY KEY,
    list_id INTEGER,
    inlet_method TEXT,
    row INTEGER,
    col INTEGER,
    value TEXT,
    sample_location TEXT,
    samples TEXT,
    type TEXT,
    std_rep INTEGER,
    tray TEXT,
    inj_vol REAL,

    conc_a TEXT,
    conc_b TEXT,
    conc_c TEXT,
    conc_d TEXT,
    conc_e TEXT,
    conc_f TEXT,
    conc_g TEXT,
    conc_h TEXT,
    conc_i TEXT,
    conc_j TEXT,
    conc_k TEXT,
    conc_l TEXT,
    conc_m TEXT,
    conc_n TEXT,
    conc_o TEXT,
    conc_p TEXT,

    compound_a TEXT,
    compound_b TEXT,
    compound_c TEXT,
    compound_d TEXT,
    compound_e TEXT,
    compound_f TEXT,
    compound_g TEXT,
    compound_h TEXT,
    compound_i TEXT,
    compound_j TEXT,
    compound_k TEXT,
    compound_m TEXT,
    compound_n TEXT,
    compound_l TEXT,
    compound_o TEXT,
    compound_p TEXT,

    file_text TEXT,
    conc TEXT,
    time TEXT,
    factor TEXT,
    UNIQUE(file_name)
  );
")

# This id auto increments and is assigned to list_id above
DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS metadata (
    id INTEGER PRIMARY KEY,
    date TEXT,
    assoc_plates TEXT,
    description TEXT,
    UNIQUE(id)
  );
") # id, date, assoc_plates


DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS peakstab (
    peak_id INTEGER PRIMARY KEY,
    file_name TEXT NOT NULL,
    compound TEXT,
    compound_id INTEGER NOT NULL,
    transition_id INTEGER NOT NULL,
    observed_rt REAL,
    observed_rt_start REAL,
    observed_rt_end REAL,
    observed_peak_height REAL,
    area REAL,
    manual INTEGER NOT NULL DEFAULT 0,
    date TEXT,
    UNIQUE(peak_id)
  );

")

DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS transtab (
    transition_id INTEGER PRIMARY KEY,
    transition_label TEXT,
    q1 REAL,
    q3 REAL,
    inlet_method TEXT,
    UNIQUE(transition_id),
    UNIQUE(q1, q3, inlet_method)
  );
")


DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS compoundstab (
    compound_id INTEGER PRIMARY KEY,
    compound TEXT,
    transition_id INTEGER,
    expected_rt_start REAL,
    expected_rt_end REAL,
    expected_rt REAL,
    IS_id INTEGER,
    UNIQUE(compound_id)
  );

")

duckdb::dbDisconnect(db, shutdown = TRUE)
}

#' Export injection sequence to vendor specific format
#'
#' @param injec_seq InjecListObj object
#'
#' @import checkmate
#' @import dplyr
#' @import rappdirs
#'
#' @export
#' @returns dataframe
write_injec_seq <- function(injec_seq){
  checkmate::assertClass(injec_seq, "InjecListObj")


  # Modify sample list
  sample_list <- dplyr::mutate(injec_seq$injec_list,
    FILE_NAME = paste0(.data$FILE_NAME, "_R", row_number())) |>
    dplyr::rename_all(tolower) |>
    select(-matches("index"))


  db_path <- rappdirs::user_data_dir() |>
    file.path("PKbioanalysis/samples.db")

  .check_sample_db()

  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)

  # find last unqiue ID and add 1
  max_id_query <- "SELECT MAX(id) AS max_id FROM metadata"
  max_id_result <- DBI::dbGetQuery(db, max_id_query)
  max_id <- max_id_result$max_id
  max_id <- ifelse(is.na(max_id), 1, as.numeric(max_id) + 1)

  # metadata table
  sample_list <- sample_list |> dplyr::mutate(list_id = as.integer(max_id))
  metadatadb <- data.frame(id = max_id,
    date = as.character(Sys.Date()),
    description = injec_seq$injec_list$FILE_TEXT[1],
    assoc_plates = paste(injec_seq$plates, collapse = ","))

  # check and for duplicates
  ## against itself
  stopifnot(anyDuplicated(sample_list$file_name) == 0)


  ## against db
  tryCatch({
    DBI::dbWriteTable(db, "samples", sample_list, append = TRUE, row.names = FALSE)
    DBI::dbWriteTable(db, "metadata", metadatadb, append = TRUE, row.names = FALSE)
    duckdb::dbDisconnect(db, shutdown = TRUE)
  }, error = function(e) {
    duckdb::dbDisconnect(db, shutdown = TRUE)
    message("Previous samples with same name deteted. Change prefix or suffix to avoid duplicates.")
  })

  sample_list
}


#' Download sample list from database to local spreadsheet
#'@param sample_list dataframe of sample list either from db or from write_injec_seq
#'@param vendor currently only 'masslynx' and 'masshunter' are supported
#'
#'@details
#'For 'masslynx' and 'masshunter', the exported format will be in csv format, compatible with the respective software.
#'@export
#'@returns dataframe
download_sample_list <- function(sample_list, vendor){
  checkmate::assertDataFrame(sample_list)
  checkmate::assertSubset(vendor, c("masslynx", "masshunter"), FALSE)

  if (vendor == "masslynx") {
    sample_list <- sample_list |>
      dplyr::rename_all(toupper) |>
      dplyr::select(matches("FILE_NAME"), matches("SAMPLE_LOCATION"),
        matches("FILE_TEXT"),
        matches("TYPE"), matches("INJ_VOL"),
        starts_with("CONC"), starts_with("COMPOUND")) |>
      dplyr::mutate(Index = dplyr::row_number())

  } else if(vendor == "masshunter"){

    sample_list <- sample_list |>
      dplyr::rename_all(toupper) |>
      dplyr::rename(`Sample Name` = .data$FILE_NAME) |>
      dplyr::rename(`Data File` = .data$FILE_NAME) |>
      dplyr::rename(Description = .data$FILE_TEXT) |>
      dplyr::rename(Vial = .data$SAMPLE_LOCATION) |>
      dplyr::rename(Volume = .data$INJ_VOL) |>
      dplyr::rename(`Sample Type` = .data$TYPE) |>
      dplyr::rename(`Dil. factor 1` = .data$CONC_A) |>
      dplyr::select(matches("Data file"), matches("Description"),
        matches("Vial"), matches("Volume"), starts_with("Dil. factor")) |>
      dplyr::mutate(Vial = \(x){ # to
        x <- strsplit(x, ":")[[1]]
        tray <- paste0("P", x[1])
        well <- gsub(",", "", x[2])
        paste0(tray, "-", well)
      }

      )

  } else {
    stop("Vendor not supported")
  }
  sample_list
}

#'@export
print.InjecListObj <- function(x, ...) {

  cat("Check if total volume is OK. Volume will depend on injection and filtration modes")
  sprintf("Total number of injections %s", nrow(x$injec_list))
  x$injec_list |>
    summarize(total_volume = sum(.data$INJ_VOL), .by = "SAMPLE_LOCATION") |>
    arrange(desc(.data$total_volume)) |> print()

  return(invisible(x))

}

.reset_samples_db <- function() {
  db_path <- rappdirs::user_data_dir() |>
    file.path("PKbioanalysis/samples.db")
    # rename
  file.rename(db_path, paste0(db_path, "_old"))
}

# return metadata table for sample list
.get_samplesdb_metadata <- function(){
  .check_sample_db()

  db_path <- rappdirs::user_data_dir() |>
    file.path("PKbioanalysis/samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  metadata <- DBI::dbGetQuery(db, "SELECT * FROM metadata")
  duckdb::dbDisconnect(db, shutdown = TRUE)

  metadata
}

.get_samplelist <- function(id){
  .check_sample_db()
  db_path <- rappdirs::user_data_dir() |>
    file.path("PKbioanalysis/samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  sample_list <- DBI::dbGetQuery(db, paste0("SELECT * FROM samples WHERE list_id = ", id))
  duckdb::dbDisconnect(db, shutdown = TRUE)
  sample_list
}


## get max list_id from db
.last_list_id <- function(){
    db_path <- rappdirs::user_data_dir() |>
        file.path("PKbioanalysis/samples.db")
    db <- duckdb::dbConnect( duckdb::duckdb(), dbdir = db_path)
    max_id_query <- "SELECT MAX(id) AS max_id FROM metadata"
    max_id_result <- DBI::dbGetQuery(db, max_id_query)
    max_id <- max_id_result$max_id
    duckdb::dbDisconnect(db, shutdown = TRUE)
    max_id
}



.connect_to_db <- function(){
  db_path <- rappdirs::user_data_dir() |>
    file.path("PKbioanalysis/samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  db

}
