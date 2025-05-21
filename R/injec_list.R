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

#' Write injection sequence to database
#'
#' @param injec_seq InjecListObj object
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


  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")

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
    message("Error writing to database: ", e$message)
    # message("Previous samples with same name deteted. Change prefix or suffix to avoid duplicates.")
  })

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
download_sample_list <- function(sample_list, vendor){
  checkmate::assertDataFrame(sample_list)
  checkmate::assertSubset(vendor, c("masslynx", "masshunter", "analyst"), FALSE)

  if (vendor == "masslynx") {
    sample_list <- sample_list |>
      dplyr::rename_all(toupper) |>
      dplyr::select(matches("FILE_NAME"), matches("SAMPLE_LOCATION"),
        matches("FILE_TEXT"),
        matches("TYPE"), matches("INJ_VOL"),
        starts_with("CONC"), starts_with("COMPOUND")) |>
      dplyr::mutate(Index = dplyr::row_number()) |> 
      dplyr::mutate(TYPE = dplyr::case_when(
        .data$TYPE == "DQC" ~ "QC",
        TRUE ~ .data$TYPE
      )) 
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

  } else if(vendor == "analyst"){

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

  }else {
    stop("Vendor not supported")
  }
  sample_list
}

#'@export
print.InjecListObj <- function(x, ...) {

  cat("Check if total volume is OK. Volume will depend on injection and filtration modes")
  sprintf("Total number of injections %s", nrow(x$injec_list))
  x$injec_list |>
    summarise(total_volume = sum(.data$INJ_VOL), .by = "SAMPLE_LOCATION") |>
    arrange(desc(.data$total_volume)) |> print()

  return(invisible(x))

}



## get max list_id from db
#'@noRd
.last_list_id <- function(){
    db_path <- PKbioanalysis_env$data_dir |>
        file.path("samples.db")

    db <- duckdb::dbConnect( duckdb::duckdb(), dbdir = db_path)
    max_id_query <- "SELECT MAX(id) AS max_id FROM metadata"
    max_id_result <- DBI::dbGetQuery(db, max_id_query)
    max_id <- max_id_result$max_id
    duckdb::dbDisconnect(db, shutdown = TRUE)
    max_id
}
