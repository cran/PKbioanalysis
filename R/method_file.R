#' Create a new methods database from a YAML file
#' @param path Path to the YAML file containing method information
#' @keywords internal
.parse_cmpds <- function(path) {
  checkmate::assert_file_exists(path)
  res <- yaml::read_yaml(path)
  res$compounds <- do.call(
    rbind,
    lapply(res$compounds, function(x) {
      data.frame(
        compound = x$cmpd,
        q1 = x$q1,
        q3 = x$q3,
        qualifier = x$qualifier,
        expected_peak_start = ifelse(
          is.null(x$expected_peak_start),
          NA,
          x$expected_peak_start
        ),
        expected_peak_end = ifelse(
          is.null(x$expected_peak_end),
          NA,
          x$expected_peak_end
        ),
        expected_rt = ifelse(is.null(x$expected_rt), NA, x$expected_rt),
        IS_id = ifelse(is.null(x$IS), NA, x$IS),
        stringsAsFactors = FALSE
      )
    })
  )
  if (any(!is.na(res$compounds$IS_id))) {
    checkmate::assert(
      all(
        res$compounds$IS_id[!is.na(res$compounds$IS_id)] %in%
          res$compounds$compound
      ),
      msg = "If a compound has an IS, the IS must be listed in another compound entry."
    )
  }

  res
}

.check_cmpd_list <- function(cmpds_list) {
  checkmate::assertList(cmpds_list)

  # check there is one method, description and compounds
  checkmate::assertNames(
    names(cmpds_list),
    must.include = c(
      "method",
      "description",
      "gradient",
      "compounds",
      "column"
    ),
    type = "unique"
  )

  checkmate::assertDataFrame(cmpds_list$compounds)
  checkmate::assertNames(
    names(cmpds_list$compounds),
    must.include = c("compound", "q1", "q3", "qualifier"),
    type = "unique"
  )

  if (any(duplicated(cmpds_list$compounds[, c("q1", "q3")]))) {
    stop(
      "The combination of q1 and q3 must be unique in the compounds data frame."
    )
  }

  if (any(is.na(cmpds_list$compounds$compound))) {
    stop("Must supply all compound names")
  }

  # add missing cols
  if (!"expected_peak_start" %in% names(cmpds_list$compounds)) {
    cmpds_list$compounds$expected_peak_start <- NA
  }
  if (!"expected_peak_end" %in% names(cmpds_list$compounds)) {
    cmpds_list$compounds$expected_peak_end <- NA
  }
  if (is.null(cmpds_list$compounds$IS_id)) {
    cmpds_list$compounds$IS_id <- NA
  }
  cmpds_list
}

#' Save methods database
#' @param cmpds_list List returned from .parse_cmpds()
#' @param method_id If NULL, a new method ID will be created. If numeric, the existing method ID will be updated.
#' @keywords internal
#' @returns NULL
#' @noRd
#' @author Omar I. Elashkar
.save_cmpd_db <- function(cmpds_list, method_id = NULL) {
  cmpds_list <- .check_cmpd_list(cmpds_list)

  # drop empty rows (shiny)
  cmpds_list$compounds <- cmpds_list$compounds[
    !is.na(cmpds_list$compounds$compound),
  ]

  # compound names doen't have to be unique

  .check_sample_db()
  db <- .connect_to_db()
  on.exit(.close_db(db, gc = TRUE), add = TRUE)

  if (is.null(method_id)) {
    # create new method ID
    max_method_id <- DBI::dbGetQuery(
      db,
      "SELECT MAX(method_id) FROM methodstab"
    ) |>
      as.numeric() |>
      max()
    method_id <- ifelse(is.na(max_method_id), 1, max_method_id + 1)
  }

  max_cmpd_id <- DBI::dbGetQuery(
    db,
    "SELECT MAX(compound_id) FROM compoundstab"
  ) |>
    as.numeric() |>
    max()
  cmpd_id <- ifelse(is.na(max_cmpd_id), 1, max_cmpd_id + 1)

  max_trans_id <- DBI::dbGetQuery(
    db,
    "SELECT MAX(transition_id) FROM transtab"
  ) |>
    as.numeric() |>
    max()
  trans_id <- ifelse(is.na(max_trans_id), 1, max_trans_id + 1)

  unique_methods_df <- data.frame(method_id = method_id) |>
    dplyr::mutate(method = cmpds_list$method) |>
    dplyr::mutate(method_column = cmpds_list$column) |>
    dplyr::mutate(method_descr = cmpds_list$description) |>
    dplyr::mutate(method_gradient = cmpds_list$gradient) |>
    dplyr::distinct()

  stopifnot(nrow(unique_methods_df) == 1) # only single method passed from this call.

  # create method tab
  unique_trans_df <- cmpds_list$compounds |>
    dplyr::select("q1", "q3") |>
    dplyr::arrange("q1", "q3") |>
    dplyr::distinct() |>
    dplyr::mutate(method_id = method_id) |>
    dplyr::mutate(transition_label = paste0(.data$q1, " > ", .data$q3)) |>
    dplyr::mutate(transition_id = seq(trans_id, trans_id + dplyr::n() - 1))

  # avoid repeated transitions in the same method
  # assert combination of q1 and q3 is unique to be saved in transition table
  checkmate::assertVector(unique_trans_df$transition_label, unique = TRUE)

  checkmate::assertNames(
    names(unique_methods_df),
    must.include = c(
      "method_id",
      "method_descr",
      "method_gradient",
      "method_column",
      "method"
    ),
    type = "unique"
  )

  # autoincrement compound_id
  cmpd_id <- seq(cmpd_id, nrow(cmpds_list$compounds) + cmpd_id - 1)

  # join transition_id to compoundstab
  transitions_df <- cmpds_list$compounds |>
    dplyr::left_join(unique_trans_df, by = c("q1", "q3"))

  # Begin a transaction
  DBI::dbBegin(db)

  tryCatch(
    {
      # Add to methodstab
      DBI::dbAppendTable(db, "methodstab", unique_methods_df)

      # Add trans first to check if they were added before adding the entire method.
      DBI::dbAppendTable(db, "transtab", unique_trans_df)

      # Add to compoundstab. Add all compound names, but only one method_id
      DBI::dbAppendTable(
        db,
        "compoundstab",
        transitions_df |>
          dplyr::mutate(method_id = method_id) |>
          dplyr::mutate(compound_id = cmpd_id) |>
          dplyr::mutate(
            IS_id = ifelse(
              is.na(.data$IS_id),
              .data$IS_id,
              cmpd_id[match(.data$IS_id, transitions_df$compound)]
            ) |>
              as.character()
          ) |>
          dplyr::mutate(
            qualifier = ifelse(is.na(.data$qualifier), FALSE, .data$qualifier)
          ) |>
          dplyr::select(
            "compound_id",
            "compound",
            "qualifier",
            "IS_id",
            "expected_peak_start",
            "expected_peak_end",
            "transition_id"
          )
      )

      DBI::dbCommit(db)
    },
    error = function(e) {
      # Roll back the transaction if any operation fails
      DBI::dbRollback(db)
      stop("Transaction failed: ", e$message)
    }
  )
}

#' Load methods database
#' @noRd
.get_methodsdb <- function() {
  .check_sample_db()

  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)

  methods <- DBI::dbReadTable(db, "methodstab")
  methods
}

.get_method_transitions <- function(method_id) {
  .check_sample_db()
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  transitions <- DBI::dbGetQuery(
    db,
    paste0("SELECT * FROM transtab WHERE method_id = ", method_id)
  ) |>
    as.data.frame()
  transitions
}

.get_method_cmpds <- function(method_id) {
  .check_sample_db()
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)

  if (!(method_id %in% .get_methodsdb()$method_id)) {
    stop("Method ID ", method_id, " not found in database.")
  }

  transitions <- .get_method_transitions(method_id)

  if (nrow(transitions) == 0) {
    stop("No transitions found for method_id ", method_id)
  }

  cmpds <- DBI::dbGetQuery(
    db,
    paste0(
      "SELECT * FROM compoundstab WHERE transition_id IN (",
      paste(transitions$transition_id, collapse = ","),
      ")"
    )
  ) |>
    as.data.frame()

  if (is.null(cmpds) | nrow(cmpds) == 0) {
    stop("No compounds found for method_id ", method_id)
  }

  cmpds |>
    dplyr::left_join(transitions, by = "transition_id")
}

.get_method_id <- function(method) {
  .check_sample_db()
  db <- .connect_to_db()
  on.exit(.close_db(db), add = TRUE)
  method_id <- DBI::dbGetQuery(
    db,
    paste0("SELECT method_id FROM methodstab WHERE method = '", method, "'")
  ) |>
    as.numeric()
  method_id
}

modify_method <- function(method_id, new_list) {
  checkmate::assertNumeric(method_id, len = 1)
  new_list <- .check_cmpd_list(new_list)

  new_list$compounds <- new_list$compounds[
    !is.na(new_list$compounds$compound),
  ] # remove empty rows

  .check_sample_db()
  # check if method_id exists in db
  if (!(method_id %in% .get_methodsdb()$method_id)) {
    stop("Method ID ", method_id, " not found in database.")
  }

  db <- .connect_to_db()
  DBI::dbBegin(db)
  tryCatch(
    {
      # delete dependent records first
      nrowtrans <- DBI::dbExecute(
        db,
        paste0(
          "DELETE FROM compoundstab WHERE transition_id IN (SELECT transition_id FROM transtab WHERE method_id = ",
          method_id,
          ")"
        )
      )
      checkmate::assert(
        nrowtrans > 0,
        msg = "No compounds found to update for method_id ",
        method_id
      )
      DBI::dbCommit(db) # commit deletion of dependent records first before adding new ones
    },
    error = function(e) {
      # Roll back the transaction if any operation fails
      DBI::dbRollback(db)
      stop("Transaction failed: ", e$message)
    },
    finally = {
      .close_db(db, gc = TRUE)
    }
  )

  db <- .connect_to_db()
  DBI::dbBegin(db)
  tryCatch(
    {
      nrowtrans <- DBI::dbExecute(
        db,
        paste0("DELETE FROM transtab WHERE method_id = ", method_id)
      )
      checkmate::assert(
        nrowtrans > 0,
        "No transitions found to update for method_id ",
        method_id
      )
      DBI::dbCommit(db) # commit deletion of dependent records first before adding new ones
    },
    error = function(e) {
      DBI::dbRollback(db)
      stop("Transaction failed: ", e$message)
    },
    finally = {
      .close_db(db, gc = TRUE)
    }
  )

  db <- .connect_to_db()
  DBI::dbBegin(db)
  tryCatch(
    {
      nrowtrans <- DBI::dbExecute(
        db,
        paste0("DELETE FROM methodstab WHERE method_id = ", method_id)
      )
      checkmate::assert(
        nrowtrans > 0,
        "No method found to update for method_id ",
        method_id
      )
      DBI::dbCommit(db) # commit deletion of method record
    },
    error = function(e) {
      # Roll back the transaction if any operation fails
      DBI::dbRollback(db)
      stop("Transaction failed: ", e$message)
    },
    finally = {
      .close_db(db, gc = TRUE)
    }
  )

  .save_cmpd_db(new_list, method_id = method_id)
}
