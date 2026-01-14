#' @include class.R generics.R
NULL


#' @param plate a 96-well matrix
#' @param df data.frame contains plate's metadata
#' @param empty_rows a vector for current active rows
#' @param last_modified last modified date
#' @param plate_id plate id
#' @param descr plate description
#'
#' @importFrom dplyr mutate slice_tail
#' @noRd
# Define the PlateObj class
setClass(
  "PlateObj",
  slots = list(
    plate = "matrix",
    df = "data.frame",
    samples_metadata = "data.frame",
    empty_rows = "character",
    filling_scheme = "list",
    last_filled = "character",
    last_modified = "POSIXct",
    plate_id = "character",
    descr = "character"
  )
)

setClass("RegisteredPlate", contains = "PlateObj")

setClass(
  "MultiPlate",
  slots = list(
    plates = "list"
  )
)

#' Subsetting method for MultiPlate
#' @param x MultiPlate object
#' @param i index
#' @param j index
#' @param ... additional arguments
#' @export
#' @returns PlateObj object
setMethod(
  "[[",
  signature(x = "MultiPlate", i = "ANY", j = "ANY"),
  function(x, i, ...) {
    x@plates[[i]]
  }
)

#' Length method for MultiPlate
#' @param x MultiPlate object
#' @export
#' @returns number of plates
setMethod("length", signature(x = "MultiPlate"), function(x) {
  length(x@plates)
})


#' Create Injection Sequence from PlateObj (Single Plate)
#' @rdname build_injec_seq
#' @export
#' @keywords internal
#' @returns InjecListObj object
setMethod(
  "build_injec_seq",
  "PlateObj",
  function(
    plate,
    method,

    rep_DB = 2,
    rep_ISblank = 1,
    rep_suitability = 1,
    rep_blank = 2,
    repeat_std = 1,
    repeat_qc = 2,
    repeat_analyte = 1,
    repeat_dqc = 1,
    n_explore = 0,

    blank_after_top_conc = TRUE,
    blank_at_end = TRUE,
    blank_every_n = NULL,
    injec_vol,
    descr = "",
    prefix = Sys.Date(),
    suffix = "1",
    tray = 1,
    conc_df = NULL,
    grouped = TRUE
  ) {
    checkmate::assertNumber(repeat_std, finite = TRUE, lower = 1)
    checkmate::assertNumber(repeat_qc, finite = TRUE, lower = 1)
    checkmate::assertNumber(repeat_analyte, finite = TRUE, lower = 1)
    checkmate::assertNumeric(injec_vol, finite = TRUE, lower = 0.1)
    checkmate::assertNumber(
      blank_every_n,
      null.ok = TRUE,
      lower = 1,
      finite = TRUE
    )
    checkmate::assertNumber(rep_DB, finite = TRUE, lower = 1)
    checkmate::assertNumber(rep_ISblank, finite = TRUE, lower = 1)
    checkmate::assertNumber(rep_suitability, finite = TRUE, lower = 0)
    checkmate::assertNumber(rep_blank, finite = TRUE, lower = 1)
    checkmate::assertNumber(n_explore, finite = TRUE, lower = 0, upper = 4)

    checkmate::assertLogical(blank_after_top_conc, len = 1)
    checkmate::assertLogical(blank_at_end, len = 1)
    checkmate::assertLogical(grouped, len = 1)

    checkmate::checkString(descr, null.ok = TRUE)
    # checkmate::assertString(prefix)
    checkmate::assertString(suffix)
    checkmate::assertCharacter(tray, min.len = 1, max.len = 12, unique = TRUE)
    # checkmate::assertString(tray)
    checkmate::assertDataFrame(
      conc_df,
      null.ok = TRUE,
      min.rows = 1,
      max.rows = 20,
      type = c("character", "numeric"),
      col.names = "named",
      ncols = 2,
      any.missing = FALSE
    )

    # assert plate is registered
    if (!.is_registered(plate)) {
      stop("Plate is not registered. Please register the plate first.")
    }

    current_plate_id <- plate@plate_id

    # add tray column if single tray (previous call will make it non-NULL if multiplate)
    if (!("tray" %in% colnames(plate@df))) {
      stopifnot(length(tray) == 1)
      if (length(tray) != 1) {
        stop("Tray must be a single value for single plate")
      }
      plate@df$tray <- tray
    }
    plate <-
      plate@df |>
      dplyr::mutate(SAMPLE_LOCATION = paste0(tray, ":", .data$SAMPLE_LOCATION))

    # group/type/vial(s)

    # Group:
    ## double blanks
    ## IS blanks
    ## suitability
    ## blank
    ## standards
    # CS tec rep1
    # blank if blank after high conc
    # CS tec rep2
    # blank if blank after high conc
    # CS tec rep3
    # blank if blank after high conc
    ## if has QCs and samples
    # QC set 1
    # samples set 1
    # QC set 2
    # samples set 2
    # QC set 3
    # samples set 3
    # QC set 4
    # samples set 4
    # ...
    ## if has samples but no QCs Xn
    ## if has QCs but no samples Xn
    # blank if blank at the end

    core_template <- list(
      "DB" = list(type = "DoubleBlank", nrep = rep_DB),
      "ISBlank" = list(type = "ISBlank", nrep = rep_ISblank),
      "Blank" = list(type = "Blank", nrep = rep_blank),
      "CS" = list(type = "Standard", nrep = repeat_std),
      "QC" = list(type = "QC", nrep = repeat_qc),
      "Analyte" = list(type = "Analyte", nrep = repeat_analyte),
      "DQC" = list(type = "DQC", nrep = repeat_dqc)
    )

    df <- plate[FALSE, ] # empty df, same dims

    groups <- unique(plate$a_group)
    results <- lapply(groups, function(grp) {
      lapply(core_template, function(args) {
        do.call(
          .get_item,
          c(args, list("group" = grp), list("platedf" = plate))
        )
      })
    })
    names(results) <- groups

    results <- lapply(results, .merge_qc_analyte)

    results <- .clean_list(results)

    if (blank_after_top_conc) {
      results <- lapply(results, .add_blank_after_high_conc)
    }

    if (blank_at_end) {
      results <- lapply(results, .add_blank_at_end)
    }

    results <- lapply(results, function(x) {
      x <- do.call(rbind, x)
    })

    results <- do.call(rbind, results)

    results$injec_rep <- NULL

    # suitability
    if (rep_suitability > 0) {
      suitability_df <- .get_suitability(plate, rep_suitability)
      if (is.null(suitability_df)) {
        stop(
          "Suitability is not present in the plate. Must have valid replicate"
        )
      }
      results <- rbind(suitability_df, results)
    } else if (rep_suitability == 0 || is.null(rep_suitability)) {
      if ("Suitability" %in% plate$TYPE) {
        stop(
          "Suitability is already present in the plate. Must have valid replicate"
        )
      }
    }

    # explore mode
    if (n_explore > 0) {
      xplore_df <- .get_xplore(plate, n_explore)
      results <- rbind(xplore_df, results)
    }

    if (!is.null(conc_df)) {
      # add conc_df to plate
      conc_df <- t(conc_df)
      cmpd_vec <- conc_df[1, ]
      cmpd_names <- paste0("COMPOUND_", LETTERS[seq_along(cmpd_vec)])
      conc_vec <- conc_df[2, ]
      conc_names <- paste0("CONC_", LETTERS[seq_along(conc_vec)])
      conc_df = data.frame(matrix(nrow = 1, ncol = length(cmpd_vec) * 2))
      colnames(conc_df) <- c(cmpd_names, conc_names)
      conc_df[1:length(cmpd_vec)] <- cmpd_vec
      conc_df[(length(cmpd_vec) + 1):(length(cmpd_vec) * 2)] <- conc_vec

      # min_conc <- min(as.numeric(df$conc))
      df <- results |>
        dplyr::bind_cols(conc_df) |> # bind conc_df
        dplyr::mutate(dplyr::across(starts_with("CONC_"), \(x) {
          (as.numeric(x) * as.numeric(.data$conc))
        })) # multiply conc_df with conc and divide by min conc
    } else {
      df <- dplyr::mutate(results, CONC_A = .data$conc)
    }

    # create filename
    ## Date
    df <- df |>
      dplyr::mutate(
        Index = dplyr::row_number(),
        FILE_NAME = paste0(prefix, "_", .data$value, "_", suffix),
        INJ_VOL = injec_vol,
        # CONC_A = conc,
        FILE_TEXT = descr,
        INLET_METHOD = method
      ) |> 
      make_sequence_names()

    x <- .injecList(df, current_plate_id)
    print(x)
  }
)


#' Create Injection Sequence from MultiPlate (Multiple Plates)
#' @rdname build_injec_seq
#' @keywords internal
#' @export
#' @returns InjecListObj object
setMethod(
  "build_injec_seq",
  "MultiPlate",
  function(
    plate,
    method,
    rep_DB = 2,
    rep_ISblank = 1,
    rep_suitability = 1,
    rep_blank = 2,
    repeat_std = 1,
    repeat_qc = 1,
    repeat_analyte = 1,
    repeat_dqc = 1,
    n_explore = 0,
    blank_after_top_conc = TRUE,
    blank_at_end = TRUE,
    blank_every_n = NULL,
    injec_vol,
    descr = "",
    prefix = Sys.Date(),
    suffix = "1",
    tray,
    conc_df = NULL,
    grouped = TRUE
  ) {
    checkmate::assertCharacter(tray, min.len = 1, max.len = 12, unique = TRUE)

    plate <- plate@plates
    if (length(plate) == 1) {
      plate <- plate[[1]]
    } else {
      ## assert length of tray is equal to number of plates
      if (length(tray) != length(plate)) {
        stop("Number of tray slots must be equal to number of plates")
      }
      ## assert all plates are registered
      if (!all(sapply(plate, .is_registered))) {
        stop("All plates are not registered. Please register the plates first.")
      }

      m <- lapply(plate, function(x) x@plate)
      m <- do.call(rbind, m)

      df <- lapply(1:length(plate), function(i) {
        x <- plate[[i]]@df
        x$tray <- tray[i]
        x
      })

      df <- dplyr::bind_rows(df)

      plate_id <- sapply(plate, function(x) x@plate_id)

      descr <- sapply(plate, function(x) x@descr) |> paste0(collapse = ", ")

      empty_rows <- sapply(plate, function(x) x@empty_rows)

      last_modified <- sapply(plate, function(x) x@last_modified)

      plate <- new(
        "RegisteredPlate",
        plate = m,
        df = df,
        plate_id = plate_id,
        descr = descr,
        # Note: all from here is dummy to avoid class checking error, this plate will not be returned
        empty_rows = empty_rows[, 1],
        last_modified = as.POSIXct(Sys.Date()),
        filling_scheme = plate[[1]]@filling_scheme,
        last_filled = plate[[1]]@last_filled
      )
      plate
    }

    build_injec_seq(
      plate,
      method = method,
      repeat_std = repeat_std,
      repeat_qc = repeat_qc,
      repeat_analyte = repeat_analyte,
      blank_after_top_conc = blank_after_top_conc,
      blank_at_end = blank_at_end,
      rep_suitability = rep_suitability,
      blank_every_n = blank_every_n,
      injec_vol = injec_vol,
      descr = descr,
      prefix = prefix,
      suffix = suffix,
      tray = tray,
      n_explore = n_explore,
      conc_df = conc_df
    )
  }
)


# create validity method for PlateObj
setValidity("PlateObj", function(object) {
  if (!is.matrix(object@plate)) {
    stop("plate must be a matrix")
  }
  if (!is.data.frame(object@df)) {
    stop("df must be a data.frame")
  }

  col_type <- c(
    "integer",
    "integer",
    "character",
    "character",
    "character",
    "character",
    "numeric",
    "character",
    "character",
    "integer",
    "integer"
  )
  cols <- c(
    "row",
    "col",
    "value",
    "SAMPLE_LOCATION",
    "samples",
    "conc",
    "TYPE",
    "std_rep",
    "e_rep",
    'dil'
  )
  checkmate::assertNames(names(object@df), must.include = cols)

  if (!is.list(object@filling_scheme)) {
    stop("filling_scheme must be a list")
  }

  if (!(object@filling_scheme$scheme %in% c("h", "v", "hv"))) {
    stop("filling_scheme must be either 'h', 'v' or 'hv'")
  }

  if (!is.character(object@empty_rows)) {
    stop("empty_rows must be a character")
  }
  if (!is.character(object@last_filled)) {
    stop("last_filled must be a character")
  }

  if (!is.character(object@plate_id)) {
    stop("plate_id must be a character")
  }
  if (!is.character(object@descr)) {
    stop("descr must be a character")
  }

  TRUE
})


setMethod(
  "show",
  signature = "PlateObj",
  definition = function(object) {
    print(object)
  }
)

#' Register a plate
#' This will save the plate to the database
#' @param plate PlateObj object
#' @export
#' @keywords internal
#' @returns Registered PlateObj object
setMethod("register_plate", "PlateObj", function(plate) {
  .register_plate_logic(plate)
})

#' Register a multiple plates at once
#' @param plate MultiPlate object
#' @export
#' @keywords internal
#' @return a list of RegisteredPlate objects
setMethod("register_plate", "MultiPlate", function(plate) {
  lapply(plate@plates, .register_plate_logic)
})

setMethod("get_plate_a_groups", "PlateObj", function(plate) {
  unique(plate@df$a_group)
})
setMethod("get_plate_a_groups", "MultiPlate", function(plate) {
  unique(unlist(lapply(plate@plates, get_plate_a_groups)))
})
