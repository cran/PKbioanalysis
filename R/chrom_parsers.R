dir_or_files_to_files <- function(dir, file_pattern) {
  if (
    checkmate::checkDirectory(dir) &
      length(dir) == 1 &
      !all(grepl(dir, pattern = file_pattern))
  ) {
    checkmate::assertDirectoryExists(dir)
    x <- list.files(
      dir,
      full.names = T,
      pattern = file_pattern,
      all.files = TRUE
    )
  } else if (checkmate::checkCharacter(dir)) {
    lapply(dir, checkmate::assertDirectoryExists)
    x <- dir
  } else if (checkmate::checkFile(dir)) {
    lapply(dir, checkmate::assertFileExists)
    x <- dir
  } else {
    stop("Input must be a directory or a vector of files")
  }
  x
}

#' Read raw files
#' @param dir directory containing raw files
#' @importFrom reticulate import
#' @import dplyr
#' @import tidyr
#' @import janitor
#' @import cli
#' @importFrom tidyr pivot_longer
#' @noRd
.parsewatersraw <- function(dir) {
  checkmate::assertDirectory(dir)

  cli::cli_h3("Reading Waters Raw Files")

  py <- reticulate::import_from_path(
    "src",
    path = system.file("pysrc", package = "PKbioanalysis")
  )
  # py <<- reticulate::import_from_path("inst", delay_load = T)
  x <- dir_or_files_to_files(dir, "raw$")

  if (length(x) < 1) {
    cli::cli_alert_danger("No raw files found in directory {dir}")
    stop("No raw files found in directory")
  }
  cli::cli_alert_info(paste0("Found ", length(x), " raw files"))

  rawreadr <- function(file) {
    f <- py$masslynx_parser$read_waters(file) # pycall
    # 1: data, 2: cmpds, 3: run meta, 4: q1 transitions, 5: q3 transitions

    # 1
    f[[1]] <- f[[1]] |> cbind(as.data.frame(f[[3]]))
    coln <- f[[1]] |> colnames()

    # 2 and 4
    q1_transitions <- f[[4]] # coln[seq(1, length(f[[2]]$compounds))] # from FUNINF
    q3_transitions <- f[[5]]

    # sanity checks for waters raw files
    stopifnot(length(q1_transitions) == length(f[[2]]$transition))
    stopifnot(length(q3_transitions) == length(q1_transitions))

    transition_id <- paste0("T", seq_along(q1_transitions))

    transition_df <- data.frame(q1 = q1_transitions, q3 = q3_transitions) |>
      dplyr::arrange(.data$q1, .data$q3) |>
      dplyr::mutate(transition_id = transition_id) |>
      dplyr::mutate(transition_label = paste0(round(.data$q1, 2), ">", round(.data$q3, 2)))

    colnames(f[[1]]) <- c(
      transition_id,
      coln[(length(transition_id) + 1):length(coln)]
    )

    cli::cli_status_update(
      id = sb,
      "{cli::symbol$arrow_right} loading {file}"
    )
    # dat <- janitor::clean_names(f[[1]], case = "none") |> colnames()

    # pivoting to long format => (RT vs intensity vs transition_id)
    dat <- f[[1]] |>
      tidyr::pivot_longer(
        cols = !!transition_id, # vector of columns to sepecify the transitions' intensity
        names_to = "transition",
        values_to = "Intensity"
      )

    list(dat, transition_df)
  }

  sb <- cli::cli_status("{cli::symbol$arrow_right} Loading")
  res <- lapply(x, \(x) rawreadr(x))

  # Assert sorted by date
  df <- lapply(res, `[[`, 1) |> dplyr::bind_rows()
  # dplyr::mutate(transition_id = cur_group_id(), .by = transition) |>
  # dplyr::mutate(sample_id = cur_group_id(), .by = sample) |>
  # dplyr::mutate(transition = gsub("^\\d+_", "", transition))

  cli::cli_alert_success(paste0("Loaded ", length(x), " chromatograms"))

  sample_transitions <- lapply(res, \(x) x[[2]])
  # assert all dataframes are identical
  # tabulate which is true and which is false, separate by delimiter

  failed <- sample_transitions |>
    sapply(\(x) identical(x, sample_transitions[[1]]))
  if (any(!failed)) {
    failure_indices <- which(!failed)
    failure_table <- data.frame(
      Index = basename(x[failure_indices]),
      Status = "Failure"
    )
    print(failure_table)
    stopifnot("Please remove inconsistent methods") # all transitions must be same across all samples
  }

  sample_transitions <- sample_transitions |>
    dplyr::bind_rows() |>
    dplyr::distinct()
  ## Must assert all transitions are same across all samples

  #need only RT and T1 - Tn. Others will be sub list called meta
  df_list <- split(df, df$sample) |>
    lapply(\(x) {
      peakdf <- x[, c("RT", "transition", "Intensity")] |>
        tidyr::pivot_wider(names_from = "transition", values_from = "Intensity")
      metadf <- x[,
        !colnames(x) %in% c("RT", "transition", "transition_id", "Intensity")
      ] |>
        dplyr::distinct() |>
        dplyr::rename(filename = "sample") |>
        dplyr::mutate(filename = tools::file_path_sans_ext(.data$filename))

      list(
        sample_chrom = peakdf,
        sample_metadata = metadf,
        sample_transitions = sample_transitions
      )
    })

  stopifnot(length(df_list) == length(x))

  df_list
}


.parsemzml <- function(dir) {
  cli::cli_h3("Reading mzML Files")
  files <- list.files(dir, full.names = T, pattern = "mzML$", all.files = T)
  stopifnot("No mzML files found in directory" = files >= 1)
  cli::cli_alert_info(paste0("Found ", length(files), " mzML files"))

  res <- lapply(files, \(x) {
    x <- RaMS::grabMSdata(x, grab_what = c("chroms", "metadata"))

    y <- x$chroms |>
      dplyr::filter(.data$chrom_type != "TIC") |>
      dplyr::mutate(
        q1 = gsub(
          .data$chrom_type,
          pattern = ".*Q1=(.*)\\sQ3=(.*)\\sfunction.*",
          replacement = "\\1"
        ),
        q3 = gsub(
          .data$chrom_type,
          pattern = ".*Q1=(.*)\\sQ3=(.*)\\sfunction.*",
          replacement = "\\2"
        )
      ) |>
      dplyr::mutate(q1 = as.numeric(.data$q1), q3 = as.numeric(.data$q3)) |>
      dplyr::select("chrom_index", "rt", "int", "filename", "q1", "q3") |>
      dplyr::rename(transition_id = "chrom_index", RT = "rt", Intensity = "int") |>
      dplyr::mutate(transition_id = paste0("T", .data$transition_id))

    dat <- y |>
      dplyr::select("RT", "transition_id", "Intensity") |>
      tidyr::pivot_wider(
        names_from = "transition_id",
        values_from = "Intensity"
      )

    list(
      sample_chrom = dat,
      sample_metadata = data.frame(
        filename = tools::file_path_sans_ext(unique(y$filename)),
        vendor = "mzML",
        date = x$metadata$timestamp
      ),
      sample_transitions = y |>
        dplyr::select("transition_id", "q1", "q3") |>
        dplyr::distinct()
    )
  })

  list(runs = res, exp_transitions = NA, exp_compounds = NA)
}

.sort_chromatograms <- function(chroms_list) {
  # ensure there is a numeric id for each sample
  names(chroms_list) <- 1:length(chroms_list)
  # add sample_id to metadata
  ## sort the list by date
  chroms_list <- chroms_list[order(sapply(chroms_list, function(x) {
    x$sample_metadata$date
  }))]
  # print(lapply(chrom_res$runs, \(x) x$sample_metadata$date))
  # print(lapply(chrom_res$runs, \(x) x$sample_metadata$filename))
  names(chroms_list) <- 1:length(chroms_list)
  chroms_list
}

# TODO solidify a schema for this file
#'@param chroms_list list of chromatograms with sample metadata slot
#' Note that this depends on chromatograms existance
#' @noRd
.construct_file_meta <- function(chroms_list) {
  index <- 1:length(chroms_list)
  new_chrom <- lapply(index, \(x) {
    sample_metadata <- chroms_list[[x]]$sample_metadata |>
      dplyr::mutate(sample_id = as.character(x))
    sample_metadata
  })
  new_chrom <- do.call(rbind, new_chrom) |>
    as.data.frame() |>
    dplyr::mutate(type = as.character(NA)) |>
    dplyr::mutate(filename = tools::file_path_sans_ext(.data$filename)) |> # remove any file extensions
    dplyr::mutate(std_rep = as.character(NA)) |>
    dplyr::mutate(sample_location = as.character(NA)) |>
    dplyr::mutate(inj_vol = as.numeric(NA)) |>
    dplyr::mutate(dilution_factor = as.numeric(NA)) |>
    dplyr::mutate(sample_id = as.character(.data$sample_id)) |>
    dplyr::mutate(subject_id = as.character(NA)) |>
    dplyr::mutate(sampling_time = as.numeric(NA)) |>
    dplyr::mutate(invitro_conc = as.numeric(NA)) |>
    dplyr::mutate(factor = as.character(NA)) |>
    dplyr::mutate(dose = as.numeric(NA))

  new_chrom
}

.check_transitions <- function(chrom_list) {
  ##### check all individual sample_transitions are same#####
  x <- chrom_list |> lapply(\(x) x$sample_transitions)

  stopifnot(class(x[[1]]) == "data.frame")

  # check all sample_transitions df are same
  if (!all(sapply(x, identical, x = x[[1]]))) {
    stop(
      "Transitions are not same across samples. Please ensure all chromatograms have same transitions"
    )
  }
}
# This function will not change the sample_transitions at all
#' @param transition_df data.frame from chromatograms with q1, q3, transition_id
#' @param transitiondb extracted transitions data.frame from database
#' @noRd
.construct_experiment_transitions <- function(
  transition_df,
  transitiondb,
  diff_tol = 0.5
) {
  checkmate::assertDataFrame(transition_df)
  checkmate::assertNames(
    colnames(transition_df),
    must.include = c("q1", "q3", "transition_id")
  )
  checkmate::assertDataFrame(transitiondb, null.ok = FALSE)
  checkmate::assertNames(
    colnames(transitiondb),
    must.include = c("q1", "q3", "transition_id", "method_id")
  )

  transition_df <- transition_df |>
    dplyr::mutate(across(c("q1", "q3"), as.numeric)) |>
    dplyr::arrange(round(.data$q1, 1), round(.data$q3, 1))
  transitiondb <- transitiondb |>
    dplyr::mutate(across(c("q1", "q3"), as.numeric)) |>
    dplyr::arrange(round(.data$q1, 1), round(.data$q3, 1))

  # check chrom and db consistency
  if (
    any(abs(transition_df$q1 - transitiondb$q1) > diff_tol) ||
      any(abs(transition_df$q3 - transitiondb$q3) > diff_tol) ||
      length(transition_df$q1) != length(transitiondb$q1) ||
      length(transition_df$q3) != length(transitiondb$q3)
  ) {
    print(data.frame(
      db_q1 = round(transitiondb$q1, 2),
      db_q3 = round(transitiondb$q3, 2),
      chrom_q1 = round(transition_df$q1, 2),
      chrom_q3 = round(transition_df$q3, 2),
      mismatch = abs(transition_df$q1 - transitiondb$q1) > diff_tol |
        abs(transition_df$q3 - transitiondb$q3) > diff_tol
    ))
    stop(
      "Transition values do not match between chromatography and method database."
    )
  }

  # any sample_transitions  df
  exp_transitions <- transitiondb |>
    dplyr::select("transition_id", "transition_label", "q1", "q3", "method_id")

  exp_transitions
}


.construct_experiment_peaktab <- function(chrom_res) {
  # update adding add compounds columns to files_metadata
  spiked_vec <- chrom_res$exp_compounds |> dplyr::pull("compound") |> unique()
  spiked_vec <- paste0("spiked_", spiked_vec)
  chrom_res$metadata[, spiked_vec] <- as.numeric(NA)

  compounds_ids <- chrom_res$exp_compounds |>
    dplyr::pull("compound_id") |>
    unique()
  samples_ids <- chrom_res$metadata |> dplyr::pull("sample_id") |> unique()

  chrom_res$exp_peaktab <- expand.grid(
    sample_id = samples_ids,
    compound_id = compounds_ids
  ) |>
    dplyr::mutate(
      area = as.numeric(NA),
      observed_peak_start = as.numeric(NA),
      observed_peak_end = as.numeric(NA),
      observed_peak_height = as.numeric(NA),
      observed_rt = as.numeric(NA),
      manual = FALSE
    )
  # join filename and compound
  chrom_res$exp_peaktab <- chrom_res$exp_peaktab |>
    dplyr::left_join(
      chrom_res$metadata |> dplyr::select("sample_id", "filename"),
      by = c("sample_id" = "sample_id")
    ) |>
    dplyr::mutate(filename = tools::file_path_sans_ext(.data$filename)) |> # remove any file extensions
    dplyr::select(
      "sample_id",
      "filename",
      "compound_id",
      "area",
      "observed_peak_start",
      "observed_peak_end",
      "observed_peak_height",
      "observed_rt",
      "manual"
    )

  chrom_res
}

#' res is as list
#' peaks is data.frame
#' @noRd
.construct_experiment_peaktab.deprecated <- function(res, peaks) {
  # check the desired columns are present
  stopifnot(all(
    c("filename", "peak_start", "peak_end", "compound", "q1", "q3") %in%
      colnames(peaks)
  ))

  # remove any file extensions from the filename
  peaks$filename <- tools::file_path_sans_ext(peaks$filename)

  # FIXME it gets what it can gets
  stopifnot(any(sapply(res$metadata$filename, \(x) x %in% peaks$filename)))
  # stopifnot(length(unique(peaks$filename))  >= length(res$runs))
  # stopifnot(length(unique(peaks$filename)) == length(res$metadata$filename)) # all samples must be unique

  # NOTE allowed to have more imported peaks than actuall chrom but not less

  # TODO grab transition ID and label if available

  peaks <- peaks |>
    dplyr::left_join(
      res$metadata |> dplyr::select("filename", "sample_id"),
      by = c("filename" = "filename")
    )

  # assert all chromatograms has a sample_id
  names(res$runs$files) <- res$metadata$sample_id
  stopifnot(
    length(unique(names(res$runs$files))) == length(res$metadata$sample_id)
  ) # all samples must be unique

  # within all samples (groups), assert equal number of compounds
  check <- peaks |>
    dplyr::group_by(.data$filename) |>
    dplyr::summarise(
      n_cmpd = n_distinct(.data$compound),
      n_q1 = n_distinct(.data$q1),
      n_q3 = n_distinct(.data$q3)
    ) |>
    select(-"filename") |>
    dplyr::distinct()

  if (nrow(check) != 1) {
    stop("Number of compounds, q1, q3 are not same across samples")
  }

  peaks <- peaks |>
    dplyr::rename(
      "observed_peak_start" = "peak_start",
      "observed_peak_end" = "peak_end"
    ) |>
    dplyr::mutate(
      observed_peak_start = as.numeric(.data$observed_peak_start),
      observed_peak_end = as.numeric(.data$observed_peak_end)
    ) |>
    dplyr::mutate(
      observed_rt = (.data$observed_peak_start + .data$observed_peak_end) / 2
    ) |>
    dplyr::mutate(observed_peak_height = as.numeric(NA)) |>
    dplyr::mutate(area = as.numeric(NA)) |>
    dplyr::mutate(dum_q1 = round(.data$q1, 0), dum_q3 = round(.data$q3, 0)) |>
    dplyr::select(-"q1", -"q3") |> # remove the manual q1 and q3, keeping the ones from chromatograms
    dplyr::left_join(
      res$exp_transitions |>
        dplyr::mutate(dum_q1 = round(.data$q1, 0), dum_q3 = round(.data$q3, 0)),
      by = c("dum_q1", "dum_q3")
    ) |>
    dplyr::select(-"dum_q1", -"dum_q3") |>
    dplyr::mutate(manual = FALSE) # selection after compounds

  # assert there is no NA in transition_id, q1, q3 or inlet_method
  stopifnot(all(!is.na(peaks$transition_id)))
  stopifnot(all(!is.na(peaks$q1)))
  stopifnot(all(!is.na(peaks$q3)))
  stopifnot(all(!is.na(peaks$inlet_method)))

  # "filename", "transition_id", "area", "compound",
  #         "observed_peak_start", "observed_peak_end", "IS"
  res$exp_peaktab <- peaks

  res
}

# This function will losely join q1 and q3 from cmpd table to transition table
# It is a must to have all transitions both ways.
# Margin of 0.5 is allowed for q1 and q3
#
# res$metadata must exist and it will be updated accordingly
.construct_experiment_compounds <- function(method_id, transition_df) {
  compoundsdf <- .get_method_cmpds(method_id)

  # compare both chrom and db transitions, stop if any mismatch by margin of 0.5
  tryCatch(
    {
      compare1 <- transition_df |>
        dplyr::select("transition_id", "q1", "q3") |>
        distinct()
      compare2 <- compoundsdf |>
        dplyr::select("transition_id", "q1", "q3") |>
        distinct()
      # compare q1 with margin of 0.5
      all.equal(compare1$q1, compare2$q1, tolerance = 0.5) |> stopifnot()
      all.equal(compare1$q3, compare2$q3, tolerance = 0.5) |> stopifnot()
      all.equal(compare1$transition_id, compare2$transition_id) |> stopifnot()
    },
    error = function(e) {
      # elaborative error messages. Print both
      print(
        transition_df |>
          dplyr::select("transition_id", "q1", "q3") |>
          distinct()
      )
      print(
        compoundsdf |> dplyr::select("transition_id", "q1", "q3") |> distinct()
      )
      print(e)
      stop("Transitions do not match")
    }
  )

  # ensure all compounds have a transition id
  stopifnot(all(!is.na(compoundsdf$transition_id)))

  # compound names are unique in chromres, not in db
  compoundsdf <- compoundsdf |>
    dplyr::mutate(compound = make.unique(as.character(.data$compound), sep = "_"))

  exp_compounds <- compoundsdf |>
    select(
      "compound_id",
      "compound",
      "transition_id",
      "expected_peak_start",
      "expected_peak_end",
      "expected_rt",
      "IS_id"
    )

  exp_compounds
}

#' Save transitions to db as unique transition id for the entire experiment
#' @noRd
.write_transitions <- function(chrom_res) {
  db <- .connect_to_db()

  # get id and labels or create id based on row number
  ## if combination not in db, add to db and retived the new id
  for (trans_row in 1:nrow(new_chrom_res$exp_transitions)) {
    q1 <- new_chrom_res$exp_transitions[trans_row, "q1"]
    q3 <- new_chrom_res$exp_transitions[trans_row, "q3"]
    inlet_method <- new_chrom_res$exp_transitions[trans_row, "inlet_method"]
    max_id <- tbl(db, "methodstab") |>
      dplyr::summarise(id = max(.data$transition_id), na.rm = T) |>
      as.data.frame() |>
      pull("id")
    max_id <- ifelse(is.na(max_id), 0, max_id)

    query <- tbl(db, "methodstab") |>
      dplyr::filter(q1 == q1, q3 == q3, inlet_method == inlet_method) |>
      as.data.frame()

    old_transition_id <- new_chrom_res$exp_transitions[
      trans_row,
      "transition_id"
    ]

    if (nrow(query) == 0) {
      ## add to db and retrieve the id
      dbWriteTable(
        db,
        "methodstab",
        data.frame(
          transition_id = max_id + 1,
          q1 = q1,
          q3 = q3,
          method_gradient = inlet_method,
          transition_label = NA
        ),
        append = T,
        row.names = F
      )

      new_transition_id <- max_id + 1
      transition_label <- NA
    } else if (nrow(query) > 1) {
      stop("Multiple transitions found in database")
    } else if (nrow(query) == 1) {
      # update the transition id and labels

      new_transition_id <- query |> dplyr::pull("transition_id") |> as.character()
      transition_label <- query |>
        dplyr::pull("transition_label") |>
        as.character()
    }

    new_chrom_res$exp_transitions$transition_id <- ifelse(
      new_chrom_res$exp_transitions$transition_id == old_transition_id,
      new_transition_id,
      new_chrom_res$exp_transitions$transition_id
    )

    # change chromatogram transitions ids

    new_chrom_res$runs <- lapply(new_chrom_res$runs, \(x) {
      colnames(x$sample_chrom) <- ifelse(
        colnames(x$sample_chrom) == old_transition_id,
        paste0("T", new_transition_id),
        colnames(x$sample_chrom)
      )
      x$sample_transitions <- NULL
      x$sample_compounds <- NULL
      x
    })

    # retrive any transition label
    new_chrom_res$exp_transitions$transition_label <- ifelse(
      new_chrom_res$exp_transitions$transition_id == new_transition_id,
      transition_label,
      new_chrom_res$exp_transitions$transition_label
    )
  } # end for loop
  DBI::dbDisconnect(db)
}


.construct_pk_metadata <- function(chrom_res) {
  # get sample_id and filename df
  cmpd_vec <- chrom_res$exp_compounds |> dplyr::pull(.data$compound_id) |> unique()
  # cmpd_vec <- paste0("spiked_", cmpd_vec)

  # pkmetadf <- chrom_res$metadata |>
  #   dplyr::select("sample_id", "filename") |>
  #   as.data.frame() |>
  #   dplyr::mutate(
  #     time = as.numeric(NA),
  #     dose = as.numeric(NA),
  #     sample = as.character(NA),
  #     factor = as.character(NA))
  # pkmetadf[, cmpd_vec] <- NA
  pk_metadata <- vector("list", length(cmpd_vec))
  names(pk_metadata) <- cmpd_vec

  #initalize empty
  chrom_res$pk_metadata <- pk_metadata
  chrom_res
}
