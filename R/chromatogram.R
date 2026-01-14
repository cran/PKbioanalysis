#' @include class.R generics.R
NULL

check_chromatograms_consistency <- function(object) {
  if (length(object@runs) == 0) {
    stop("No chromatogram data found.")
  }

  if (
    nrow(object@peaks) != length(object@runs$files) * nrow(object@compounds)
  ) {
    # here compounds df are important regardless of uniquness. transitions are latent
    stop("Number of peaks does not match the number of samples and compounds.")
  }

  # all chrom must have names
  if (is.null(names(object@runs$files))) {
    stop("Sample names are missing.")
  }

  checkmate::assertDataFrame(object@metadata, nrows = length(object@runs$files))

  # assert both vectors are identical
  if (
    !identical(unique(names(object@runs$files)), unique(object@peaks$sample_id))
  ) {
    stop("Sample names are not found in the peaktab.")
  }
}

check_all <- function(object) {
  if (length(object@transitions) == 0) {
    stop("No transitions data found.")
  }

  if (
    nrow(object@peaks) !=
      (nrow(object@compounds) * length(unique(object@peaks$sample_id)))
  ) {
    stop("Number of peaks does not match the number of samples and compounds.")
  }

  spiked_vec <- paste0("spiked_", unique(object@compounds$compound))
  checkmate::assertNames(
    names(object@metadata),
    must.include = c(
      "sample_id",
      "filename",
      "date",
      "type",
      "sample_id",
      "std_rep",
      "sample_location",
      "inj_vol",
      "dilution_factor",
      "sample_id",
      "subject_id",
      "invitro_conc",
      "sampling_time",
      "factor",
      "dose",
      spiked_vec
    )
  )

  # peaktab columns
  checkmate::assertNames(
    names(object@peaks),
    identical.to = c(
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
  )

  # checkmate::assertDataFrame(object@peaks,
  #     types = c("numeric", "character", "numeric", "numeric",
  #         "character", "numeric", "numeric", "numeric", "numeric",
  #         "numeric", "character", "logical", "character", "character", "character", "character")
  # )

  # transitions columns (to match methodstab)
  ## method_id is only in the db, no need to check or have it here
  ## transition id is a unique string identifier ("T1" "T2") for the transition INSIDE same method_id. But could be repeated.
  checkmate::assertNames(
    names(object@transitions),
    identical.to = c(
      "transition_id",
      "transition_label",
      "q1",
      "q3",
      "method_id"
    )
  )
  # checkmate::assertDataFrame(object@transitions,
  #     types = c("character", "character", "numeric", "numeric", "character")
  # )

  # compounds columns
  ## compound_id is a unique string identifier ("C1" "C2") for the compound INSIDE same method_id. But could be repeated.
  checkmate::assertNames(
    names(object@compounds),
    permutation.of = c(
      "compound_id",
      "compound",
      "transition_id",
      "expected_peak_start",
      "expected_peak_end",
      "expected_rt",
      "IS_id"
    )
  )
  # checkmate::assertDataFrame(object@compounds,
  #     types = c("character", "character", "character",
  #         "numeric", "numeric", "numeric")
  # )

  ## check linearity
  # checkmate::assertList(object@linearity, len = 2, types = c(
  #     "data.frame", "list")) # linearitytab and list

  # # check linearitytab
  # checkmate::assertNames(names(object@linearity[[1]]),
  #     identical.to  = c("filename", "compound_id", "transition_id",
  #         "area", "conc", "weight", "model", "intercept", "avg_rep", "plot")
  # )

  # check metadata columns names constistency
  # response_vec <- paste0("response_", unique(object@compounds$compound))

  # checkmate::assertNames(names(object@pk_metadata),
  #     must.include =  c("sample_id", "filename",
  #     "sample", "time", "dose", "factor") # "response_vec"
  # )

  # stopifnot(nrow(object@compounds) == length(object@pk_metadata))

  ## Assert sorting by date
}

setValidity("ChromResBase", function(object) {
  check_all(object)
  TRUE
})

setValidity("ChromRes", function(object) {
  check_chromatograms_consistency(object)
  check_all(object)
  TRUE
})


#' @title Read Chromatogram Files
#' @description This function reads chromatogram files from a directory and returns a data frame with the chromatogram data.
#' @param dir directory for chromatraograms
#' @param format format of the chromatogram files. Options are "waters_raw" and "mzML".
#' @param method LC-MS/MS method ID saved available in the database.
#' @export
#' @examples
#' \dontrun{
#' path <- system.file("extdata", "waters_raw_ex", package="PKbioanalysis")
#' main <- read_chrom(path, method = 1)
#' }
read_chrom <- function(dir, format = "waters_raw", method) {
  checkmate::assertChoice(format, choices = c("waters_raw", "mzML"))
  checkmate::assertNumber(method, lower = 1, null.ok = FALSE)

  transitionsdb <- .get_method_transitions(method)
  if (is.null(transitionsdb) | nrow(transitionsdb) == 0) {
    stop(paste0("No transitions found for method_id ", method, "."))
  }

  if (format == "waters_raw") {
    chroms_list <- .parsewatersraw(dir)
  } else if (format == "mzML") {
    chroms_list <- .parsemzml(dir)
  }

  chroms_list <- .sort_chromatograms(chroms_list)

  metadata_df <- .construct_file_meta(chroms_list) # make unique ID

  .check_transitions(chroms_list) # ensure list and all identical
  # pluck only one sample
  transitions_df <- .construct_experiment_transitions(
    chroms_list[[1]]$sample_transitions,
    transitionsdb
  )

  compounds_df <- .construct_experiment_compounds(method, transitions_df)

  res <- list(
    runs = list(files = chroms_list),
    metadata = metadata_df,
    exp_transitions = transitions_df,
    exp_compounds = compounds_df
  )

  res <- .construct_experiment_peaktab(res) # Note res$runs must have files_metadata

  # get the compounds from the peaktab
  res <- .construct_pk_metadata(res)

  res <- new(
    "ChromRes",
    runs = res$runs,
    metadata = res$metadata,
    peaks = res$exp_peaktab,
    transitions = res$exp_transitions, # transitions global
    compounds = res$exp_compounds, # compounds global
    pk_metadata = res$pk_metadata,
    vendor = format
  ) #FIXME: vendor is not always the format as mzML can be any vendor.

  # TODO automatically select any suitability vial, start and end)
  max_con <- rle(get_vials(res))
  max_con <- max_con$values[max_con$lengths |> which.max()]

  validObject(res)
  res
}


#' @title Plot Chromatogram per Sample for Selected transitions
#' @description This function plots chromatograms for selected transitions per sample.
#' @param chrom_res ChromRes object
#' @param ncol Number of columns for facet_wrap. If 0, the chromatograms are overlayed in a single plot.
#' @param transitions_ids Vector of transition IDs to plot. If NULL, all transitions are plotted.
#' @param sample_id Sample ID to plot.
#' @param integrated Boolean to show integrated area overlayed
#' @param show_RT Boolean to show RT values
#' @param smoothed Boolean to show smoothed chromatogram
#' @importFrom ggplot2 ggplot geom_line theme_minimal theme labs scale_y_continuous facet_wrap
#' @importFrom dplyr mutate left_join select rename filter
#' @importFrom tidyr pivot_longer
#' @import checkmate
#'
#' @export
#' @examples
#' \dontrun{
#' path <- system.file("extdata", "waters_raw_ex", package="PKbioanalysis")
#' main <- read_chrom(path, method = 1)
#' plot_chrom(main, ncol = 2, transitions_ids = c(18,19,20), sample_id = 3)
#' plot_chrom(main, ncol = 3, transitions_ids = c(18,19,20), sample_id = 3)
#' plot_chrom(main, ncol = 1, transitions_ids = c(18,19,20), sample_id = 3)
#' plot_chrom(main, ncol = NULL, transitions_ids = c(18,19,20), sample_id = 3)
#' }
plot_chrom <- function(
  chrom_res,
  ncol = 2,
  transitions_ids = NULL,
  sample_id,
  integrated = FALSE,
  show_RT = FALSE,
  smoothed = FALSE
) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumber(ncol, lower = 0, null.ok = F)
  checkmate::assertNumeric(transitions_ids, lower = 1, null.ok = T)
  checkmate::assertNumber(sample_id, lower = 1, null.ok = F)
  checkmate::assertLogical(integrated)
  checkmate::assertLogical(show_RT)
  checkmate::assertLogical(smoothed)

  chrom_res <- filter_chrom(
    chrom_res,
    transitions_ids = transitions_ids,
    samples_id = sample_id
  )

  dat <- chrom_res@runs$files
  vendor <- chrom_res@vendor
  peaktab <- chrom_res@peaks |>
    dplyr::mutate(sample_id = as.numeric(sample_id))

  transitions <- chrom_res@transitions |>
    mutate(transition_id = paste0("T", .data$transition_id))
  compounds <- chrom_res@compounds |>
    dplyr::mutate(transition_id = paste0("T", .data$transition_id))

  if (smoothed) {
    if (!is_smoothed(chrom_res)$smoothed[1]) {
      stop("No smoothed chromatogram found.")
    }
  }

  stopifnot(length(dat) == 1) # either repeated samples or no sample found

  # switch between smoothed and original intensity
  yvar <- ifelse(smoothed, "smoothed", "sample_chrom") # 1 is the original intensity, 3 is the smoothed intensity

  # get all needed chromatogrames in one place
  active_df <- dat[[1]][[yvar]] |>
    tidyr::pivot_longer(
      cols = -c("RT"),
      names_to = "transition_id",
      values_to = "Intensity"
    ) |>
    dplyr::mutate(
      sample_id = as.numeric(chrom_res@metadata$sample_id),
      sample = chrom_res@metadata$filename,
      date_captured = chrom_res@metadata$date
    )

  # join transitions and peaks to chromatograms
  ## chromatograms <=> file name
  ## peaks$transition_id <> transition_id

  active_df <- active_df |>
    left_join(
      select(transitions, "transition_id", "transition_label"), # get the label correctly for the transitions
      by = "transition_id"
    ) |>
    left_join(compounds, by = "transition_id") |>
    left_join(peaktab, by = c("sample_id", "compound_id")) # get the RT markers

  sample_name <- unique(active_df$filename)
  caption <- unique(active_df$date_captured)
  if (ncol > 0) {
    y <- ggplot(
      active_df,
      aes(x = .data$RT, y = .data$Intensity, color = .data$transition_label)
    ) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(
        subtitle = sample_name,
        x = "RT (Minutes)",
        y = "Intensity",
        caption = caption
      ) +
      scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
      facet_wrap(~transition_label, ncol = ncol, scales = "free_y")
  } else {
    y <- ggplot(
      active_df,
      aes(x = .data$RT, y = .data$Intensity, color = .data$transition_label)
    ) +
      geom_line() +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(
        subtitle = sample_name,
        x = "RT (Minutes)",
        y = "Intensity",
        caption = caption
      ) +
      scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
  }

  if (show_RT) {
    y <- y +
      ggplot2::geom_point(
        aes(x = .data$observed_rt, y = .data$observed_peak_height),
        color = "purple"
      ) +
      ggplot2::geom_point(aes(x = .data$observed_peak_start, y = 0), color = "blue") +
      ggplot2::geom_point(aes(x = .data$observed_peak_end, y = 0), color = "green") +
      ggplot2::geom_text(
        aes(
          x = .data$observed_rt,
          y = .data$observed_peak_height,
          label = paste0(
            "RT:",
            .data$observed_rt %>% round(2),
            " Area: ",
            round(.data$area, 3)
          )
        ),
        check_overlap = T,
        hjust = 0.5,
        vjust = 2
      )
  }
  if (integrated) {
    # add peak area and cmpd name
    cmpd_lab <- sapply(active_df$compound_id, \(x) {
      get_compound_name(chrom_res, x)
    })
    y = y +
      ggplot2::geom_area(
        mapping = aes(
          x = ifelse(
            .data$RT >= .data$observed_peak_start & .data$RT <= .data$observed_peak_end,
            .data$RT,
            NA
          )
        ),
        fill = "grey",
        alpha = 0.5
      ) +
      ggplot2::geom_text(
        mapping = aes(
          x = .data$observed_rt,
          y = .data$observed_peak_height,
          label = cmpd_lab
        ),
        check_overlap = T,
        hjust = -0.5,
        vjust = 0
      )
  }

  y
}

#'title Filter Chromatogram Peaks
#' @description This function filters chromatogram peaks based on transition ID and sample ID.
#' @param chrom_res ChromRes object
#' @param transitions_ids Vector of transition IDs to filter. If NULL, all transitions are returned.
#' @param samples_id Sample ID to filter.
#' @param cmpd_ids Compound ID to filter. It must be numeric. If NULL, all compounds are returned.
#' @import dplyr
#' @import checkmate
#' @export
filter_chrom <- function(
  chrom_res,
  transitions_ids = NULL,
  samples_id = NULL,
  cmpd_ids = NULL
) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumeric(transitions_ids, lower = 1, null.ok = TRUE)
  checkmate::assertNumeric(samples_id, lower = 1, null.ok = TRUE)
  checkmate::assertNumeric(cmpd_ids, lower = 1, null.ok = TRUE)

  # filter samples
  # filter transitions
  # filter peaktab
  # filter smoothed

  dat <- chrom_res@runs

  exp_peaktab <- chrom_res@peaks
  exp_transitions <- chrom_res@transitions
  exp_compounds <- chrom_res@compounds
  metadata_df <- chrom_res@metadata

  # get all compds if transitions are null
  if (is.null(transitions_ids)) {
    transitions_ids <- unique(exp_transitions$transition_id) # already T'd
  } else {
    # transitions_ids <- paste0("T", transitions_ids)
    if (
      all(transitions_ids %in% unique(exp_transitions$transition_id)) == FALSE
    ) {
      stop("Transition ID not found. Check the transition ID supplied.")
    }
  }

  # get all samples if samples are null
  if (is.null(samples_id)) {
    samples_id <- names(dat$files) |> as.numeric()
  } else if (!is.null(samples_id)) {
    if (all(samples_id %in% names(dat$files)) == FALSE) {
      stop("Sample ID not found. Check the sample ID supplied.")
    }
  }

  # get all compounds if cmpd_ids are null
  if (is.null(cmpd_ids)) {
    cmpd_ids <- unique(exp_compounds$compound_id) # already C'd
  } else if (!is.null(cmpd_ids)) {
    # cmpd_ids <- paste0("C", cmpd_ids)
    if (all(cmpd_ids %in% unique(exp_compounds$compound_id)) == FALSE) {
      stop("Compound ID not found. Check the compound ID supplied.")
    }
  }

  # sample_id: peaks and peaktab ####
  # filter samples by sample_id
  dat$files <- dat$files[as.character(samples_id)] # filter samples
  metadata_df <- metadata_df |> dplyr::filter(.data$sample_id %in% !!samples_id)

  # filter peaks intensiy by transition_id. Assume first column is RT
  dat$files <- lapply(dat$files, function(x) {
    # filter nested samples
    x[[1]] <- x[[1]][, c("RT", paste0("T", transitions_ids))] # filter transitions

    if (!is.null(x$smoothed)) {
      x$smoothed <- x$smoothed[, c("RT", paste0("T", transitions_ids))] # filter transitions
    }
    x
  })
  # compound_ids: compounds and peaktab ####
  # transitions_ids: peaktab, compounds, transitions, samples features ####

  exp_compounds <- exp_compounds |>
    dplyr::filter(.data$transition_id %in% transitions_ids) |>
    dplyr::filter(.data$compound_id %in% cmpd_ids)

  # filter peaktab by sample_id and transition_id and cmpd name
  ## NOTE: Pivoting on the newly filtered compound as it could have been filtered on transition and there is no transition_id in the peaktab
  exp_peaktab <- exp_peaktab |>
    dplyr::filter(
      .data$sample_id %in%
        samples_id &
        .data$compound_id %in% exp_compounds$compound_id
    )

  exp_transitions <- exp_transitions |>
    dplyr::filter(.data$transition_id %in% transitions_ids)

  # filter pk_metadata by sample_id
  pk_metadata <- chrom_res@pk_metadata[exp_compounds$compound_id]

  res <- new(
    "ChromRes",
    runs = dat,
    metadata = metadata_df,
    peaks = exp_peaktab,
    transitions = exp_transitions,
    compounds = exp_compounds,
    vendor = chrom_res@vendor
  )
  validObject(res)
  res
}

#' @title Smooth Chromatogram Peaks
#' @export
#' @description This function smooths chromatogram peaks using different algorithms.
#' @param chrom_res ChromRes object
#' @param filter Filter to use. Options are "mean", "median", "savgol", "gaussian"
#' @param window Window size for the filter
#' @param iter Number of iterations. If 0, no smoothing is applied.
#' @import dplyr
#' @import checkmate
#' @export
smooth_chrom <- function(chrom_res, filter = "mean", window = 2, iter = 2) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertString(filter)
  checkmate::assert_choice(
    filter,
    choices = c("mean", "median", "savgol", "gaussian")
  )
  checkmate::assertNumber(window, lower = 1)
  checkmate::assertNumber(iter, lower = 1)

  dat <- chrom_res@runs$files

  py <- reticulate::import_from_path(
    "src",
    path = system.file("pysrc", package = "PKbioanalysis")
  )

  # py <<- reticulate::import_from_path("inst", delay_load = T)
  smooth_peaks <- py$peak_integrate$smooth_peaks

  dat <- lapply(dat, \(x) {
    x$smoothed <- x[[1]] |>
      dplyr::mutate(across(-"RT", \(x) {
        smooth_peaks(
          x,
          filter,
          window = as.integer(window),
          iter = as.integer(iter)
        )
      }))

    x[[2]]$smooth_params <- paste0(filter, "_", window, "_", iter)
    x[[2]]$smoothed <- TRUE
    x
  })

  chrom_res@runs$files <- dat

  validObject(chrom_res)
  chrom_res
}

setMethod("show", "ChromRes", function(object) {
  print.ChromRes(object)
})

#' @export
print.ChromRes <- function(x, ...) {
  cat("Chromatogram Data\n")
  cat(unique(x@vendor), "\n")
  cat("Number of Samples: ", length(x@runs$files), "\n")
  cat(
    "Number of transitions: ",
    length(unique(x@transitions$transition_id)),
    "\n"
  )
  cat("Number of Compounds: ", length(unique(x@compounds$compound)), "\n")
  cat(
    "Injection Volume(s): ",
    x@metadata$injection_volume |> unique(),
    "\n"
  )
  cat(
    "Instrument(s): ",
    x@metadata$instrument |> unique(),
    "\n"
  )

  # cat(
  #     "Run Time(s): ",
  #     sapply(x@runs, \(x) x[[2]]$run_time) |> unique(),
  #     "\n"
  # )

  cat(
    "Injection Mode(s): ",
    x@metadata$injection_mode |> unique(),
    "\n"
  )

  cat(
    "Column Type(s): ",
    x@metadata$column_type |> unique(),
    "\n"
  )

  # data format. date- time
  cat(
    "First Sample Time: ",
    as.POSIXct(x@metadata$date, "%d-%b-%Y %H:%M:%S", tz = "UTC") |>
      min() |>
      format("%d-%b-%Y %H:%M:%S"),
    "\n"
  )

  cat(
    "Last Sample Time: ",
    as.POSIXct(x@metadata$date, "%d-%b-%Y %H:%M:%S", tz = "UTC") |>
      max() |>
      format("%d-%b-%Y %H:%M:%S"),
    "\n"
  )

  return(invisible(x))
}

#' @rdname is_integrated
setMethod(
  "is_integrated",
  signature(chrom_res = "ChromRes"),
  function(chrom_res, compound_id, sample_id = NULL) {
    is_integrated_chrom_res(chrom_res, compound_id, sample_id)
  }
)

#' @rdname is_integrated
setMethod(
  "is_integrated",
  signature(chrom_res = "ChromResBase"),
  function(chrom_res, compound_id, sample_id = NULL) {
    is_integrated_chrom_base(chrom_res)
  }
)

#' @title check if peak is integrated before
#' @param chrom_res ChromRes object
#' @param sample_id Sample ID. If NULL, all samples are checked
#' @param compound_id Compound ID
#' @return logical
#' @noRd
is_integrated_chrom_res <- function(chrom_res, compound_id, sample_id = NULL) {
  checkmate::assertNumber(sample_id, lower = 1, null.ok = TRUE)
  checkmate::assertCount(compound_id, positive = TRUE)
  # compound_id <- .cmpds_string_handler(compound_id)

  peaktab <- chrom_res@peaks

  if (is.null(sample_id)) {
    sample_id <- chrom_res@metadata$sample_id
  }

  compound_id_filter <- compound_id
  sample_id_filter <- sample_id

  if (nrow(peaktab) == 0) {
    stop("No compounds found")
  }
  # if(!(sample_id %in% unique(peaktab$sample_id))){
  #     stop("Sample ID not found in peaks. Please run update_RT() first or update_compound() ")
  # } else {
  res <- peaktab |>
    dplyr::filter(
      .data$sample_id == sample_id_filter &
        .data$compound_id == compound_id_filter
    ) |>
    dplyr::select(
      "observed_rt",
      "observed_peak_height",
      "observed_peak_start",
      "observed_peak_end",
      "area"
    )

  #}
  if (nrow(res) == 0) {
    stop(
      "No peaks found for the specified sample and compound. Please run update_RT() first"
    )
  }

  res <- stats::complete.cases(res)
  all(res)
}

is_integrated_chrom_base <- function(chrom_res) {
  TRUE
}


get_vials.ChromRes <- function(x) {
  x@metadata |>
    dplyr::pull("vialpos")
}

setMethod("get_vials", signature(x = "ChromRes"), get_vials.ChromRes)
