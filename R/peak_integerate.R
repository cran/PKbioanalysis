#' @title Clean and check sample IDs
#' If NULL, all samples will be returned
#' @param samples_ids Sample IDs
#' @param chrom_res ChromRes object
#' @noRd
sample_ids_handler <- function(samples_ids = NULL, chrom_res) {
  checkmate::assertVector(
    samples_ids,
    strict = TRUE,
    any.missing = FALSE,
    null.ok = TRUE
  )
  if (is.null(samples_ids)) {
    samples_ids <- names(chrom_res@runs$files)
  } else {
    samples_ids <- as.character(samples_ids)
  }
  as.character(samples_ids)
}

#' @title Update Peak Area, Observed RT, Peak Start, Peak End, Peak Height
#' @description Update Peak Area, Observed RT, Peak Start, Peak End, Peak Height for a specific compound and sample
#' Function is not vectorized and needs to be preceeded by `integrate()`
#' @param chrom_res ChromRes object
#' @param compound_id Compound ID
#' @param sample_id Sample ID
#' @param area Peak Area
#' @param observed_rt Observed RT
#' @param observed_peak_start Observed Peak Start
#' @param observed_peak_end Observed Peak End
#' @param observed_peak_height Observed Peak Height
#' @return Updated ChromRes object
#' @noRd
update_peak_area <- function(
  chrom_res,
  compound_id,
  sample_id,
  area,
  observed_rt,
  observed_peak_start,
  observed_peak_end,
  observed_peak_height
) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertCount(compound_id, positive = TRUE)
  checkmate::assertNumber(sample_id, lower = 1)
  checkmate::assertNumber(area, lower = 0)
  checkmate::assertNumber(observed_rt, lower = 0)
  checkmate::assertNumber(observed_peak_start, lower = 0)
  checkmate::assertNumber(observed_peak_end, lower = observed_peak_start)
  checkmate::assertNumber(observed_peak_height, lower = 0)

  peaktab <- chrom_res@peaks
  if (!all(sample_id %in% as.numeric(peaktab$sample_id))) {
    stop("Sample ID does not exist")
  }
  if (!all(compound_id %in% as.numeric(peaktab$compound_id))) {
    stop("Compound ID does not exist")
  }
  peaktab <- dplyr::rows_update(
    peaktab,
    data.frame(
      sample_id = as.character(sample_id),
      compound_id = compound_id,
      observed_rt = observed_rt,
      observed_peak_start = observed_peak_start,
      observed_peak_end = observed_peak_end,
      observed_peak_height = observed_peak_height,
      area = area
    ),
    by = c("sample_id", "compound_id")
  )

  chrom_res@peaks <- peaktab
  validObject(chrom_res)

  chrom_res
}

update_peak_area_from_df <- function(chrom_res, df) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertDataFrame(df, min.cols = 7, min.rows = 1)
  checkmate::assertNames(
    colnames(df),
    must.include = c(
      "sample_id",
      "compound_id",
      "observed_rt",
      "observed_peak_start",
      "observed_peak_end",
      "observed_peak_height",
      "area"
    )
  )
  chrom_res@peaks <- dplyr::rows_update(
    chrom_res@peaks,
    df,
    by = c("sample_id", "compound_id")
  )

  validObject(chrom_res)

  chrom_res
}

#' @title integrate Peak with trapzoid method given start and end
#' @description integrate Peak with trapzoid method given start and end
#' @param chrom_res ChromRes object. Must have observed RT values
#' @param compound_id Compound ID
#' @param samples_ids Sample ID. If NULL, all samples will be used
#' @param smoothed Logical. If TRUE, use smoothed chromatogram. Default is TRUE
#' @export
integrate <- function(chrom_res, compound_id, samples_ids, smoothed = TRUE) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertCount(compound_id, positive = TRUE)
  # compound_id <- .cmpds_string_handler(compound_id)
  samples_ids <- sample_ids_handler(samples_ids, chrom_res)

  transition_id <- .which_transition(chrom_res, compound_id)

  bounds <- extract_peak_bounds(chrom_res, compound_id, samples_ids)
  min_bound <- bounds$min
  max_bound <- bounds$max

  # this first filter for performance using empirical cutoffs
  filtered_peaks <- .filter_peak(
    chrom_res = chrom_res,
    transition_id = transition_id,
    samples_ids = samples_ids,
    peak_start = min_bound,
    peak_end = max_bound,
    smoothed = smoothed
  )

  checkmate::assertDataFrame(filtered_peaks, min.rows = 1, ncols = 3)
  checkmate::assertNames(
    colnames(filtered_peaks),
    must.include = c("RT", paste0("T", transition_id), "sample_id")
  )
  stopifnot(all(sort(unique(filtered_peaks$sample_id)) == sort(samples_ids)))

  # group intesities by sample_id and compound_id, then integrate
  # here I need to join to be able to find the peakstart and end at peaktab
  integrated_peaks <-
    filtered_peaks |>
    dplyr::left_join(
      chrom_res@peaks |>
        filter(.data$compound_id == !!compound_id) |>
        filter(.data$sample_id %in% samples_ids),
      by = dplyr::join_by("sample_id")
    ) |>
    dplyr::mutate(area = as.numeric(.data$area)) |>
    dplyr::group_by(
      .data$sample_id,
      .data$compound_id,
      .data$observed_peak_start,
      .data$observed_peak_end,
      .data$observed_rt,
      .data$observed_peak_height
    ) |>
    dplyr::filter(
      .data$RT >= .data$observed_peak_start &
        .data$RT <= .data$observed_peak_end
    ) |>
    dplyr::mutate(
      observed_peak_height = max(.data[[paste0("T", transition_id)]])
    ) |> # max peak height
    dplyr::mutate(
      observed_rt = .data$RT[which.max(.data[[paste0("T", transition_id)]])]
    ) |> # observed RT
    dplyr::summarize(
      area = pracma::trapz(.data$RT, .data[[paste0("T", transition_id)]])
    )

  update_peak_area_from_df(chrom_res, integrated_peaks)
}


#' Check Matching of Compound and Transitions in chrom_res and method database
#' @param chrom_res ChromRes object
#' @param method_id Method ID in the method database
#' This is important to give no error before merging quantification results to ensure consistency.
#' @return TRUE if all compounds and transitions match, otherwise FALSE
check_chrom_cmpds <- function(chrom_res, method_id) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertCount(method_id, positive = TRUE)

  # expect_equal(n_samples * n_cmpd == nrow(dat1@peaks), TRUE)
  # number of cmpds here is there
  n_cmpd <- nrow(chrom_res@compounds)
  n_samples <- length(chrom_res@runs$files)
  if (n_samples * n_cmpd != nrow(chrom_res@peaks)) {
    message(
      "Number of compounds in chrom_res does not match number of compounds in method database"
    )
  }
  # all cmpd names here in there
  transitions_db <- .get_method_transitions(method_id)
  cmpds_db <- .get_method_cmpds(method_id)

  message("Number of compounds in chrom_res: ", n_cmpd)
  message("Number of compounds in method database: ", nrow(cmpds_db))
  message("Number of transitions in method database: ", nrow(transitions_db))
  message("Number of transitions in chrom_res: ", nrow(chrom_res@transitions))

  bool <- TRUE
  cmpdsdb_names <- make.unique(cmpds_db$compound, sep = "_") # automatically convert repeted names like chromres
  if (!all(chrom_res@compounds$compound %in% cmpdsdb_names)) {
    ("Some compounds in chrom_res not found in method database")
    message(chrom_res@compounds$compound[
      !chrom_res@compounds$compound %in% cmpdsdb_names
    ])
    message("These compounds are not found in method database:")
    message(paste(
      setdiff(chrom_res@compounds$compound, cmpdsdb_names),
      collapse = ", "
    ))
    bool <- FALSE
  }

  if (!all(cmpdsdb_names %in% chrom_res@compounds$compound)) {
    message("Some compounds in method database not found in chrom_res")
    message(paste(
      cmpdsdb_names[!cmpdsdb_names %in% chrom_res@compounds$compound],
      collapse = ", "
    ))
    message("These compounds are not found in chrom_res:")
    message(paste(
      setdiff(cmpdsdb_names, chrom_res@compounds$compound),
      collapse = ", "
    ))
    bool <- FALSE
  }

  # all transitions here in there
  if (
    !all(
      chrom_res@transitions$transition_label %in%
        transitions_db$transition_label
    )
  ) {
    message("Some transitions in chrom_res not found in method database")
    message("These transitions are not found in method database:")
    message(paste(
      setdiff(
        chrom_res@transitions$transition_label,
        transitions_db$transition_label
      ),
      collapse = ", "
    ))
    bool <- FALSE
  }
  if (
    !all(
      transitions_db$transition_label %in%
        chrom_res@transitions$transition_label
    )
  ) {
    message("Some transitions in method database not found in chrom_res")
    message("These transitions are not found in chrom_res:")
    message(paste(
      setdiff(
        transitions_db$transition_label,
        chrom_res@transitions$transition_label
      ),
      collapse = ", "
    ))
    bool <- FALSE
  }
  bool
}

# #' @title Add a compound to quantify
# #' @description Add a compound to quantify. This function will add a compound to quantify in the chromatogram.
# #' This function does not reintegrate the chromatogram automatically.
# #' @param chrom_res ChromRes object
# #' @param transition_id Transition ID
# #' @param compound_name Compound name
# #' @param IS Internal Standard. ID of a compound in the chromatogram
# #'
# #' @import checkmate
# #' @noRd
# add_compound <- function(chrom_res, compound_name, transition_id, IS = NA){
#     checkmate::assertClass(chrom_res, "ChromRes")
#     transition_id <- .transition_string_handler(transition_id)
#     checkmate::assertCount(IS, na.ok = TRUE, positive = TRUE)

#     dat <- chrom_res@runs # not to be used
#     peaktab <- chrom_res@peaks
#     compounds <- chrom_res@compounds
#     transitions <- chrom_res@transitions

#     if(!(transition_id %in% transitions$transition_id)){
#         stop("Transition ID not found in transitions")
#     }
#     if(compound_name %in% compounds$compound){
#         stop("Compound name already in compounds. Try different name.")
#     }

#     if(!is.na(IS)){
#         if(!(IS %in% compounds$compound_id)){
#             stop("IS ID not found in compounds")
#         }
#     }

#     # increment last compound_id
#     cmpd_id <- ifelse(nrow(compounds) == 0, 1,
#         max(as.numeric(.trim_id(compounds$compound_id, "C"))) + 1)
#     cmpd_id <- paste0("C", cmpd_id)

#     compounds <- dplyr::rows_insert(compounds,
#         data.frame(
#             compound_id = cmpd_id,
#             compound = compound_name,
#             transition_id = transition_id,
#             expected_peak_start = as.numeric(NA),
#             expected_peak_end = as.numeric(NA),
#             expected_rt = as.numeric(NA),
#             IS = IS
#         ),
#         by = "compound_id"
#     )

#     peaktab <- dplyr::rows_insert(peaktab,
#         data.frame(
#             sample_id = names(dat),
#             filename = lapply(dat, function(x) x[[2]]$filename) |> unlist(),
#             compound_id =  cmpd_id,
#             transition_id = transition_id,
#             observed_rt = as.numeric(NA),
#             observed_peak_start = as.numeric(NA),
#             observed_peak_end = as.numeric(NA),
#             IS = IS,
#             observed_peak_height = as.numeric(NA), area = as.numeric(NA), manual = FALSE),
#             by = c("sample_id", "compound_id", "transition_id")
#         )

#     chrom_res@peaks <- peaktab
#     chrom_res@compounds <- compounds
#     validObject(chrom_res)
#     chrom_res

# }

# #' @title Update a compound name
# #' @description Update a compound in quantification. This function will update a compound in quantification in the chromatogram.
# #' @param chrom_res ChromRes object
# #' @param compound_id Compound ID
# #' @param new_name New name of the compound
# #' @param IS Internal Standard. ID of a compound in the chromatogram
# #' @import checkmate
# #' @noRd
# update_compound <- function(chrom_res, compound_id, new_name = NULL, IS=NA){
#     checkmate::assertClass(chrom_res, "ChromRes")
#     compound_id <- .cmpds_string_handler(compound_id)
#     checkmate::assertCharacter(new_name, null.ok = TRUE)
#     checkmate::assertCount(IS, na.ok = TRUE, positive = TRUE)

#     dat <- chrom_res@runs
#     peaktab <- chrom_res@peaks
#     compounds <- chrom_res@compounds
#     transitions <- chrom_res@transitions

#     if(!(compound_id %in% compounds$compound_id)){
#         stop("Compound ID not found in compounds")}

#     if(!is.null(new_name)){
#         if(new_name %in% compounds$compound){
#             stop("Compound name already in compounds. Try different name.")
#         }

#         compounds$compound[compounds$compound_id == compound_id] <- new_name
#         peaktab$compound[peaktab$compound_id == compound_id] <- new_name
#     }

#     if(!is.na(IS)){
#         if(!(IS %in% compounds$compound_id)){
#             stop("IS ID not found in compounds")
#         }
#         compounds$IS[compounds$compound_id == compound_id] <- IS
#     }

#     # update compounds and peaktab
#     chrom_res@compounds <- compounds
#     chrom_res@peaks <- peaktab

#     validObject(chrom_res)
#     chrom_res

# }

# #' @title Remove a compound from quantification
# #' @description Remove a compound from quantification. This function will remove a compound from quantification in the chromatogram.
# #' This function will also remove any compounds that has been integrated before.
# #' @param chrom_res ChromRes object
# #' @param compound_id Compound ID
# #' @import checkmate
# #' @noRd
# remove_compound <- function(chrom_res, compound_id){
#     checkmate::assertClass(chrom_res, "ChromRes")
#     compound_id <- .cmpds_string_handler(compound_id)

#     dat <- chrom_res@runs
#     peaktab <- chrom_res@peaks
#     compounds <- chrom_res@compounds
#     transitions <- chrom_res@transitions

#     if(!(compound_id %in% compounds$compound_id)){
#         stop("Compound ID not found in compounds")
#     }

#     compound_id_filter <- compound_id
#     # remove the compound from compounds
#     compounds <- compounds |> dplyr::filter(compound_id != compound_id_filter)

#     # remove the compound from peaktab
#     peaktab <- peaktab |> dplyr::filter(compound_id != compound_id_filter)

#     chrom_res@compounds <- compounds
#     chrom_res@peaks <- peaktab

#     validObject(chrom_res)
#     chrom_res

# }

#' @title Find transition ID for a compound
#' @noRd
.which_transition <- function(chrom_res, compound_id) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertCount(compound_id, positive = TRUE)
  # compound_id <- .cmpds_string_handler(compound_id)

  compounds <- chrom_res@compounds
  # find the transition_id for the compound_id
  if (!(compound_id %in% compounds$compound_id)) {
    stop("Compound ID not found in compounds")
  }
  index <- compounds$transition_id[which(compounds$compound_id == compound_id)]

  chrom_res@transitions |>
    dplyr::filter(.data$transition_id == index) |>
    dplyr::pull("transition_id")
}

#' @title Filter peaks to specific RT range
#'
#' @param chrom_res ChromRes object
#' @param transition_id Transition ID
#' @param samples_ids Sample ID. If NULL, all samples will be used
#' @param peak_start Minimum RT value. If NULL, all RT values will be used
#' @param peak_end Maximum RT value. If NULL, all RT values will be used
#' @param smoothed Logical. If TRUE, use smoothed chromatogram. Default is FALSE
#' @return Dataframe with RT, intensity and sample_id
#' @keywords internal
.filter_peak <- function(
  chrom_res,
  transition_id,
  samples_ids = NULL,
  peak_start,
  peak_end,
  smoothed = FALSE
) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumeric(peak_start, lower = 0, null.ok = FALSE)
  checkmate::assertNumeric(peak_end, lower = min(peak_start), null.ok = TRUE)
  transition_id <- .transition_string_handler(transition_id)
  samples_ids <- sample_ids_handler(samples_ids, chrom_res)

  if (is_smoothed(chrom_res)$smoothed |> all() == FALSE & smoothed) {
    stop("Chromatogram not smoothed. Please smooth the chromatogram first.")
  }

  i <- ifelse(smoothed, "smoothed", "sample_chrom")
  intensities <- lapply(seq_along(samples_ids), function(i) {
    x <- chrom_res@runs$files[[samples_ids[i]]]$sample_chrom
    x |>
      dplyr::select("RT", !!transition_id) |>
      dplyr::mutate(sample_id = samples_ids[i])
  }) |>
    dplyr::bind_rows()

  stopifnot(nrow(intensities) > 0)
  if (is.null(peak_end)) {
    peak_end <- max(intensities$RT)
  }
  # ensure vectorization
  intensities <- intensities |>
    dplyr::left_join(
      data.frame(
        sample_id = samples_ids,
        peak_start = peak_start,
        peak_end = peak_end
      ),
      by = "sample_id"
    ) |>
    dplyr::filter(.data$RT >= .data$peak_start & .data$RT <= .data$peak_end) |>
    dplyr::select(-c("peak_start", "peak_end"))

  if (!(transition_id %in% colnames(intensities))) {
    stop(
      "Transition ID not found in chromatogram. Check the transitions chrom_res)"
    )
  }

  intensities
}

#' @title Find max intensity for a sample
#' Observed RT regarless manual or automatic
#' @param intensities Dataframe coming from .filter_peak
#' @return Dataframe with sample_id, RT and peak_height
#' @noRd
.find_max_intensity <- function(intensities) {
  stopifnot(ncol(intensities) == 3)
  stopifnot("sample_id" %in% colnames(intensities))
  stopifnot("RT" %in% colnames(intensities))

  col_i <- colnames(intensities)[2] |> as.symbol()
  intensities |>
    dplyr::slice_max(!!col_i, with_ties = FALSE, by = "sample_id") |>
    dplyr::rename(peak_height = !!col_i)
}


.integrate_all_slack <- function(
  chrom_res,
  compound_id,
  peak_start,
  peak_end,
  mode = "auto"
) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumber(peak_start, lower = 0)
  checkmate::assertNumber(peak_end, lower = peak_start)
  checkmate::assertChoice(mode, choices = c("manual", "auto", "ai"))

  message("Updating expected RT for all samples for compound ", compound_id, " in mode ", mode)

  # Update expected RT for the compound (applies to all samples)
  chrom_res <- .update_expected_rt(
    chrom_res,
    compound_id,
    sample_ids = NULL,
    peak_start,
    peak_end,
    mode = mode
  )

  # Get all sample IDs
  all_sample_ids <- names(chrom_res@runs$files)

  # For AI mode, loop through each sample individually
  if (mode == "ai") {
    for (i in all_sample_ids) {
      chrom_res <- .integrate_individual_slack(
        chrom_res,
        compound_id,
        i,
        peak_start,
        peak_end,
        mode = mode
      )
    }
    return(chrom_res)
  }

  # For manual and auto modes, use batch processing
  transition_id <- .which_transition(chrom_res, compound_id)
  obs_rt <- .filter_peak(
    chrom_res,
    transition_id,
    samples_ids = NULL,
    peak_start = peak_start,
    peak_end = peak_end
  )

  # Update observed RT values for all samples
  chrom_res@peaks <- .update_observed_rt(
    chrom_res@peaks,
    obs_rt,
    compound_id,
    all_sample_ids,
    peak_start,
    peak_end,
    mode = mode
  )

  # Integrate for all samples
  integrate(chrom_res, compound_id, samples_ids = NULL)
}

#' @title Update expected RT values (observed and expected bounds)
#' @description Helper function to update both observed and expected RT values
#' @param chrom_res ChromRes object
#' @param compound_id Compound ID to update
#' @param sample_ids Sample IDs to update (NULL for all samples)
#' @param peak_start New peak start RT value
#' @param peak_end New peak end RT value
#' @param mode Integration mode: "manual", "auto", or "ai"
#' @return Updated ChromRes object
#' @noRd
.update_expected_rt <- function(
  chrom_res,
  compound_id,
  sample_ids = NULL,
  peak_start,
  peak_end,
  mode = "auto"
) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumber(peak_start, lower = 0)
  checkmate::assertNumber(peak_end, lower = peak_start)
  checkmate::assertChoice(mode, choices = c("manual", "auto", "ai"))

  # Update expected RT for the compound (both observed and expected bounds)
  chrom_res@compounds <- chrom_res@compounds |>
    dplyr::mutate(
      expected_peak_start = ifelse(
        .data$compound_id == compound_id,
        peak_start,
        .data$expected_peak_start
      ),
      expected_peak_end = ifelse(
        .data$compound_id == compound_id,
        peak_end,
        .data$expected_peak_end
      ),
      expected_rt = ifelse(
        .data$compound_id == compound_id,
        (peak_start + peak_end) / 2,
        .data$expected_rt
      )
    )

  chrom_res
}

## ---- Observed RT update helpers (DRY) ----
#' @title Apply manual bounds to peaks table
#' @noRd
.apply_manual_bounds <- function(
  peaktab,
  sample_ids,
  compound_id,
  peak_start,
  peak_end,
  comment = "",
  flag = FALSE
) {
  dplyr::rows_update(
    peaktab,
    data.frame(
      sample_id = as.character(sample_ids),
      compound_id = compound_id,
      observed_rt = as.numeric(NA),
      observed_peak_start = peak_start,
      observed_peak_end = peak_end,
      observed_peak_height = as.numeric(NA),
      area = as.numeric(NA),
      manual = TRUE,
      flag = flag,
      comment = comment
    ),
    by = c("sample_id", "compound_id")
  )
}

#' @title Detect peaks by mode
#' @description Helper function to detect peaks based on the specified mode (auto or ai)
#' @param obs_rt Observed RT data
#' @param mode Integration mode: "auto" or "ai"
#' @noRd
.detect_peaks_by_mode <- function(obs_rt, mode = "auto") {
  if (is.null(obs_rt) || nrow(obs_rt) == 0) {
    return(NULL)
  }

  x <- split(obs_rt, obs_rt$sample_id)

  fp <- switch(
    mode,
    "auto" = py$peak_integrate$find_peak2_d,
    NULL
  )

  if (is.null(fp)) {
    return(NULL)
  }

  fp(x, 1)
}

#' @title Apply detected peaks to peaks table
#' @noRd
.apply_detected_peaks <- function(peaktab, detected, compound_id) {
  if (is.null(detected) || nrow(detected) == 0) {
    return(peaktab)
  }

  detected$compound_id <- compound_id
  detected$sample_id <- as.character(detected$sample_id)
  detected$manual <- FALSE
  detected$area <- NA

  dplyr::rows_update(
    peaktab,
    detected,
    by = c("sample_id", "compound_id"),
    unmatched = "ignore"
  )
}

#' @title Update observed RT values in peaks table
#' @description Helper function to update observed RT values based on mode
#' @param peaktab Peaks table to update
#' @param obs_rt Observed RT data
#' @param compound_id Compound ID to update
#' @param sample_ids Sample IDs to update
#' @param peak_start Peak start RT value
#' @param peak_end Peak end RT value
#' @param mode Integration mode: "manual", "auto", or "ai"
#' @param comment Comment for manual updates
#' @param flag Whether to flag the peak
#' @return Updated peaks table
#' @noRd
.update_observed_rt <- function(
  peaktab,
  obs_rt,
  compound_id,
  sample_ids,
  peak_start,
  peak_end,
  mode = "manual",
  comment = "",
  flag = FALSE
) {
  checkmate::assertDataFrame(peaktab)
  checkmate::assertChoice(mode, choices = c("manual", "auto", "ai"))

  if (mode == "manual") {
    peaktab <- .apply_manual_bounds(
      peaktab,
      sample_ids,
      compound_id,
      peak_start,
      peak_end,
      comment = comment,
      flag = flag
    )

    if (!is.null(obs_rt) && nrow(obs_rt) > 0) {
      obs_rt <- obs_rt |>
        .find_max_intensity() |>
        mutate(compound_id = compound_id) |>
        dplyr::rename(observed_peak_height = "peak_height") |>
        dplyr::rename(observed_rt = "RT")

      peaktab <- dplyr::rows_patch(
        peaktab,
        obs_rt,
        by = c("sample_id", "compound_id")
      )
    }

    return(peaktab)
  }

  # For auto and ai modes
  if (mode == "ai") {
    # For AI mode, apply the refined bounds with comment and flag
    peaktab <- .apply_manual_bounds(
      peaktab,
      sample_ids,
      compound_id,
      peak_start,
      peak_end,
      comment = comment,
      flag = flag
    )
    
    # Find and patch max intensity from filtered peak data
    if (!is.null(obs_rt) && nrow(obs_rt) > 0) {
      obs_rt <- obs_rt |>
        .find_max_intensity() |>
        mutate(compound_id = compound_id) |>
        dplyr::rename(observed_peak_height = "peak_height") |>
        dplyr::rename(observed_rt = "RT")

      peaktab <- dplyr::rows_patch(
        peaktab,
        obs_rt,
        by = c("sample_id", "compound_id")
      )
    }
    
    return(peaktab)
  }

  # For auto mode, use Python peak detection
  detected <- .detect_peaks_by_mode(obs_rt, mode = mode)
  .apply_detected_peaks(peaktab, detected, compound_id)
}

#' @title integrate Next Peak
#' @noRd
.integrate_next_slack <- function(
  chrom_res,
  compound_id,
  sample_id,
  peak_start,
  peak_end,
  mode = "auto"
) {
  # >=sample_id
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumber(peak_start, lower = 0)
  checkmate::assertNumber(peak_end, lower = peak_start)
  checkmate::assertChoice(mode, choices = c("manual", "auto", "ai"))

  message("Updating RT for all samples starting from sample ", sample_id, " for compound ", compound_id, " in mode ", mode)

  if (!has_default_bounds(chrom_res, compound_id)) {
    stop(
      "Expected RT not set. Use target = 'all' to set expected RT for the first time"
    )
  }

  all_sample_ids <- names(chrom_res@runs$files)
  start_idx <- match(as.character(sample_id), all_sample_ids)
  if (is.na(start_idx)) {
    stop("Sample ID not found in chromatogram")
  }

  target_sample_ids <- all_sample_ids[start_idx:length(all_sample_ids)]

  for (i in target_sample_ids) {
    chrom_res <- .integrate_individual_slack(
      chrom_res,
      compound_id,
      i,
      peak_start,
      peak_end,
      mode = mode
    )
  }

  chrom_res
}

#' @title integrate single sample
#' @details This is a unified integration function that handles manual, auto, and ai modes
#' @noRd
.integrate_individual_slack <- function(
  chrom_res,
  compound_id,
  sample_id,
  peak_start,
  peak_end,
  mode = "auto",
  comment = "",
  flag = FALSE
) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumber(peak_start, lower = 0)
  checkmate::assertNumber(peak_end, lower = peak_start)
  checkmate::assertChoice(mode, choices = c("manual", "auto", "ai"))
  checkmate::assertString(comment)
  checkmate::assertLogical(flag)

  message("Updating RT for sample ", sample_id, " for compound ", compound_id, " in mode ", mode)

  # Check if expected bounds exist for auto mode
  if (!has_default_bounds(chrom_res, compound_id) & mode == "auto") {
    stop(
      "Expected RT not set. Use target = 'all' to set expected RT for the first time"
    )
  }

  # For AI mode, call integrate_ai to get refined bounds and metadata
  if (mode == "ai") {
    transition_id <- .which_transition(chrom_res, compound_id)
    ai_result <- integrate_ai(
      chrom_res,
      transition_id = transition_id,
      sample_id = sample_id,
      peak_start = peak_start,
      peak_end = peak_end
    )
    
    # Use AI-refined bounds and metadata
    peak_start <- ai_result$peak_start
    peak_end <- ai_result$peak_end
    comment <- ai_result$comment
    flag <- ai_result$flagged
  }

  # Find transition and filter peak data
  transition_id <- .which_transition(chrom_res, compound_id)
  obs_rt <- .filter_peak(
    chrom_res,
    transition_id,
    samples_ids = sample_id,
    peak_start = peak_start,
    peak_end = peak_end
  )

  # Update observed RT values in peaks table
  chrom_res@peaks <- .update_observed_rt(
    chrom_res@peaks,
    obs_rt,
    compound_id,
    sample_id,
    peak_start,
    peak_end,
    mode = mode,
    comment = comment,
    flag = flag
  )

  # Integrate the peaks
  integrate(chrom_res, compound_id, samples_ids = sample_id)
}


#' This function will make any area below the cutoff to be zero
#'@noRd
apply_area_cutoff <- function(chrom_res, cutoff, compound_id) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumber(cutoff, lower = 0, upper = 1e20)
  # compound_id <- .cmpds_string_handler(compound_id)

  peaktab <- chrom_res@peaks

  peaktab <- peaktab |>
    dplyr::mutate(
      area = ifelse(
        .data$area < cutoff & .data$compound_id == !!compound_id,
        0,
        .data$area
      )
    )

  chrom_res@peaks <- peaktab
  validObject(chrom_res)
  chrom_res
}

#' @title Manually Update Observed RT for either all compounds, all next samples, or single compound and sample
#' @description Update RT for either all compounds, all next samples, or single compound and sample
#' @param chrom_res ChromRes object
#' @param compound_id Compound ID
#' @param sample_id Sample ID (required for "single" and "all_next", must be NULL for "all")
#' @param peak_start Minimum RT value
#' @param peak_end Maximum RT value
#' @param mode Mode of update. Options are "auto", "manual", "ai". Default is "auto"
#' @param target Target of update. Options are "single", "all", "all_next". Default is "single"
#' @param force Force update if previous peak exists. Default is FALSE
#' @param comment Comment for the update. Default is an empty string
#' @param flag Flag the peak after update. Default is FALSE
#' @return Updated ChromRes object
#' @details 
#' - target = "single": Updates RT for one compound and sample
#' - target = "all": Updates expected RT for all samples (sets expected bounds)
#' - target = "all_next": Updates RT for specified sample and subsequent samples
#' 
#' All modes affect both observed and expected RT values:
#' - "manual": Sets exact peak bounds, marks as manual
#' - "auto": Auto-detects peaks within bounds
#' - "ai": AI-based peak detection
#' @export
#' @examples
#' \dontrun{
#' update_RT(chrom_res, compound_id = 1, sample_id = 1,
#'           peak_start = 0.1, peak_end = 1, target = "single")
#' }
update_RT <- function(
  chrom_res,
  compound_id,
  sample_id = NULL,
  peak_start,
  peak_end,
  mode = "auto",
  target = "single",
  force = FALSE,
  comment = "",
  flag = FALSE
) {
  # Input validation
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumber(sample_id, lower = 1, null.ok = TRUE)
  checkmate::assertNumber(peak_start, lower = 0)
  checkmate::assertNumber(peak_end, lower = peak_start)
  checkmate::assert_choice(target, choices = c("single", "all", "all_next"))
  checkmate::assertChoice(mode, choices = c("auto", "manual", "ai"))
  checkmate::assertLogical(force)
  checkmate::assertString(comment)
  checkmate::assertLogical(flag)

  stopifnot(peak_start < peak_end)

  # Check compound exists
  if (!(compound_id %in% chrom_res@compounds$compound_id)) {
    stop("Compound ID not found in compounds. Use add_compound() to add compound.")
  }

  # Check and validate sample_id requirement based on target
  .validate_update_rt_params(chrom_res, sample_id, target)

  # Route to appropriate integration strategy
  switch(target,
    "single" = .integrate_individual_slack(chrom_res, compound_id, sample_id, peak_start, peak_end, mode, comment, flag),
    "all" = .integrate_all_slack(chrom_res, compound_id, peak_start, peak_end, mode),
    "all_next" = .integrate_next_slack(chrom_res, compound_id, sample_id, peak_start, peak_end, mode)
  )
}

#' @title Validate update_RT parameters
#' @noRd
.validate_update_rt_params <- function(chrom_res, sample_id, target) {
  if (!is.null(sample_id)) {
    if (!(sample_id %in% names(chrom_res@runs$files))) {
      stop("Sample ID not found in chromatogram")
    }
  } else {
    # sample_id is NULL
    if (target %in% c("single", "all_next")) {
      stop("Sample ID not specified. You can only use 'all' for target when sample_id is NULL")
    }
  }
}


#' @title Automatic Peak Boundary Detection
#' @description Find peak in chromatogram. This function will use the dataframe with min and max RT values to find the peak in the chromatogram.
#' @param chrom_res ChromRes object
#' @param cutoff Cutoff value for peak height
#' @details The RT_df dataframe should have the following columns: compound_id, RT_min, RT_max.
#' @import checkmate
#' @noRd
#' @author Omar Elashkar
.find_peak <- function(chrom_res, cutoff = 1e2) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertNumber(cutoff, lower = 0, upper = 1e20)

  # recieve list with samples and chromatogram to find peak
  # pass to python as list
  # retrieve list with peak_start, peak_end, peak_height
  # update peaktab

  dat <- chrom_res@runs
  peaktab <- chrom_res@peaks

  stopifnot(length(unique(dat$sample)) == 1)
  stopifnot(length(unique(dat$compound_id)) == 1)

  # find the highest prominenant peak in range
  find_peak_py <- py$peak_integrate$find_peak2
  integral_range <- py$peak_integrate$integral_range

  if (all(is_smoothed(chrom_res))) {
    intensity_vec <- dat$smoothed_intensity
  } else {
    intensity_vec <- dat$Intensity
  }

  x <- find_peak_py(intensity_vec, height = cutoff) # min and max not used here as it is already filtered

  if (length(x) == 2) {
    xloc <- x$RT[[1]]
    yloc <- x$prom[[1]]
    actual_peak_start <- x$prom[[2]]
    actual_peak_end <- x$prom[[3]]
  } else {
    xloc <- NA
    yloc <- NA
    actual_peak_start <- 0
    actual_peak_end <- 0
  }

  # in some cases, Intensity is 0 and no peak is found.
  if (actual_peak_start != 0) {
    calculated_peak_area <- integral_range(
      dat$RT,
      dat$Intensity,
      actual_peak_start,
      actual_peak_end
    )[[1]]
  } else {
    calculated_peak_area <- 0
  }

  # allow unequal columns. slice the last row.
  peaktab <- dplyr::bind_rows(
    peaktab,
    data.frame(
      compound_id = unique(dat$compound_id),
      sample = unique(dat$sample),
      compound = unique(dat$compound),
      sample_id = unique(dat$sample_id),
      observed_rt = ifelse(length(xloc) == 0, 0, xloc),
      observed_peak_height = ifelse(length(yloc) == 0, 0, yloc),
      observed_peak_start = ifelse(
        length(actual_peak_start) == 0,
        0,
        actual_peak_start
      ),
      observed_peak_end = ifelse(
        length(actual_peak_end) == 0,
        0,
        actual_peak_end
      ),
      expected_peak_start = min,
      expected_peak_end = max,
      expected_peak_RT = (min + max) / 2,
      peak_area = calculated_peak_area
    )
  ) |> # rbind closed here
    dplyr::group_by(
      .data$compound_id,
      .data$sample_id,
      .data$compound,
      .data$sample
    ) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup()

  chrom_res@peaks <- peaktab
  validObject(chrom_res)
  chrom_res
}


#' @title Extract Peak Boundaries
#' @description Extract peak boundaries for all samples for a given compound ID
#' @param chrom_res ChromRes object
#' @param compound_id Compound ID
#' @param samples_ids Sample IDs. If NULL, all samples will be used
#' The function automatically priortizes observed peak boundaries (manual integration) over expected ones.
#' If observed boundaries are not available, it falls back to expected boundaries.
#' @return Dataframe with compound_id, min, max
extract_peak_bounds <- function(chrom_res, compound_id, samples_ids = NULL) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertCount(compound_id, positive = TRUE)
  # compound_id <- .cmpds_string_handler(compound_id)
  compounds <- chrom_res@compounds
  if (!(compound_id %in% compounds$compound_id)) {
    stop("Compound ID not found in compounds")
  }
  if (is.null(samples_ids)) {
    samples_ids <- names(chrom_res@runs$files)
  } else {
    samples_ids <- sample_ids_handler(samples_ids, chrom_res)
  }

  if (
    !has_observed_bounds(chrom_res, compound_id, samples_ids) &
      !has_default_bounds(chrom_res, compound_id)
  ) {
    stop(
      "Compound must have either observed or expected peak boundaries to proceed"
    )
  }

  peaktab <- chrom_res@peaks
  peaktab <- peaktab |>
    dplyr::filter(
      .data$compound_id == !!compound_id,
      .data$sample_id %in% !!samples_ids
    )

  res <- data.frame(
    compound_id = peaktab$compound_id,
    min = ifelse(
      !is.na(peaktab$observed_peak_start),
      peaktab$observed_peak_start,
      peaktab$expected_peak_start
    ),
    max = ifelse(
      !is.na(peaktab$observed_peak_end),
      peaktab$observed_peak_end,
      peaktab$expected_peak_end
    )
  )

  if (any(is.na(res$min)) || any(is.na(res$max))) {
    stop(
      "Some peaks do not have observed peak boundaries. Please check the peaktab."
    )
  }

  res
}

#' @title Check if samples have observed bounds for a specific compound
#' @description Check whether all selected samples have observed peak boundaries
#'   (both start and end) for the given compound. Optionally filters by sample IDs.
#' @param chrom_res ChromRes object containing peak information
#' @param compound_id Compound ID to check
#' @param samples_ids Sample IDs. If NULL, all samples for the compound are used
#' @noRd
has_observed_bounds <- function(chrom_res, compound_id, samples_ids = NULL) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertCount(compound_id, positive = TRUE)
  # compound_id <- .cmpds_string_handler(compound_id)
  peaktab <- chrom_res@peaks
  if (!(compound_id %in% peaktab$compound_id)) {
    stop("Compound ID not found in compounds")
  }
  peaktab <- peaktab |>
    dplyr::filter(.data$compound_id == !!compound_id)
  if (!is.null(samples_ids)) {
    peaktab <- peaktab |>
      dplyr::filter(.data$sample_id %in% !!samples_ids)
  }
  all(!is.na(peaktab$observed_peak_start) & !is.na(peaktab$observed_peak_end))
}
