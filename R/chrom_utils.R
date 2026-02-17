#'@title Find sample names for all samples
#'@param chrom_res ChromRes object
#'@return data.frame with sample and sample_id
#'@export
get_sample_names <- function(chrom_res) {
  # dat <- chrom_res@runs
  # sample_name <- lapply(dat, \(x) data.frame(sample = x[[2]]$sample, sample_id = x[[2]]$sample_id))
  # do.call(rbind, sample_name)
  sample_name <- chrom_res@peaks |>
    dplyr::select("filename", "sample_id") |>
    dplyr::distinct() |>
    dplyr::rename(sample = "filename", sample_id = "sample_id")
  sample_name
}

#'@title Find Sample ID from sample Name
#' @description This function returns the sample ID
#' @param chrom_res ChromRes object
#' @param sample_name Sample Name
#' @export
get_sample_ID <- function(chrom_res, sample_name) {
  sample_id <- get_sample_names(chrom_res)
  sample_id <- sample_id[sample_id$sample == sample_name, "sample_id"]
  if (length(sample_id) == 0) {
    stop("Sample not found.")
  }
  sample_id
}


#' @title Find Compound Name from compound ID
#' @description This function returns the compound name
#' @param chrom_res ChromRes object
#' @param compound_id Compound ID
#' @noRd
get_compound_name <- function(chrom_res, compound_id) {
  compounds <- chrom_res@compounds[
    chrom_res@compounds$compound_id == compound_id,
    "compound"
  ]
  compounds
}

#' @title Find Compound ID from compound Name
#' @description This function returns the compound ID
#' @param chrom_res ChromRes object
#' @param compound_name Compound Name
#' @export
get_compound_ID <- function(chrom_res, compound_name) {
  compound_id <- chrom_res@compounds[
    chrom_res@compounds$compound == compound_name,
    "compound_id"
  ]
  if (length(compound_id) == 0) {
    stop("Compound not found.")
  }
  compound_id
}


#'@title Update internal standard for a compound
#'@param chrom_res ChromRes object
#'@param compound_id Compound ID
#'@param standard_id Standard ID
#'@noRd
#'@author Omar Elashkar
update_IS <- function(chrom_res, compound_id, standard_id) {
  checkmate::assertClass(chrom_res, "ChromResBase")
  checkmate::assertCount(compound_id, positive = TRUE)
  checkmate::assertCount(standard_id, positive = TRUE)

  # compound_id <- .cmpds_string_handler(compound_id)
  # standard_id <- .cmpds_string_handler(standard_id)

  if (!(compound_id %in% chrom_res@compounds$compound_id)) {
    stop("Compound ID not found in compounds")
  }
  if (!(standard_id %in% chrom_res@compounds$compound_id)) {
    stop("Standard ID not found in compounds")
  }
  chrom_res@compounds <- chrom_res@compounds |>
    dplyr::mutate(
      IS_id = ifelse(.data$compound_id == !!compound_id, standard_id, .data$IS_id)
    )
  chrom_res
}

#' @title Find IS Name for corresponding compound ID
#' @description This function returns the IS name
#' @param chrom_res ChromRes object
#' @param compound_id Compound ID
#' @author Omar Elashkar
#' @noRd
#' @examples
#' \dontrun{
#' get_cmpd_IS(chrom_res, 1)
#' }
get_cmpd_IS <- function(chrom_res, compound_id) {
  if (!(compound_id %in% chrom_res@compounds$compound_id)) {
    stop("Compound ID not found in compounds")
  }

  is_id <- chrom_res@compounds |>
    dplyr::filter(.data$compound_id == !!compound_id) |>
    dplyr::pull("IS_id")
  is_id # returns NA if no IS is set
}

#' @title Get transition ID from transition label
#' @noRd
get_trans_id_from_trans_label <- function(chrom_res, label) {
  chrom_res@transitions |>
    dplyr::filter(.data$transition_label == label) |>
    dplyr::pull("transition_id")
}


get_trans_label_from_id <- function(chrom_res, trans_id) {
  chrom_res@transitions |>
    dplyr::filter(.data$transition_id == trans_id) |>
    dplyr::pull("transition_label")
}


get_trans_id_from_cmpd_id <- function(chrom_res, cmpd_id) {
  trans_id <- chrom_res@compounds |>
    dplyr::filter(.data$compound_id == !!cmpd_id) |>
    dplyr::pull("transition_id")
  stopifnot(length(trans_id) == 1)
  trans_id
}


#' @title Remove leading ID identifier
#' vec vector of character ID
#' prefix prefix to remove. Eihter "C" or "T"
#' @noRd
.trim_id <- function(vec, prefix) {
  checkmate::assertCharacter(prefix, pattern = "(C|T)")
  checkmate::assertChoice(prefix, choices = c("C", "T"))
  gsub(paste0("^", prefix), "", vec)
}


# take either String with "C1" or number. Always return C1
.cmpds_string_handler <- function(cmpds, multiple = FALSE) {
  if (is.numeric(cmpds)) {
    cmpds <- paste0("C", cmpds)
  }
  checkmate::assertString(cmpds, pattern = "C[0-9]+$")
  cmpds
}

# take either String with "T1" or number. Always return T1
.transition_string_handler <- function(trans, multiple = FALSE) {
  if (is.numeric(trans)) {
    trans <- paste0("T", trans)
  }
  checkmate::assertString(trans, pattern = "^T[0-9]+$")
  trans
}


#' @noRd
.compound_trans_df <- function(chrom_res) {
  cmpds <- chrom_res@compounds
  # join compound tabl with transitions
  cmpds <- dplyr::left_join(
    cmpds,
    chrom_res@transitions,
    by = "transition_id"
  ) |>
    dplyr::mutate(compound_trans = paste(.data$compound, round(.data$q3, 1))) |>
    dplyr::select("compound_id", "compound", "compound_trans", "transition_id")

  cmpds
}

#' @title Get compound ID from compound-transition label
#' @param cmpd_trans_df compound transition data frame
#' @param compound_trans compound transition label
#' @noRd
.get_compound_id_from_compound_trans <- function(
  cmpd_trans_df,
  compound_trans
) {
  checkmate::assertDataFrame(cmpd_trans_df)
  checkmate::assertString(compound_trans)
  compound_id <- cmpd_trans_df |>
    dplyr::filter(.data$compound_trans == !!compound_trans) |>
    dplyr::pull("compound_id")
  stopifnot(length(compound_id) == 1)
  compound_id
}

.get_compound_trans_from_compound_id <- function(cmpd_trans_df, compound_id) {
  checkmate::assertDataFrame(cmpd_trans_df)
  checkmate::assertString(compound_id)

  compound_trans_label <- cmpd_trans_df |>
    dplyr::filter(.data$compound_id == !!compound_id) |>
    dplyr::pull("compound_trans")
  stopifnot(length(compound_trans_label) == 1)
  compound_trans_label
}


#' @title Convert the peakareas to dataframe with last column sample type.
#' @return dataframe with columns compound_trans and area values
#' @noRd
chromres_to_matrix <- function(chrom_res, wide = FALSE) {
  x <- chrom_res@peaks |>
    dplyr::left_join(
      .compound_trans_df(chrom_res) |>
        select("compound_id", "compound_trans"),
      by = "compound_id"
    ) |>
    dplyr::mutate(area = round(.data$area, 2)) |>
    select("filename", "compound_trans", "area")

  if (wide) {
    x <- tidyr::pivot_wider(
      x,
      names_from = "compound_trans",
      values_from = "area"
    )
  }
  x
}


#' @title check if default expected RT  is set for a compound
#' @param chrom_res ChromRes object
#' @param compound_id Compound ID
#' @import checkmate
#' @import dplyr
#' @export
has_default_bounds <- function(chrom_res, compound_id) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertCount(compound_id, positive = TRUE)
  # compound_id <- .cmpds_string_handler(compound_id)

  compounds <- chrom_res@compounds
  compound_id_filter <- compound_id

  if (!(compound_id_filter %in% compounds$compound_id)) {
    stop("Compound ID not found in compounds")
  } else {
    res <- compounds |>
      dplyr::filter(.data$compound_id == compound_id_filter) |>
      dplyr::select("expected_peak_start", "expected_peak_end") |>
      stats::complete.cases()
  }
  res
}


#' @title Return an indicator if the chromatogram is smoothed
#' @param chrom_res ChromRes object
#'
#' @export
is_smoothed <- function(chrom_res) {
  res <- lapply(names(chrom_res@runs$files), function(x) {
    smoothed <- !is.null(chrom_res@runs$files[[x]]$smoothed)
    sample_id <- x
    data.frame(sample_id = sample_id, smoothed = smoothed)
  })

  do.call(rbind, res)
}


#' Override metadata dataframe
#' @param chrom_res ChromRes object
#' @param metadata data.frame
#'
#' This function is meant to run after the ChromRes object has been created.
#' The match occurs on the "filename" column.
#' Columns allowed to update are:
#' "subject_id", "sampling_time", "invitro_conc", "dose", "factor", "type"
#' @return ChromRes object
#' @noRd
update_metadata <- function(chrom_res, metadata, ignore_unmatched = TRUE) {
  checkmate::assertClass(chrom_res, "ChromResBase")
  checkmate::assertClass(metadata, "data.frame")

  # assert type clean
  stopifnot(
    metadata$type %in%
      c(
        "Sample",
        "QC",
        "Blank",
        "Standard",
        "DoubleBlank",
        "Suitability",
        "",
        NA
      )
  )

  chrom_res@metadata <- dplyr::rows_update(
    chrom_res@metadata,
    dplyr::select(
      metadata,
      dplyr::any_of(c(
        "filename",
        "subject_id",
        "sampling_time",
        "invitro_conc",
        "dose",
        "factor",
        "type",
        "dilution_factor"
      )),
      dplyr::starts_with("spiked_")
    ),
    by = "filename",
    unmatched = ifelse(ignore_unmatched, "ignore", "error")
  ) |>
    dplyr::mutate(dplyr::across(everything(), \(x) ifelse(x == "", NA, x)))

  # assert all spiked / dose/ sampling_time are numeric
  chrom_res@metadata <- chrom_res@metadata |>
    dplyr::mutate(across(starts_with("spiked_"), as.numeric)) |>
    dplyr::mutate(dose = as.numeric(.data$dose)) |>
    dplyr::mutate(sampling_time = as.numeric(.data$sampling_time)) |>
    dplyr::mutate(subject_id = as.character(.data$subject_id)) |>
    dplyr::mutate(type = as.character(.data$type)) |>
    dplyr::mutate(dilution_factor = as.numeric(.data$dilution_factor)) |>
    dplyr::mutate(factor = as.character(.data$factor)) |>
    dplyr::mutate(invitro_conc = as.numeric(.data$invitro_conc)) |>
    dplyr::mutate(dilution_factor = as.numeric(.data$dilution_factor))

  # refectch suitability
  # chrom_res <- prepare_suitability(chrom_res)

  ## linearity tab
  # chrom_res <- sync_linearity(chrom_res)
  # chrom_res <- prepare_suitability(chrom_res)

  validObject(chrom_res)
  chrom_res
}

list_compound_names <- function(chrom_res) {
  chrom_res@compounds$compound
}