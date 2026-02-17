#' @include class.R generics.R

check_quantRes <- function(object) {
  ## check quanttab
  lapply(object@quanttab, function(x) {
    checkmate::assertNames(
      names(x),
      identical.to = c(
        "filename",
        "vial",
        "type",
        "stdconc",
        "compound",
        "area",
        "height",
        "peak_start",
        "peak_end",
        "SN",
        "IS_name",
        "RT", 
        "injec_id"
      )
    )
  })

  ## check suitability
  checkmate::assertList(object@suitability)
  checkmate::assertNames(
    names(object@suitability),
    must.include = c("config", "results")
  )

  checkmate::assertNames(
    names(object@suitability$config),
    identical.to = c("vial", "start_pos", "end_pos")
  )

  ##check linearity
  stopifnot(length(object@linearity) == length(object@quanttab))

  lapply(object@linearity, function(x) {
    checkmate::assertList(x)
    checkmate::assertNames(
      names(x),
      identical.to = c("linearitytab", "results")
    )
    stopifnot(length(x) == 2)
    checkmate::assertDataFrame(x$linearitytab)
    checkmate::assertNames(
      names(x$linearitytab),
      must.include = c(
        "filename",
        "type",
        "abs_response",
        "rel_response",
        "stdconc",
        "include",
        "dev",
        "estimated_conc"
      )
    )
  })

  ## check samples_metadata
  checkmate::assertDataFrame(object@samples_metadata)
  checkmate::assertNames(
    names(object@samples_metadata),
    must.include = c("filename", "vial", "type", "injec_id")
  )

  lapply(object@quanttab, function(x) {
    stopifnot(nrow(x) == nrow(object@samples_metadata))
  })

  ## check compounds_metadata
  checkmate::assertDataFrame(object@compounds_metadata)
  checkmate::assertNames(
    names(object@compounds_metadata),
    must.include = c("compound")
  )
}


setValidity("QuantRes", function(object) {
  check_quantRes(object)
  TRUE
})

check_quant_method_quantres <- function(quantobj, method_id) {
  # check that all compounds in quantobj are in method_id
  cmpds_metadata <- .get_method_cmpds(method_id)
  if (nrow(cmpds_metadata) == 0) {
    stop("No compounds found for the given method_id")
  }
  missing_cmpds <- setdiff(names(quantobj@quanttab), cmpds_metadata$compound)
  if (length(missing_cmpds) > 0) {
    stop(paste0(
      "The following compounds are not in the method_id ",
      method_id,
      ": ",
      paste(missing_cmpds, collapse = ", ")
    ))
  }
  TRUE
}

create_quant_object <- function(df, method_id = NULL) {
  checkmate::assertDataFrame(df)
  checkmate::assertIntegerish(method_id, null.ok = TRUE, len = 1)

  # df names
  # filename, compound_name, area, height, peak_start, peak_end, IS_name
  checkmate::assertNames(
    names(df),
    must.include = c(
      "filename",
      "vial",
      "type",
      "stdconc",
      "compound",
      "area",
      "height",
      "peak_start",
      "peak_end",
      "SN",
      "IS_name",
      "RT", 
      "injec_id"
    ),
    type = "unique"
  )

  quantlist <- df |>
    dplyr::select(
      "filename",
      "vial",
      "type",
      "stdconc",
      "compound",
      "area",
      "height",
      "peak_start",
      "peak_end",
      "SN",
      "IS_name",
      "RT", 
      "injec_id"
    ) |>
    split(f = df$compound)

  # create linearity list
  linearitylist <- .construct_linearity(quantlist)
  suitabilitylist <- .construct_suitability(quantlist)
  resEstimlist <- .construct_resEstim(quantlist)
  pklist <- .construct_pkdata(quantlist)

  cmpd_metadata <- data.frame(compound = names(quantlist))
  res <- new(
    "QuantRes",
    samples_metadata = .construct_samples_metadata(quantlist),
    compounds_metadata = cmpd_metadata,
    quanttab = quantlist,
    linearity = linearitylist,
    suitability = suitabilitylist,
    resEstim = resEstimlist,
    pkdata = pklist
  )
  if (!is.null(method_id)) {
    res <- update_IS_info(res, method_id)
    res <- update_cmpd_info(res, method_id)
    res <- update_rel_response(res)
  }

  validObject(res)
  res
}


.construct_samples_metadata <- function(quantlist) {
  quantlist[[1]] |> 
    dplyr::select("filename", "vial", "type", "injec_id") |> 
    distinct() 
}

.construct_linearity <- function(quantlist) {
  # create a list with names compound_id

  linearity <- list()
  linearity <- lapply(quantlist, function(x) {
    spiked_name <- paste0("spiked_", unique(x$compound))
    linearitytab <- data.frame(
      filename = x$filename,
      type = x$type,
      abs_response = x$area,
      rel_response = NA,
      stdconc = x$stdconc,
      include = TRUE,
      dev = as.numeric(NA),
      estimated_conc = as.numeric(NA)
    )
    results <- NA
    list(
      linearitytab = linearitytab,
      results = results
    )
  })
  linearity
}

.construct_suitability <- function(quantlist) {
  list(
    config = list(vial = NA, start_pos = NA, end_pos = NA),
    results = data.frame()
  )
}

.construct_resEstim <- function(quantlist) {
  resEstim <- lapply(quantlist, function(x) {
    data.frame(
      error = c("additive", "proportional"),
      value = as.numeric(NA),
      rse = as.numeric(NA)
    )
  })
  resEstim
}

.construct_pkdata <- function(quantlist) {
  # for each compound, make empty dataframe with id, time, conc, filename, sex, dose, arm
  pkdata <- lapply(quantlist, function(x) {
    NA
  })
  pkdata
}

update_IS_info <- function(quantres, method_id) {
  checkmate::assertClass(quantres, "QuantRes")
  check_quant_method_quantres(quantres, method_id)

  cmpd_metadata <- .get_method_cmpds(method_id)

  quantres@quanttab <- lapply(quantres@quanttab, function(x) {
    IS_name <- cmpd_metadata$compound[match(
      x$compound,
      cmpd_metadata$compound
    )]
    x$IS_name <- IS_name
    x
  })

  validObject(quantres)
  quantres
}

update_cmpd_info <- function(quantres, method_id) {
  checkmate::assertClass(quantres, "QuantRes")
  check_quant_method_quantres(quantres, method_id)

  cmpd_metadata <- .get_method_cmpds(method_id)
  quantres@compounds_metadata <- cmpd_metadata
  validObject(quantres)
  quantres
}


has_IS <- function(quantres, compound_id) {
  cmpd_md <- quantres@compounds_metadata

  IS_id <- get_IS(quantres, compound_id)
  IS_name <- get_IS_name(quantres, IS_id)

  # non-NA IS_id and also exists among compounds
  if (!is.na(IS_id)) {
    stopifnot(IS_name %in% cmpd_md$compound)
    TRUE
  } else {
    FALSE
  }
}

get_IS <- function(quantres, compound_id) {
  cmpd_md <- quantres@compounds_metadata
  # Check if compound_id exists in compounds_metadata
  if (!compound_id %in% cmpd_md$compound) {
    return(FALSE)
  }
  if (is.null(cmpd_md$IS_id)) {
    return(FALSE)
  }
  # Get IS_id for the given compound_id
  IS_id <- cmpd_md$IS_id[cmpd_md$compound == compound_id]
  IS_id
}

get_IS_name <- function(quantres, IS_id) {
  cmpd_md <- quantres@compounds_metadata
  IS_name <- cmpd_md$compound[cmpd_md$compound_id == IS_id]
  IS_name
}


get_vials.QuantRes <- function(x) {
  # assume all compounds have same vials
  lapply(x@quanttab[1], function(y) y$vial) |> unlist() |> unname()
}

setMethod("get_vials", signature(x = "QuantRes"), get_vials.QuantRes)


#' @title Convert the quantres to dataframe with last column sample type.
#' @return dataframe with columns compound_trans and area values
#' @noRd
quantres_to_matrix <- function(quantres, wide = FALSE, val = "abs_response") {
  if (val == "conc") {
    haslin <- lapply(names(quantres@linearity), function(x) {
      has_linearity(quantres, x)
    }) |>
      unlist()

    if (sum(haslin) == 0) {
      stop(
        "No single compound has linearity table. Please run run_linearity() first."
      )
    }
  } else if (val == "abs_response") {
    haslin <- rep(TRUE, length(quantres@linearity))
  } else {
    stop("val must be either 'conc' or 'abs_response'")
  }

  x <- lapply(names(quantres@linearity[haslin]), function(x) {
    x <- quantres@linearity[[x]]$linearitytab |>
      mutate(compound = x) |>
      select("filename", "compound", all_of(val))
  })
  x <- do.call(rbind, x)

  if (wide) {
    x <- tidyr::pivot_wider(
      x,
      names_from = "compound",
      values_from = all_of(val)
    )
  }
  x
}

current_cmpds <- function(quantres) {
  names(quantres@quanttab)
}

derive_rel_response <- function(quantres, compound_id) {
  # stop if has_IS is false
  if (!has_IS(quantres, compound_id)) {
    stop(
      "No internal standard (IS) found for this compound. Update Method first"
    )
  }

  IS_id <- get_IS(quantres, compound_id)
  IS_name <- get_IS_name(quantres, IS_id)

  cmpd_tab <- quantres@quanttab[[compound_id]]
  IS_tab <- quantres@quanttab[[IS_name]]

  # Check for NA areas
  if (all(is.na(cmpd_tab$area))) {
    stop("All area values for the compound are NA.")
  }
  if (all(is.na(IS_tab$area))) {
    stop("All area values for the IS are NA.")
  }

  # Calculate relative response
  rel_response <- cmpd_tab$area / IS_tab$area

  rel_response
}

update_rel_response <- function(quantres) {
  linearitylist <- quantres@linearity
  res <- lapply(names(linearitylist), function(x) {
    currlintab <- linearitylist[[x]]
    if (has_IS(quantres, x)) {
      rel_response <- derive_rel_response(quantres, x)
      currlintab$linearitytab$rel_response <- rel_response
    } else {
      currlintab$linearitytab$rel_response <- NA
    }
    list(
      linearitytab = currlintab[["linearitytab"]],
      results = currlintab[["results"]]
    )
  })
  names(res) <- names(quantres@linearity)
  quantres@linearity <- res

  validObject(quantres)
  quantres
}


prefilter_precision_data.QuantRes <- function(
  x,
  type,
  acc_cutoff = 0.2,
  dev_cutoff = 0.2,
  compound_id
) {
  stopifnot(inherits(x, "QuantRes"))
  stopifnot(type %in% c("QC", "DQC", "Standard"))
  checkmate::assertString(compound_id)
  stopifnot(has_linearity(x, compound_id))

  df <- x@linearity[[compound_id]]$linearitytab |>
    dplyr::rename(conc = "estimated_conc") |>
    dplyr::select("conc", "stdconc", "type")

  prefilter_precision_data(df, type = type, acc_cutoff = acc_cutoff, dev_cutoff = dev_cutoff)
}

#' @rdname prefilter_precision_data
#' @export
setMethod(
  "prefilter_precision_data",
  signature(x = "QuantRes"),
  prefilter_precision_data.QuantRes
)

prefilter_precision_data.data.frame <- function(x, type, acc_cutoff = 0.2, dev_cutoff = 0.2) {
  stopifnot(is.data.frame(x))
  stopifnot(type %in% c("QC", "DQC", "Standard"))
  checkmate::assertNames(
    colnames(x),
    must.include = c("conc", "stdconc", "type")
  )
  checkmate::assertNumeric(acc_cutoff, lower = 0, upper = 1)
  checkmate::assertNumeric(dev_cutoff, lower = 0, upper = 1)
  checkmate::assertChoice(type, choices = c("Standard", "QC", "DQC"))
  x |>
    dplyr::filter(.data$type == .env$type) |>
    dplyr::filter(dplyr::between(accuracy(.data$conc, .data$stdconc, percent = FALSE), 1 - acc_cutoff, 1 + acc_cutoff)) |>
    dplyr::filter(dplyr::between(rel_deviation(.data$conc, .data$stdconc, percent = FALSE), -dev_cutoff, dev_cutoff))
}

#' @rdname prefilter_precision_data
#' @export
setMethod(
  "prefilter_precision_data",
  "data.frame",
  prefilter_precision_data.data.frame
)

#' Save QuantRes object to cache directory
#'
#' @param x QuantRes object
#' @param name Name for the saved file (without extension)
#' @return Path to the saved file (in cache dir)
#' @noRd
save_quant_object <- function(x, name) {
  stopifnot(inherits(x, "QuantRes"))
  checkmate::assertString(name)
  cache_dir <- get_pkbioanalysis_option("quant_cache_dir")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  save_path <- file.path(cache_dir, paste0(name, ".rds"))
  saveRDS(x, save_path)
  invisible(save_path)
}

lst_quant_cache_files <- function() {
  cache_dir <- get_pkbioanalysis_option("quant_cache_dir")
  if (!dir.exists(cache_dir)) return(character(0))
  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(character(0))
  file_info <- file.info(files)
  files_sorted <- files[order(file_info$mtime, decreasing = TRUE)]

  tools::file_path_sans_ext(basename(files_sorted))
}

#' Load QuantRes object from cache directory
#'
#' @param name Name of the saved file (without extension)
#' @return Loaded QuantRes object
#' @noRd
load_quant_object <- function(name) {
  checkmate::assertString(name)
  cache_dir <- get_pkbioanalysis_option("quant_cache_dir")
  file_path <- file.path(cache_dir, paste0(name, ".rds"))
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  readRDS(file_path)
}