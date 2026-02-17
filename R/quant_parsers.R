#' Class constructor to PeaksRes
#' @param list list with compound name
#' @noRd
.peak_res <- function(list, vendor) {
  structure(list(res = list, vendor = vendor), class = "PeakRes")
}


#' Read experiment results
#' @param x path to experiment results. See details
#' @param drop_prefix logical. If TRUE, drop the prefix from the sample name
#' @param vendor vendor name. Currently only "targetlynx_xml" or "targetlynx_csv" are supported.
#' @param logkey character. The column name in the targetlynx CSV file that contains the injection sequence or log key. Default is "Index" which is the default column in targetlynx CSV exports, but it can be customized if the user has a different column name for the injection sequence.
#' @details  
#' Currently only targetlynx XML or CSV exported files are supported.
#' @return QuantRes object with the results of the experiment.
#' @export
read_experiment_results <- function(
  x,
  drop_prefix = FALSE,
  vendor = "targetlynx_xml", 
  logkey = "Index"
) {
  checkmate::assertFileExists(x)
  checkmate::assertChoice(
    vendor,
    choices = c("targetlynx_xml", "targetlynx_csv", "generic")
  )

  if (vendor == "targetlynx_xml") {
    stopifnot(grepl(".xml$", x))
    quantobj <- .parse_tlynx_xml(x, drop_prefix = drop_prefix)
  } else if (vendor == "targetlynx_csv") {
    stopifnot(grepl("\\.(csv|txt)$", x, ignore.case = TRUE))

    dat <- .parse_tlynx_csv(x)
    quantobj <- lapply(names(dat$res), function(y) {
      dat$res[[y]]$compound <- y
      dat$res[[y]]
    })
    quantobj <- do.call("rbind", quantobj)

    quantobj <- quantobj |>
      dplyr::rename(filename = "Name") |>
      dplyr::rename(vial = "Vial") |>
      dplyr::rename(type = "Type") |>
      # dplyr::rename(height = "PEAK_height") |>
      # dplyr::rename(peak_start = "PEAK_startrt") |>
      # dplyr::rename(peak_end = "PEAK_endrt") |>
      dplyr::rename(SN = "S/N") |>
      dplyr::mutate(height = NA) |>
      dplyr::mutate(peak_start = NA) |>
      dplyr::mutate(peak_end = NA) |>
      dplyr::mutate(IS_name = NA) |>
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
        logkey
      ) |>
      dplyr::mutate(across(
        c("stdconc", "area", "height", "peak_start", "peak_end", "SN", "RT"),
        as.numeric
      )) |> 
      dplyr::rename(injec_id = !!sym(logkey))
  } else if (vendor == "generic") {
    stop("Vendor not supported")
  } else {
    stop("Vendor not supported")
  }
  quantobj
}

# create list of compounds with
# Compound_id, abs_response, rel_response
peakres_to_chromres <- function(peakres, method = NA) {
  metadata_df <- lapply(split(peakres$res, peakres$res$cmpd_id), \(x) {
    x |>
      select(
        "sample_id",
        "sample_name",
        "sample_type",
        "sample_dilutionfac",
        "sample_createdate",
        "sample_createtime",
        "cmpd_id",
        "cmpd_name",
        "PEAK_area",
        "ISPEAK_area",
        "PEAK_chromtrace"
      ) |>
      mutate(rel_response = .data$PEAK_area / .data$ISPEAK_area)
  })

  metadata_df <- peakres$res |>
    select(
      "sample_id",
      "sample_name",
      "sample_type",
      "sample_dilutionfac",
      "sample_createdate",
      "sample_createtime",
      "sample_vial",
      "sample_injectvolume"
    ) |>
    rename(
      type = "sample_type",
      dilution_factor = "sample_dilutionfac",
      filename = "sample_name",
      sample_location = "sample_vial"
    ) |>
    mutate(vialpos = .data$sample_location) |> # FIXME vialpos or sample_location to avoid confusion
    mutate(run_time = as.numeric(NA)) |>
    mutate(injection_mode = as.character(NA)) |>
    mutate(column_type = as.character(NA)) |>
    mutate(column_serial_number = as.character(NA)) |>
    mutate(vendor = "targetlynx") |>
    mutate(instrument = as.character(NA)) |>

    mutate(std_rep = as.character(NA)) |>
    mutate(inj_vol = "sample_injectvolume") |>
    mutate(dilution_factor = as.numeric(.data$dilution_factor)) |>
    mutate(sample_id = as.character(.data$sample_id)) |>
    mutate(subject_id = as.character(NA)) |>
    mutate(sampling_time = as.numeric(NA)) |>
    mutate(invitro_conc = as.numeric(NA)) |>
    mutate(factor = as.character(NA)) |>
    mutate(dose = as.numeric(NA)) |>
    mutate(date = paste0(.data$sample_createdate, " ", .data$sample_createtime)) |>
    distinct()

  transitions_df <- do.call(
    rbind,
    lapply(split(peakres$res, peakres$res$cmpd_id), \(x) {
      x |> select("PEAK_chromtrace") |> unique()
    })
  ) |>
    dplyr::filter(.data$PEAK_chromtrace != "") |>
    tidyr::separate_wider_delim("PEAK_chromtrace", names = c("q1", "q3"), delim = ">") |>
    mutate(transition_id = paste0("T", row_number())) |>
    mutate(method_id = method) |>
    select("transition_id", "q1", "q3")

  transitions_df <- .construct_experiment_transitions(transitions_df, method)

  compounds_df <- .construct_experiment_compounds(method, transitions_df)

  res <- list(
    runs = NA,
    metadata = metadata_df,
    exp_transitions = transitions_df,
    exp_compounds = compounds_df
  )

  res <- .construct_experiment_peaktab(res)

  res <- .construct_suitability(res)

  res <- .construct_linearity(res)

  res <- .construct_pk_metadata(res)

  chrom_res <- new(
    "ChromResBase",
    metadata = res$metadata,
    peaks = res$exp_peaktab,
    transitions = res$exp_transitions,
    compounds = res$exp_compounds,
    linearity = res$linearity,
    pk_metadata = res$pk_metadata,
    suitability = res$suitability,
    vendor = "targetlynx"
  )

  # filename + compound + area
  # update_peak_external(chrom_res)

  cmpds_trans_df <- .compound_trans_df(chrom_res)

  int_area <- peakres$res |>
    select(
      "sample_name",
      "PEAK_area",
      "PEAK_height",
      "PEAK_startrt",
      "PEAK_endrt",
      "PEAK_foundrt",
      "PEAK_foundrt",
      "PEAK_startrt",
      "PEAK_endrt",
      "PEAK_height",
      "PEAK_chromtrace",
      "cmpd_name"
    ) |>
    mutate(across(
      c("PEAK_area", "PEAK_foundrt", "PEAK_startrt", "PEAK_endrt", "PEAK_height"),
      as.numeric
    )) |>
    filter(.data$PEAK_chromtrace != "") |>
    tidyr::separate_wider_delim("PEAK_chromtrace", names = c("q1", "q3"), delim = ">") |>
    mutate(compound_trans = paste(.data$cmpd_name, round(as.numeric(.data$q3), 1))) |>
    left_join(cmpds_trans_df, by = "compound_trans") |>

    rename(
      filename = "sample_name",
      area = "PEAK_area",
      observed_rt = "PEAK_foundrt",
      observed_peak_start = "PEAK_startrt",
      observed_peak_end = "PEAK_endrt",
      observed_peak_height = "PEAK_height"
    ) |>
    select(
      "filename",
      "area",
      "compound_id",
      "observed_peak_start",
      "observed_peak_end",
      "observed_peak_height",
      "observed_rt"
    ) |>
    filter(!is.na(.data$compound_id))

  if (nrow(int_area) == 0) {
    stop(
      "No peaks found. Possibly compound name mismatch from file and database"
    )
  }

  chrom_res@peaks <- rows_update(
    chrom_res@peaks,
    int_area,
    by = c("filename", "compound_id")
  )

  validObject(chrom_res)
  chrom_res
}


#' Utility to extract named xml child to dataframe and rename
#'
#' @param xmltree xml tree
#' @param tag xml tag to extract
#' @keywords internal
.child_to_df <- function(xmltree, tag) {
  if (tag == "ISPEAK") {
    df <- xml2::xml_child(xmltree, tag) |>
      (\(x) tidyr::pivot_wider(enframe(xml2::xml_attrs(x)[[1]])))()
  } else {
    df <- xml2::xml_child(xmltree, tag) |>
      (\(x) tidyr::pivot_wider(enframe(xml2::xml_attrs(x))))()
  }
  colnames(df) <- paste0(tag, "_", colnames(df))
  df
}


#' Parse targetlynx
#'
#' Peaks must be integrated and checked
#' @param xmlpath xml targetlynx output
#' @param drop_prefix logical. If TRUE, drop the prefix from the sample name
#' @return A list of Dataframe with each compound
#' @keywords internal
.parse_tlynx_xml <- function(xmlpath, drop_prefix = FALSE) {
  checkmate::assertLogical(drop_prefix)
  # assert ending xml
  if (!grepl(".xml$", xmlpath)) {
    stop("Targetlynx file must be xml")
  }

  xmlSPL <- xml2::read_xml(xmlpath) |> xml2::xml_find_all("//SAMPLELISTDATA")

  x <- list()

  for (spl in xmlSPL) {
    # main
    maintmp <- tidyr::pivot_wider(tibble::enframe(xml2::xml_attrs(spl)))
    colnames(maintmp) <- paste0("main_", colnames(maintmp))

    for (i in xml2::xml_children(spl)) {
      #samples
      if (length(xml2::xml_attrs(i)) == 2) {
        next
      }
      spltmp <- tidyr::pivot_wider(tibble::enframe(xml2::xml_attrs(i)))
      colnames(spltmp) <- paste0("sample_", colnames(spltmp))

      for (ii in xml2::xml_children(i)) {
        # compounds
        if (xml2::xml_name(ii) == "COMPOUND") {
          cmptmp <- tidyr::pivot_wider(tibble::enframe(xml2::xml_attrs(ii)))
          colnames(cmptmp) <- paste0("cmpd_", colnames(cmptmp))

          compoundstree <- xml2::xml_children(ii) # individual compounds
          # PEAK -> IS
          # METHOD
          # USERDATA
          peakdf <- .child_to_df(ii, "PEAK")
          ispeakdf <- .child_to_df(compoundstree, "ISPEAK")
          methodf <- .child_to_df(ii, "METHOD")
          userdatadf <- .child_to_df(ii, "USERDATA")
          tmp <- cbind(maintmp, spltmp, cmptmp, peakdf, ispeakdf, userdatadf)

          if (drop_prefix) {
            tmp$sample_name <- gsub(pattern = "^.*?_", "", x = tmp$sample_name)
          }
          x <- append(x, list(tmp))
        }
      }
    }
  }

  .peak_res(
    dplyr::bind_rows(x) |>
      mutate(across(
        c(
          "PEAK_startrt",
          "PEAK_endrt",
          "PEAK_foundrt",
          "PEAK_area",
          "ISPEAK_area",
          "PEAK_foundscan"
        ),
        as.numeric
      )) |>
      mutate(area_ratio = normalizeIS(.data$PEAK_area, .data$ISPEAK_area)) |>
      mutate(
        sample_type = case_when(
          .data$sample_type == "Analyte" ~ "Sample",
          .default = .data$sample_type
        )
      ),
    vendor = "targetlynx"
  )
}


.peakresToDF <- function(peak_res) {
  checkmate::assertClass(peak_res, "PeakRes")

  # sample name, peak_start, peak_end, compound, transition
  if (peak_res$vendor == "targetlynxXML") {
    # peak_res$res$cmpd_name
    # peak_res$res$PEAK_chromtrace
    data.frame(
      filename = peak_res$res$sample_name,
      peak_start = peak_res$res$PEAK_startrt,
      peak_end = peak_res$res$PEAK_endrt,
      compound = peak_res$res$cmpd_name,
      peak_area = peak_res$res$PEAK_area,
      is_area = peak_res$res$ISPEAK_area,
      stdconc = peak_res$res$sample_stdconc,
      sample_vial = peak_res$res$sample_vial,
      peak_area_ratio = peak_res$res$area_ratio,
      transition = peak_res$res$PEAK_chromtrace
    ) |>
      tidyr::separate_wider_delim("transition", names = c("q1", "q3"), delim = ">")
  } else if (peak_res$vendor == "targetlynxCSV") {
    stop("Not implemented yet")
  } else {
    stop("No know vendor")
  }
}

#' Parse targetlynx CSV file
#' @param filepath path to the targetlynx CSV file
#' @param first_cmpd name of the first compound, if not provided it will be extracted from the file
#' @return a list of data frames, each containing the data for a compound
#' @keywords internal
.parse_tlynx_csv <- function(filepath, first_cmpd) {
  mylist <- list()

  # extract compound name from the first 5 lines
  x <- readLines(filepath, n = 5)

  first_cmpd <- gsub("Compound \\d+:\\s+", "", x[5])
  x <- utils::read.delim(
    filepath,
    skip = 5,
    sep = "\t",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(x)[1] <- "cmpd"

  # Find which rows in first column starts with "Compound"
  compound_rows <- which(grepl("^Compound", x[[1]]))
  if (length(compound_rows) == 1) {
    message("2 compounds detected") # first compound skipped in header (first_cmpd)
  }

  # add first cmpd starts from 1 till the first compound row
  mylist[[1]] <- x[1:(compound_rows[1] - 1), ]

  # Split the data frame into a list of data frames from first row till before the next "Compound" row
  # the first row after the compound index is the header

  for (i in 1:(length(compound_rows) - 1)) {
    # delete first redundant row
    mylist[[i + 1]] <- x[(compound_rows[i] + 1):(compound_rows[i + 1] - 1), ] |>
      filter(row_number() != 1)
  }

  # Add the last compound data frame
  mylist[[length(compound_rows) + 1]] <- x[
    (compound_rows[length(compound_rows)] + 1):nrow(x),
  ] |>
    filter(row_number() != 1)

  # get compund names
  compound_names <- gsub("Compound \\d+:\\s+", "", x[[1]][compound_rows])
  compound_names <- c(first_cmpd, compound_names)

  # name the list
  names(mylist) <- compound_names
  reslist <- lapply(mylist, function(x) {
    x |>
      dplyr::rename(conc = "Conc.") |>
      dplyr::rename(stdconc = "Std. Conc") |>
      dplyr::rename(area = "Area") |>
      dplyr::rename(area_ratio = "Area Ratio") |>
      dplyr::mutate(across(
        c("conc", "area_ratio", "stdconc", "area", "%Dev"),
        as.numeric
      )) |>
      dplyr::mutate(accuracy = accuracy(.data$conc, .data$stdconc))
  })

  .peak_res(reslist, vendor = "targetlynx")
}
