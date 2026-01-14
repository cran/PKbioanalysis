#' @title Configure suitability runs
#' @description Configure suitability runs by specifying vial position and range of runs to include.
#' @param quantres QuantRes object
#' @param vial_pos Vial position to use for suitability (e.g., "2:H,9")
#' @param start Start position (1-based index) of runs to include. If NULL, starts from the first run.
#' @param end End position (1-based index) of runs to include. If NULL, ends at the last run.
#' @return Updated QuantRes object with suitability configuration.
config_suitability <- function(quantres, vial_pos, start = NULL, end = NULL) {
  checkmate::assertClass(quantres, "QuantRes")
  checkmate::assertNumber(
    start,
    lower = 1,
    upper = length(get_vials(quantres)),
    null.ok = TRUE
  )
  checkmate::assertNumber(
    end,
    lower = start,
    upper = length(get_vials(quantres)),
    null.ok = TRUE
  )

  # check if vial_pos is in metadata
  stopifnot(vial_pos %in% get_vials(quantres))

  if (is.null(start)) {
    start <- 1
  }
  if (is.null(end)) {
    end <- sum(vial_pos == get_vials(quantres))
  }

  # check enough runs are present
  if (sum(vial_pos == get_vials(quantres)) < 3) {
    stop("Selected vial has to be present in at least 3 runs.")
  }

  quantres@suitability$config <- list(
    vial = vial_pos,
    start_pos = start,
    end_pos = end
  )

  quantres
}

# return data.frame with included/excluded runs
prepare_suitability <- function(quantres) {
  # assert configuation is set
  if (!has_suitability_config(quantres)) {
    stop(
      "Suitability configuration not set. Please run config_suitability() first."
    )
  }

  config <- quantres@suitability$config
  vial_pos <- config$vial
  start_pos <- config$start_pos
  end_pos <- config$end_pos

  start_pos <- ifelse(is.null(start_pos), 1, start_pos)
  end_pos <- ifelse(
    is.null(end_pos),
    sum(vial_pos == get_vials(quantres)),
    end_pos
  )

  # get data
  res <- quantres_to_matrix(quantres, wide = FALSE, val = "abs_response") |>
    dplyr::left_join(
      quantres@samples_metadata |> dplyr::select("filename", "type", "vial"),
      by = "filename"
    ) |>
    dplyr::filter(.data$vial == !!vial_pos)

  files <- unique(res$filename)[start_pos:end_pos]

  res <- res |>
    dplyr::mutate(include = ifelse(.data$filename %in% files, TRUE, FALSE)) |>
    dplyr::mutate(across(starts_with("spiked_"), as.numeric)) |>
    dplyr::select("filename", "include", everything())

  quantres@suitability$suitabilitytab <- res
  quantres
}

run_suitability <- function(quantres) {
  stopifnot(has_suitability_config(quantres))

  # prepare suitability data
  quantres <- prepare_suitability(quantres)

  quantres@suitability$results <- quantres@suitability$suitabilitytab |>
    dplyr::filter(.data$include == TRUE) |>
    dplyr::select(-"filename", -"include", -"vial", -"type") |>
    dplyr::group_by(.data$compound) |>
    dplyr::summarize(RSD = cv(.data$abs_response), n = dplyr::n())

  quantres
}

plot_suitability <- function(quantres) {
  ggplot2::ggplot(
    quantres@suitability[["results"]],
    aes(y = .data$compound, x = .data$RSD, fill = .data$compound)
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(title = "RSD Plot", x = "Compound", y = "RSD%") +
    ggplot2::theme_minimal() +
    ggplot2::geom_label(
      aes(label = paste0(round(.data$RSD, 2), "%")),
      fill = "white",
      position = ggplot2::position_stack(vjust = 0.5)
    ) +
    ggplot2::theme(legend.position = "none")
}


plot_suitability_trend <- function(quantres) {
  df <- quantres@suitability$suitabilitytab |>
    tidyr::pivot_wider(names_from = "compound", values_from = "abs_response")

  rsd_values <- df[, 5:ncol(df)] # only cmpds
  rsd_list <- list()
  for (i in seq(nrow(df))) {
    # calulate RSD for each compound end at nrow(df), start at nrow(df) - i + 1
    new_df <- apply(rsd_values[(nrow(df) - i + 1):nrow(df), ], 2, cv)
    new_df <- as.data.frame(t(new_df))
    new_df$n <- i
    rsd_list[[i]] <- new_df
  }
  rsd_values <- do.call(rbind, rsd_list) |>
    tidyr::pivot_longer(cols = -"n", names_to = "compound", values_to = "RSD")

  ggplot2::ggplot(rsd_values, aes(x = .data$n, y = .data$RSD, color = .data$compound)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~compound, scales = "free") +
    ggplot2::labs(
      title = "RSD Trend Plot",
      x = "Number of Points",
      y = "RSD%"
    ) +
    ggplot2::scale_x_reverse() +
    ggplot2::theme_minimal()
}


#' Check if suitability configuraion is set
#' @noRd
has_suitability_config <- function(quantres) {
  checkmate::assertClass(quantres, "QuantRes")

  x <- quantres@suitability
  vial_l <- !is.na(x$config)

  stopifnot(length(vial_l) == 3)

  all(c(vial_l))
}


has_suitability_results <- function(quantres) {
  checkmate::assertClass(quantres, "QuantRes")

  x <- quantres@suitability
  res_l <- !is.null(x$results) && nrow(x$results) > 0

  res_l
}
