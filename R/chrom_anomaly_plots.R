#' Wrangle data for anomaly plots
#'@noRd
.vis_data <- function(chrom_res) {
  dat <- chrom_res@peaks |>
    dplyr::select(
      "sample_id",
      "filename",
      "sample_name",
      "sample_type",
      "area",
      "compound_id",
      "observed_peak_start",
      "observed_peak_end",
      "observed_rt"
    ) |>
    dplyr::mutate(sample_id = as.numeric(.data$sample_id)) |>
    # get compount name
    dplyr::left_join(
      chrom_res@compounds |> dplyr::select("compound_id", "compound", "transition_id"),
      by = "compound_id"
    ) |>
    # get type
    dplyr::left_join(
      chrom_res@metadata |> dplyr::select("filename", "type"),
      by = "filename"
    ) |>
    # get q3 from transitons tab
    dplyr::left_join(
      chrom_res@transitions |> dplyr::select("transition_id", "q3"),
      by = "transition_id"
    ) |>
    dplyr::mutate(compound_label = paste0(.data$compound, " (", round(.data$q3, 1), ")")) |>
    dplyr::mutate(split_id = paste0(.data$compound_id, "___split___", .data$filename))
  dat
}

#' Plotting RT intervals of chromatogram
#' @param chrom_res ChromRes object
plot_RT.ChromRes <- function(chrom_res) {
  dat <- .vis_data(chrom_res)
  # df: area vs filename vs compound
  res <- ggplot2::ggplot(
    dat,
    aes(y = .data$sample_id, x = .data$observed_rt, color = .data$type)
  ) +
    ggiraph::geom_point_interactive(
      aes(tooltip = .data$split_id, data_id = .data$split_id),
      stat = "identity",
      size = 0.7
    ) +
    ggplot2::theme_minimal() +
    ggiraph::geom_errorbarh_interactive(
      aes(
        data_id = .data$split_id,
        tooltip = .data$compound_id,
        xmin = .data$observed_peak_start,
        xmax = .data$observed_peak_end
      ),
      height = 0.1
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Peak Area", y = "") +
    ggplot2::facet_wrap(
      compound_label ~ .,
      drop = FALSE,
      ncol = 6,
      scales = "free"
    ) +
    theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, size = 5),
      strip.text = element_text(size = 4)
    )

  res
}


plot_area_bar.ChromRes <- function(chrom_res, log_scale = TRUE) {
  dat <- .vis_data(chrom_res)
  # df: area vs filename vs compound
  res <- ggplot2::ggplot(dat, aes(x = .data$sample_id, y = .data$area, fill = .data$type)) +
    ggiraph::geom_bar_interactive(
      aes(tooltip = paste0(.data$filename, .data$area, .data$type), data_id = .data$split_id),
      color = "white",
      stat = "identity",
      position = ggplot2::position_dodge(width = 0.001),
      width = 0.9
    ) +
    ggplot2::geom_bar(stat = "identity", width = 0.9) +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    # ggplot2::theme(legend.position = "none") +
    ggplot2::labs(y = "Peak Area", x = "") +
    facet_grid(compound_label ~ ., scales = "free") +
    theme(
      axis.text.y = ggplot2::element_text(angle = 10, size = 4),
      axis.text.x = ggplot2::element_blank(),
      strip.text = element_text(size = 4)
    )

  if (log_scale) {
    res <- res + ggplot2::scale_y_log10()
  }

  res
}

ggiraph_config1 <- function(res) {
  ggiraph::girafe(
    ggobj = res,
    options = list(
      ggiraph::opts_selection(
        type = "single",
        only_shiny = TRUE
      ),
      ggiraph::opts_zoom(min = 1, max = 5),
      ggiraph::opts_sizing(rescale = TRUE, width = 1)
    )
  )
}


plot_area_dot.ChromRes <- function(chrom_res, log_scale = TRUE) {
  dat <- .vis_data(chrom_res)
  # df: area vs filename vs compound
  res <- ggplot2::ggplot(dat, aes(y = .data$type, x = .data$area)) +
    ggiraph::geom_point_interactive(
      aes(tooltip = paste0(.data$filename, .data$area, .data$type), data_id = .data$split_id),
      shape = 21,
      size = 0.7,
      fill = "green"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = "Peak Area", y = "") +
    ggiraph::facet_wrap_interactive(
      ~compound_label,
      scales = "free_x",
      ncol = 4
    ) +
    theme(
      axis.text.y = ggplot2::element_text(angle = 15, size = 5),
      axis.text.x = ggplot2::element_text(angle = 10, size = 5),
      strip.text.x = element_text(size = 5)
    )

  if (log_scale) {
    res <- res + ggplot2::scale_x_log10()
  }

  res
}

plot_areas_heatmap <- function(chrom_res) {
  # filename, compound_label, type
  dat <- .vis_data(chrom_res)
  res <- ggplot(dat, aes(x = .data$sample_id, y = .data$compound_label, fill = .data$area)) +
    # ggplot2::geom_tile(lwd = 1.5, color = "white", fill = "red") +
    ggiraph::geom_tile_interactive(
      aes(tooltip = paste0(.data$filename, .data$area, .data$type), data_id = .data$split_id),
      color = "white",
      lwd = 0.6,
      linetype = 1
    ) +
    facet_grid(~type, scales = "free_x", space = "free_x") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank()
    )
  res
}
