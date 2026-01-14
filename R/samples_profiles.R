extract_pk_profiles <- function(chrom_res) {
  samples_conc <- list()
  for (i in chrom_res@compounds$compound_id) {
    # check if linearity calculated
    if (has_linearity(chrom_res, i)) {
      samples_conc[[i]] <- chrom_res@linearity[[i]]$linearitytab |>
        dplyr::filter(
          .data$type == "Sample" &
            !is.na(.data$sampling_time) &
            !is.na(.data$subject_id)
        ) |> # check if time and subject id present
        dplyr::mutate(estimated_conc_analytical = .data$estimated_conc) |>
        dplyr::mutate(
          estimated_conc = .data$estimated_conc * .data$dilution_factor
        ) |>
        dplyr::select(
          "filename",
          "sampling_time",
          "subject_id",
          "invitro_conc",
          "dosage",
          "factor",
          "estimated_conc_analytical",
          "estimated_conc",
          "dilution_factor"
        ) |>
        dplyr::mutate(dosage = ifelse(is.na(.data$dosage), ".", .data$dosage)) |>
        dplyr::mutate(compound_id =  i)

      if (nrow(samples_conc[[i]]) == 0) {
        message(paste0("No PK samples found for ", i))
        samples_conc[[i]] <- NA
      }
    } else {
      message(paste0("Linearity not calculated executed for ", i))
      samples_conc[[i]] <- NA
    }
  }
  chrom_res@pk_metadata <- samples_conc

  validObject(chrom_res)

  chrom_res
}

has_pk_profiles <- function(chrom_res, compound_id) {
  if (is.null(chrom_res@pk_metadata)) {
    return(FALSE)
  }

  if (is.null(chrom_res@pk_metadata[[compound_id]])) {
    return(FALSE)
  }

  return(TRUE)
}

plot_pk_profiles <- function(chrom_res, compound_id = NULL) {
  if (is.null(compound_id)) {
    data_to_plot <- do.call(rbind, chrom_res@pk_metadata) |>
      dplyr::filter(!is.na(.data$compound_id))
  } else {
    data_to_plot <- chrom_res@pk_metadata[[compound_id]]
  }

  if (is.null(data_to_plot) || nrow(data_to_plot) == 0) {
    stop("No data available to plot.")
  }

  p <- ggplot2::ggplot(
    data_to_plot,
    ggplot2::aes(x = .data$sampling_time, y = .data$estimated_conc, color = .data$subject_id)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "PK Profiles",
      x = "Sampling Time",
      y = "Estimated Concentration"
    ) +
    ggplot2::theme_minimal()

  p <- p +
    ggplot2::facet_wrap(
      compound_id ~ factor + dosage,
      ncol = 4,
      scales = "free"
    )
  ggiraph::girafe(
    ggobj = p,
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


export_pk_profiles <- function(format = "nonmem") {}


nca_table <- function(chrom_res) {
  # filter pk_metadata for only available data #FIXME clean up
  df <- chrom_res@pk_metadata[
    lapply(chrom_res@pk_metadata, \(x) !is.null(x) & nrow(x) > 0) |>
      unlist() |>
      names()
  ] |>
    dplyr::bind_rows()

  split(df, paste(df$compound_id, df$dosage, df$factor, sep = "_")) |>
    lapply(\(x) {
      pmxTools::get_auc(
        id = "subject_id",
        time = "sampling_time",
        dv = "estimated_conc",
        data = x
      )
    })
}
