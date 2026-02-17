#' Merge PK profiles into QuantRes object
#' @param x QuantRes object
#' @return QuantRes object with PK profiles merged into pkdata slot 
#' @export
#' @author Omar I. Elashkar
pkmerge <- function(x){
    checkmate::assertClass(x, "QuantRes")

  linbool <- lapply(names(x@linearity), \(y) has_linearity(x, y))
  if(sum(unlist(linbool)) < 1){
    stop("Linearity must be calculated for at least single compound before extracting PK profiles.")}

  
  # assert length and names of pkdata
  injec_ids <- x@samples_metadata |> 
    dplyr::filter(.data$type == "Analyte") |>
    dplyr::pull("injec_id") |>
    unique() 
  
  if(length(injec_ids) == 0){
    stop("No samples with valid ID matched. Please check the sample metadata.")
  }
  
  samplemetadf <- retrieve_log_by_injecid(injec_ids) 

  # left join file name 
  samplemetadf <- samplemetadf |> 
    dplyr::left_join(x@samples_metadata |> 
      dplyr::select("filename", "injec_id"),
    by = c("injec_id" = "injec_id"))


  # assert filename and log id are unique combination
  if(any(duplicated(samplemetadf$filename))){
      stop("Filename and injec_id combination must be unique. Please check the sample metadata.")
  } 

  # join sample metadata with log metadata to get dosing info and sampling time 
  res <- lapply(names(x@linearity), \(i) {
    if(has_linearity(x, i)){
      samplemetadf |> 
        dplyr::left_join(x@linearity[[i]]$linearitytab |> 
          dplyr::select("filename", "estimated_conc"),
        by = c("filename" = "filename")) |>
        dplyr::mutate(compound_id = i) |> 
        dplyr::rename(conc = "estimated_conc") |> 
        dplyr::mutate(conc = .data$conc * .data$dil) |>
        dplyr::mutate(nominal_time = as.numeric(.data$nominal_time))

    } else {
      x@pkdata[[i]]
    }
  }) 
  names(res) <- names(x@linearity)
  x@pkdata <- res
  
  # split by compound id
  validObject(x)
  x
}

extract_pk_profiles <- function(x) {
  checkmate::assert_class(x, "QuantRes")
  samples_conc <- list()
  for (i in x@compounds$compound_id) {
    # check if linearity calculated
    if (has_linearity(x, i)) {
      samples_conc[[i]] <- x@linearity[[i]]$linearitytab |>
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
  x@pk_metadata <- samples_conc

  validObject(x)

  x
}

has_pk_profiles <- function(x, compound_id) {
  checkmate::assert_class(x, "QuantRes")
  if (!compound_id %in% names(x@pkdata)) {
    stop(paste0("Compound ID ", compound_id, " not found in pkdata."))
  }
  !is.null(x@pkdata[[compound_id]]) && inherits(x@pkdata[[compound_id]], "data.frame") 
}

#' Plot PK profiles for a given compound
#' @param x QuantRes object with PK profiles extracted
#' @param compound_id Compound ID for which to plot PK profiles. If NULL, plots all compounds with PK profiles.
#' @param stratify_by variable to stratify the plots by. Can be one of "group_label", "route", "extra_factors", "subject_id
#' or "sex". If NA, no stratification is applied. Default is NA.
#' @param shape variable to use for point shapes. Can be one of "dil
#' or any other column in the PK data. If NA, no shapes are applied. Default is "dil".
#' @details This function plots PK profiles for a given compound. It uses ggplot2 for plotting and ggiraph for interactivity. The x-axis is the nominal sampling time and the y-axis is the concentration. The lines are colored by subject ID. If stratify_by is specified, the plots are faceted by the specified variable. If shape is specified, points are shaped by the specified variable.
#' @noRd
plot_pk_profiles <- function(x, compound_id= NULL, stratify_by = NA, shape = "dil") {
  checkmate::assert_class(x, "QuantRes")
  checkmate::assertChoice(stratify_by, c("group_label", "route", "extra_factors", "subject_id", "sex", NA_character_))
  checkmate::assertChoice(shape, c("dil", NA_character_))

  
  if (is.null(compound_id)) {
    ## check at least one compound has pk profiles 
    activeCompounds <- names(x@pkdata)[sapply(x@pkdata, function(df) inherits(df, "data.frame"))]
    if (length(activeCompounds) == 0){
      stop("No PK profiles available to plot.")
    } 
    data_to_plot <- do.call(rbind, x@pkdata[activeCompounds])
  } else {
    data_to_plot <- x@pkdata[[compound_id]]
  }
  data_to_plot <- data_to_plot |> 
    dplyr::mutate(dil = factor(.data$dil, 
        levels = sort(unique(.data$dil)), 
      labels = paste0(sort(unique(.data$dil)), "X"))) 

  if (is.null(data_to_plot) || nrow(data_to_plot) == 0) {
    stop("No data available to plot.")
  }

  p <- ggplot2::ggplot(
    data_to_plot,
    ggplot2::aes(x = .data$nominal_time, y = .data$conc, color = .data$subject_id)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data$subject_id),
      linewidth = 1
    ) +
    (if (!is.na(shape) && shape %in% colnames(data_to_plot)) {
      ggplot2::geom_point(
        ggplot2::aes(shape = .data[[shape]]),
        size = 2
      )
    } else {
      ggplot2::geom_point(size = 2)
    }) +
    ggplot2::labs(
      title = "PK Profiles",
      x = "Nominal Sampling Time",
      y = "Concentration"
    ) +
    ggplot2::theme_minimal()

  if (!is.na(stratify_by)) {
    p <- p +
      ggplot2::facet_wrap(
        as.formula(paste("compound_id~", stratify_by)),
        ncol = 4,
        scales = "free"
      )
  } else{ 
    p <- p + ggplot2::facet_wrap(~compound_id, ncol = 4, scales = "free")
  }

  p <- p + ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  ) + labs(color = "Subject ID", shape = ifelse(shape == "dil", "Dilution Factor", shape))

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


#' Export PK profiles for a given compound in a specified format
#' Currently supports "nonmem" format. The exported file will include a CSV with the PK data and an Excel file with the codebook.
#' @param x QuantRes object
#' @param compound_id Compound ID for which to export PK profiles
#' @param format Format to export (currently only "NONMEM" supported)
#' @param filename Name of the output zip file (default: "data.zip")
#' @author Omar I. Elashkar
#' @export
export_pk_profiles <- function(x, compound_id, format = "NONMEM", filename = "data.zip") {
  checkmate::assertClass(x, "QuantRes")
  checkmate::assertChoice(compound_id, names(x@pkdata))
  checkmate::assertChoice(format, c("NONMEM"))

  if(!has_pk_profiles(x, compound_id)){
    stop(paste0("No PK profiles available for compound: ", compound_id))
  }


  dosedf <-  x@pkdata[[compound_id]] |>
      dplyr::select("subject_id", "group_label", "route", "dose_amount", "dose_unit") |> 
      dplyr::distinct()

  obsdf <-  x@pkdata[[compound_id]] |> 
      dplyr::select("subject_id", "nominal_time", "conc", "dil", "group_label", 
                    "sex", "age", "race", "extra_factors")  |>
      dplyr::rename(
        ID = "subject_id",
        TIME = "nominal_time",
        DV = "conc",
        DIL = "dil"
      ) |> 
      dplyr::select(dplyr::where( \(x) !all(is.na(x)))) # remove columns with all NA

    


    filelist <- c()

    # temp paths
    tmp_dir <- tempdir()

    if(format == "NONMEM"){

    # merge dosing and obs data 
      nmdata <- obsdf |> 
        dplyr::left_join(dosedf, by = c("ID" = "subject_id", "group_label" = "group_label")) |> 
        dplyr::group_by(.data$ID, .data$group_label) |>
        # add dosing records at time 0 for each subject
        dplyr::reframe(
          ID = c(unique(.data$ID), rep(unique(.data$ID), dplyr::n())),
          group_label = c(unique(.data$group_label), rep(unique(.data$group_label), n())),
          TIME = c(0, .data$TIME),
          DV = c(NA, .data$DV),
          DIL = c(NA, .data$DIL),
          route = c(unique(.data$route), rep(NA, n())),
          dose_amount = c(unique(.data$dose_amount), rep(NA, n())),
          dose_unit = c(unique(.data$dose_unit), rep(NA, n()))) |> 
        dplyr::arrange(.data$ID, .data$TIME, .data$group_label) |> 
        dplyr::ungroup()

      # recode as codebook 
      codebook_descr <- nmdata |> 
        dplyr::filter(.data$TIME == 0) |>
        dplyr::select(-c("TIME", "DV", "DIL")) |> 
        dplyr::distinct() |>
        dplyr::mutate(ID = paste0(.data$ID, "= ", as.numeric(as.factor(.data$ID)))) |>
        dplyr::mutate(group_label = paste0(.data$group_label, "= ", as.numeric(as.factor(.data$group_label)))) |>
        dplyr::mutate(route = paste0(.data$route, "= ", as.numeric(as.factor(.data$route)))) |>
        dplyr::mutate(dose_amount = paste5(.data$dose_amount, " ", .data$dose_unit, "= ", as.numeric(as.factor(.data$dose_amount))))

      
      codebook_colnames <- data.frame(code_name = c("ID", "group_label", "route", "AMT")) |>
        dplyr::mutate(description = 
            dplyr::case_when(
                code_name == "ID" ~ "Unique subject identifier",
                code_name == "group_label" ~ "Group label",
                code_name == "route" ~ "Route of administration",
                code_name == "AMT" ~ "Dose amount"
            ))

      # transform data to numeric and create codebook
      nmdata <- nmdata |> 
        dplyr::mutate(across(dplyr::everything(), \(x) if(is.character(x)) as.numeric(as.factor(x)) else x)) |> 
        dplyr::select(-"dose_unit") |> 
        dplyr::rename(AMT = "dose_amount") |> 
        dplyr::relocate("ID", "TIME", "AMT", "DV") |> 
        dplyr::rename_with(toupper)

      csv_file <- file.path(tmp_dir, "data.csv")
      codebook_file <- file.path(tmp_dir, "codebook.xlsx")

      # ---- write  ----
      write.csv(nmdata, csv_file, row.names = FALSE, na = ".")
      writexl::write_xlsx(list(Data_codebook = codebook_descr, Columns = codebook_colnames),
        path = codebook_file)
      filelist <- c(filelist, csv_file, codebook_file)
    } 

    # ---- zip them ----
    utils::zip(
      zipfile = filename,
      files = filelist
      # mode = "cherry-pick"
    )

}


#' Calculate Cmax, Tmax and AUC for each subject given a compound's PK profiles
#' @param x QuantRes object with PK profiles extracted
#' @param compound_id Compound ID for which to calculate NCA parameters
#' @return data frame with columns: subject_id, cmax, tmax, auc_last, compound_id
#' @details This function calculates Cmax, Tmax and AUC for each subject given a compound's PK profiles. 
#' @export
nca_table <- function(x, compound_id){
  checkmate::assertClass(x, "QuantRes")
  checkmate::assertChoice(compound_id, names(x@pkdata))

  if (!has_pk_profiles(x, compound_id)) {
    stop(paste0("No PK profiles available for compound: ", compound_id))
  }

  pk_data <- x@pkdata[[compound_id]]

  result <- pk_data |>
    dplyr::group_by(.data$subject_id) |>
    dplyr::summarise(
      cmax = max(.data$conc, na.rm = TRUE),
      tmax = .data$nominal_time[which.max(.data$conc)],
      auc_last = pracma::trapz(.data$nominal_time, .data$conc),
      .groups = "drop"
    ) |>
    dplyr::mutate(compound_id = compound_id)

  result
}