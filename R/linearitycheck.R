#' Reverse predict concentration from response
#' @param fit lm object
#' @param newdata vector or data frame with response values
#' @param intercept logical. Whether the model has intercept or not
#' @return numeric. Estimated concentration
#' @author Omar I. Elashkar
#' @export
reverse_predict <- function(fit, newdata, intercept) {
  slope <- ifelse(intercept, unname(stats::coef(fit)[2]), unname(stats::coef(fit)[1]))
  intercept <- ifelse(intercept, unname(stats::coef(fit)[1]), 0)

  # calculate estimated response
  (newdata - intercept) / slope
}

#' Update linearity response, type and stdconc columns
#' response from peaktab
#' type and stdconc from filetab
#' @param quantres QuantRes object
#' @param compound_id character. If NULL, all compounds will be updated
#' @param meta_only logical. If TRUE, only metadata will be updated. lm will not be deleted
#'
#' @return QuantRes object
#' @author Omar Elashkar
#' @noRd
sync_linearity <- function(quantres, compound_id = NULL, meta_only = FALSE) {
  if (is.null(compound_id)) {
    compound_id <- quantres@compounds$compound_id
  }

  for (cmpd in compound_id) {
    cmpd_name <- get_compound_name(quantres, cmpd)
    spiked_name <- paste0("spiked_", cmpd_name)

    is_id <- get_cmpd_IS(quantres, cmpd)
    if (!is.na(is_id)) {
      if (is_integrated(quantres, is_id)) {
        IS_area_df <-
          quantres@peaks |>
          dplyr::filter(compound_id == is_id) |>
          dplyr::select("filename", "area", "compound_id") |>
          dplyr::rename(IS_area = "area")
      }
    }
    if (!exists("IS_area_df")) {
      IS_area_df <- data.frame(
        filename = character(),
        IS_area = numeric(),
        compound_id = character()
      )
    }

    quantres@linearity[[cmpd]]$linearitytab <- quantres@peaks |>
      dplyr::filter(.data$compound_id == !!cmpd) |>
      dplyr::select("filename", "area", "compound_id") |>
      dplyr::left_join(
        quantres@metadata,
        by = c("filename" = "filename")
      ) |>
      # dplyr::filter(.data$type %in% c("Standard", "QC")) |>
      dplyr::rename(abs_response = "area") |>
      dplyr::left_join(IS_area_df, by = dplyr::join_by("filename")) |> # NOTE SAME cmpd, so no cmpd_id
      dplyr::mutate(rel_response = .data$abs_response / .data$IS_area) |>
      dplyr::select(
        "filename",
        "type",
        "sample_location",
        "sample_id",
        "abs_response",
        "rel_response",
        spiked_name,
        "dilution_factor",
        "subject_id",
        "sampling_time",
        "dosage",
        "factor"
      ) |>
      dplyr::rename(stdconc = !!spiked_name) |> # rename to stdconc/nominal_conc
      dplyr::mutate(include = TRUE) |>
      dplyr::mutate(estimated_conc = as.numeric(NA)) |> # reverse
      dplyr::mutate(residual_conc = as.numeric(NA)) |> # reverse
      dplyr::mutate(dev_conc = as.numeric(NA)) |>
      dplyr::mutate(estimate_CI_lwr = as.numeric(NA)) |>
      dplyr::mutate(estimate_CI_upr = as.numeric(NA)) |>
      dplyr::mutate(estimated_pred_lwr = as.numeric(NA)) |>
      dplyr::mutate(estimated_pred_upr = as.numeric(NA)) |>
      dplyr::mutate(estimated_response = as.numeric(NA)) |>
      dplyr::mutate(residual_response = as.numeric(NA)) |>
      dplyr::mutate(rstandard_response = as.numeric(NA)) |>
      dplyr::mutate(passed = as.logical(NA))

    if (!meta_only) {
      quantres@linearity[[cmpd]]$results <- NA
    }
  }

  validObject(quantres)

  quantres
}



setMethod(
  "run_linearity",
  signature(quantres = "QuantRes"),
  function(
    quantres,
    compound_id,
    weight = "1/x^2",
    model = "linear",
    intercept = TRUE,
    normalize = FALSE,
    avg_rep = FALSE
  ) {
    run_linearity_quantres(
      quantres,
      compound_id,
      weight,
      model,
      intercept,
      normalize,
      avg_rep
    )
  }
)

setMethod(
  "run_linearity",
  signature(quantres = "list"),
  function(
    quantres,
    compound_id,
    weight = "1/x^2",
    model = "linear",
    intercept = TRUE,
    normalize = FALSE,
    avg_rep = FALSE
  ) {
    run_linearity_list(
      quantres,
      compound_id,
      weight,
      model,
      intercept,
      normalize,
      avg_rep
    )
  }
)

setMethod(
  "run_linearity",
  signature(quantres = "data.frame"),
  function(
    quantres,
    compound_id = "compound1",
    weight = "1/x^2",
    model = "linear",
    intercept = TRUE,
    response = "abs_response",
    avg_rep = FALSE
  ) {
    run_linearity_df(
      quantres,
      weight,
      model,
      intercept,
      response,
      avg_rep
    )
  }
)

run_linearity_list <- function(
  quantres,
  compound_id,
  weight = "1/x^2",
  model = "linear",
  intercept = TRUE,
  normalize = FALSE,
  avg_rep = FALSE
) {
  target_df <- quantres[[compound_id]]$linearitytab |>
    dplyr::filter(.data$include == TRUE & .data$type == "Standard")
}

run_linearity_df <- function(
  df,
  weight = "1/x^2",
  model = "linear",
  intercept = TRUE,
  response = "abs_response",
  avg_rep = FALSE
) {
  stopifnot(all(c("filename", "type", response, "stdconc", "include") %in% colnames(df)))

  target_df <- df |>
    dplyr::filter(.data$include == TRUE & .data$type == "Standard")

  if (nrow(target_df) == 0) {
    stop("No standards available to run linearity")
  }
  if (any(is.na(target_df$stdconc))) {
    stop("Nominal concentration is missing")
  }

  
  if (all(is.na(target_df[[response]]))) {
      stop(paste0("All ", response, " values are missing"))
  }


  # check if set_linearity

  if (model == "linear") {
    model_func <- stats::lm
  } else if (model == "quadratic") {
    model_func <- stats::nls
  }

  if (weight == "1/x") {
    weight_vec <- 1 / target_df$stdconc
  } else if (weight == "1/x^2") {
    weight_vec <- 1 / target_df$stdconc^2
  } else if (weight == "1/y") {
    weight_vec <- 1 / target_df[[response]]
  } else if (weight == "1/y^2") {
    weight_vec <- 1 / target_df[[response]]^2
  } else if (weight == "1/x^0.5") {
    weight_vec <- 1 / sqrt(target_df$stdconc)
  } else if (weight == "1/y^0.5") {
    weight_vec <- 1 / sqrt(target_df[[response]])
  } else if (weight == "non") {
    weight_vec <- NULL
  }
  # weight_vec <- ifelse(is.infinite(weight_vec) , NULL, weight_vec)

  if (avg_rep) {
    quantres <- quantres |>
      group_by(.data$stdconc) |>
      summarise(
        response = mean(!!sym(response), na.rm = TRUE)
      )
  }

  if (intercept) {
    if (model == "linear") {
      fit <- model_func(
        stats::as.formula(paste0(response, "~stdconc")),
        weights = weight_vec,
        data = target_df
      )
    } else if (model == "quadratic") {
      fit <- model_func(
        response ~ I(stdconc^2) + stdconc,
        data = target_df,
        weights = weight_vec
      )
    }
  } else {
    if (model == "linear") {
      fit <- model_func(
        stats::as.formula(paste0(response, "~stdconc - 1")),
        weights = weight_vec,
        data = target_df
      )
    } else if (model == "quadratic") {
      fit <- model_func(
        response ~ I(stdconc^2) + 0,
        data = target_df,
        weights = weight_vec
      )
    }
  }

  # https://stackoverflow.com/questions/38109501/how-does-predict-lm-compute-confidence-interval-and-prediction-interval
  fitted_res <- stats::predict(fit, interval = "confidence") |> as.data.frame()
  fitted_res_pred <- stats::predict(fit, interval = "prediction") |> as.data.frame()
  # update target_df with resdiual_response and estimated_response

  target_df <- target_df |>
    dplyr::select("filename", "abs_response", "rel_response", "stdconc") |>
    dplyr::mutate(estimated_response = fitted_res$fit) |>
    dplyr::mutate(estimate_CI_lwr = fitted_res$lwr) |>
    dplyr::mutate(estimate_CI_upr = fitted_res$upr) |>
    dplyr::mutate(estimated_pred_lwr = fitted_res_pred$lwr) |>
    dplyr::mutate(estimated_pred_upr = fitted_res_pred$upr) |>
    dplyr::mutate(residual_response = stats::residuals(fit)) |>
    dplyr::mutate(rstandard_response = stats::rstandard(fit))


  resdf <- df |> 
    dplyr::mutate(estimated_response = NA_real_) |>
    dplyr::mutate(estimate_CI_lwr = NA_real_) |>
    dplyr::mutate(estimate_CI_upr = NA_real_) |>
    dplyr::mutate(estimated_pred_lwr = NA_real_) |>
    dplyr::mutate(estimated_pred_upr = NA_real_) |>
    dplyr::mutate(residual_response = NA_real_) |>
    dplyr::mutate(rstandard_response = NA_real_) |>
    dplyr::mutate(estimated_conc = as.numeric(NA)) |>
    dplyr::mutate(residual_conc = as.numeric(NA)) |>
    dplyr::mutate(dev_conc = as.numeric(NA)) |>
    dplyr::mutate(passed = as.logical(NA)) |>
    dplyr::rows_update(target_df, by = c("filename", "stdconc")) |> # update only the rows that were used in fitting
    dplyr::mutate( # predict estimated_conc for all rows
      estimated_conc = reverse_predict(
        fit,
        newdata = df[[response]],
        intercept = intercept
      )
    ) |>
    dplyr::mutate(residual_conc = .data$estimated_conc - .data$stdconc) |>
    dplyr::mutate(dev_conc = rel_deviation(.data$stdconc, .data$estimated_conc)) |>
    dplyr::mutate(passed = dplyr::case_when(abs(dev_conc) <= 0.20 ~ TRUE, TRUE ~ FALSE))

  resdfqc <- resdf |> 
    dplyr::filter(.data$type == "QC")
  resdfstd <- resdf |>
    dplyr::filter(.data$type == "Standard")


  slope <- ifelse(intercept, unname(stats::coef(fit)[2]), unname(stats::coef(fit)[1]))

  intercept <- ifelse(intercept, unname(stats::coef(fit)[1]), intercept)
  intercept <- ifelse(
    intercept != FALSE,
    paste0(
      round(intercept, 2),
      " 95% CI: (",
      round(stats::confint(fit)[1, 1], 2),
      " - ",
      round(stats::confint(fit)[1, 2], 2),
      ")"
    ),
    FALSE
  )

  sd_residuals <- stats::sd(stats::residuals(fit))

  
  reslist <- list(
    modelobj = fit,
    model = model,
    weight = weight,
    avg_rep = avg_rep,
    normalized = ifelse(response == "rel_response", TRUE, FALSE),
    IS = NA,
    r_squared = summary(fit)$r.squared,
    adj_r_squared = summary(fit)$adj.r.squared,
    mape_cs = mape(resdfstd$stdconc, resdfstd$estimated_conc, percent = TRUE),
    mape_qc = mape(resdfqc$stdconc, resdfqc$estimated_conc, percent = TRUE),
    rmse_cs = rmse(resdfstd$stdconc, resdfstd$estimated_conc),
    rmse_qc = rmse(resdfqc$stdconc, resdfqc$estimated_conc),
    intercept = intercept,
    slope = slope,
    see_weighted = sum((1 / weight_vec) * (fit$residuals**2)), # sum of squared residuals
    rse_weighted = summary(fit)$sigma, # weighted residual standard error
    lloq_assumed = min(resdfstd$stdconc),
    uloq_assumed = max(resdfstd$stdconc),
    lloq_passed = .find_passed_lloq(resdf),
    uloq_passed = .find_passed_uloq(resdf),
    loq = 10 * sd_residuals / slope, # FIXME. Study how weighted close to the standard deviation of lloq
    cs_total_passed = .find_passed(resdfstd),
    qc_total_passed = .find_passed(resdfqc),
    qc_level_passed = .find_passed_qc_level(resdfqc),
    n_standards = nrow(resdfstd),
    n_qcs = nrow(resdfqc),
    aic = stats::AIC(fit)
  )
  
    list(resdf = resdf, summary = reslist)
}

#' Run linearity check
#' @param quantres QuantRes object
#' @param compound_id character. Compound ID to run linearity for.
#' @param weight character. Choices are "non", "1/x", "1/x^2", "1/y", "1/y^2"
#' @param model character
#' @param intercept logical
#' @param avg_rep logical
#' @param normalize logical. If TRUE, relative response will be used instead of absolute response.
#' The function will run linearity on all included standards. The residuals will be calculated on all standards,
#' QCs, blanks, double blanks and suitability vials.
#' @return QuantRes object
#' @author Omar Elashkar
#' @noRd
run_linearity_quantres <- function(
  quantres,
  compound_id,
  weight = "1/x^2",
  model = "linear",
  intercept = TRUE,
  normalize = FALSE,
  avg_rep = FALSE
) {
  checkmate::assertChoice(
    weight,
    c("non", "1/x", "1/x^2", "1/y", "1/y^2", "1/x^0.5", "1/y^0.5")
  )
  checkmate::assertChoice(model, c("linear", "quadratic"))
  checkmate::assertLogical(intercept)
  checkmate::assertLogical(avg_rep)
  checkmate::assertString(compound_id)
  checkmate::assertLogical(normalize)


  # check if there is pkmeta
  target_df <- quantres@linearity[[compound_id]]$linearitytab 
  checkmate::assertDataFrame(target_df)
  checkmate::assertNames(
    names(target_df),
    must.include = c("type", "stdconc", "include")
  )

  if(normalize){
      response <- "rel_response"
  } else {
      response <- "abs_response"
  }

  target_list <- run_linearity(
    quantres = target_df, 
    weight = weight, 
    model = model, 
    intercept = intercept, 
    response = response, 
    avg_rep = avg_rep)

  # if(!is_integrated(quantres, compound_id= compound_id)){
  #     stop("Compound has not been integrated")
  # }

  # target_df_qc <- quantres@linearity[[compound_id]]$linearitytab |>
  #     dplyr::filter(type != "QC" || (type == "Standard" & include == FALSE))



  # update the linearitytab
  quantres@linearity[[compound_id]]$linearitytab <- target_list$resdf
  quantres@linearity[[compound_id]]$results <- target_list$summary
  
  quantres
}


#' @author Omar Elashkar
#' @noRd
.find_passed_lloq <- function(df) {
  df |>
    dplyr::mutate(passed = ifelse(abs(.data$dev_conc) <= 0.20, TRUE, FALSE)) |>
    dplyr::filter(.data$passed) |>
    dplyr::mutate(stdconc = as.numeric(.data$stdconc)) |>
    dplyr::pull("stdconc") |>
    min()
}


#' @author Omar I. Elashkar
#' @noRd
.find_passed_uloq <- function(df) {
  df |>
    dplyr::mutate(passed = ifelse(abs(.data$dev_conc) <= 0.15, TRUE, FALSE)) |>
    dplyr::filter(.data$passed) |>
    dplyr::mutate(stdconc = as.numeric(.data$stdconc)) |>
    dplyr::pull("stdconc") |>
    max()
}


#' Calculate passed calibrartion points 
#' @param df data frame with dev_conc and passed columns
#' 
#' This function count passed variables in totality based on bias (relative deviation), so one need to filter for standards or QCs if specific result is expected.
#' @author Omar Elashkar
#' @noRd
.find_passed <- function(df, criteria = "reldev") {
  if (!"passed" %in% colnames(df)) {
    stop("df must contain a column named 'passed'")
  }

  passed <- df |>
    dplyr::summarise(passed = sum(passed), total = n()) |>
    dplyr::mutate(
      passed = paste0(
        .data$passed,
        "/",
        .data$total,
        " (",
        round(.data$passed / .data$total * 100, 2),
        "%)"
      )
    ) |>
    pull("passed")

  passed
}



#' @author Omar Elashkar
#' @noRd
.find_passed_qc_level <- function(df) {
  QCs_passed_level <- df |>
    # dplyr::filter(type == "QC") |>
    dplyr::group_by(.data$stdconc) |>
    dplyr::summarise(passed = sum(.data$passed), total = n()) |>
    dplyr::mutate(
      QCs_passed = paste0(
        .data$stdconc,
        ": ",
        .data$passed,
        "/",
        .data$total,
        " (",
        round(.data$passed / .data$total * 100, 2),
        "%)"
      )
    ) |>
    pull("QCs_passed") |>
    paste(collapse = ", ")
  QCs_passed_level
}


#' @author Omar Elashkar
#' @noRd
plot_linearity <- function(quantres, compound_id) {
  stopifnot(has_linearity(quantres, compound_id))

  response <- quantres@linearity[[compound_id]]$results$normalized |>
    ifelse("rel_response", "abs_response")

  ## extract the linearitytab
  linearitytab <- quantres@linearity[[compound_id]]$linearitytab |>
    dplyr::filter(.data$type %in% c("Standard", "QC"))

  ## extract the results
  results <- quantres@linearity[[compound_id]]$results

  linearityfig <- ggplot(
    linearitytab |>
      arrange(.data$stdconc) |>
      mutate(include = ifelse(.data$include, "Included", "Excluded"))
  ) +
    ggplot2::geom_line(
      data = linearitytab[!is.na(linearitytab$estimated_response), ],
      aes(x = .data$stdconc, y = .data$estimated_response),
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = linearitytab[!is.na(linearitytab$estimate_CI_lwr), ],
      aes(x = .data$stdconc, y = .data$estimate_CI_lwr),
      linetype = "dashed",
      na.rm = TRUE,
      color = "blue"
    ) +
    ggplot2::geom_line(
      data = linearitytab[!is.na(linearitytab$estimate_CI_upr), ],
      aes(x = .data$stdconc, y = .data$estimate_CI_upr),
      linetype = "dashed",
      na.rm = TRUE,
      color = "blue"
    ) +
    ggplot2::geom_line(
      data = linearitytab[!is.na(linearitytab$estimated_pred_lwr), ],
      aes(x = .data$stdconc, y = .data$estimated_pred_lwr),
      linetype = "dotted",
      na.rm = TRUE,
      color = "red"
    ) +
    ggplot2::geom_line(
      data = linearitytab[!is.na(linearitytab$estimated_pred_upr), ],
      aes(x = .data$stdconc, y = .data$estimated_pred_upr),
      linetype = "dotted",
      na.rm = TRUE,
      color = "red"
    ) +
    ggiraph::geom_point_interactive(
      aes(
        tooltip = paste0(.data$filename, " ", .data$dev_conc, "%"),
        data_id = .data$filename,
        x = .data$stdconc,
        y = .data[[response]],
        color = .data$type,
        shape = .data$include
      ),
      size = 3
    ) +
    ggplot2::scale_shape_manual(
      values = c("Included" = 16, "Excluded" = 13)
    ) +
    labs(
      title = paste0("Linearity of ", compound_id),
      x = "Actual Concentration",
      y = response
    ) +
    theme_minimal()
  linearityfig <- ggiraph::girafe(
    ggobj = linearityfig,
    width_svg = 7,
    height_svg = 4,
    options = list(
      ggiraph::opts_selection(type = "single"),
      ggiraph::opts_zoom(max = 4, min = 0.8, duration = 300),
      ggiraph::opts_sizing(rescale = TRUE, width = 0.4)
    )
  )
  # coefficients, r_squared, adj_r_squared, aic
  # linearityfig <- linearityfig +
  #     annotate("text", x = 0.5, y = 0.5, label = paste0("R^2: ", round(results$r_squared, 2))) +
  #     annotate("text", x = 0.5, y = 0.4, label = paste0("Adj R^2: ", round(results$adj_r_squared, 2))) +
  #     annotate("text", x = 0.5, y = 0.3, label = paste0("AIC: ", round(results$aic, 2))) +
  #     annotate("text", x = 0.5, y = 0.2, label = paste0("BIC: ", round(results$bic, 2)))

  linearityfig
}


#' @author Omar Elashkar
#' @noRd
plot_residuals <- function(quantres, compound_id) {
  ## extract the linearitytab
  linearitytab <- quantres@linearity[[compound_id]]$linearitytab |>
    dplyr::filter(.data$type %in% c("Standard", "QC", "Suitability"))

  residualsfig <- ggplot(
    linearitytab |>
      dplyr::mutate(include = ifelse(.data$include, "Included", "Excluded")),
    aes(x = .data$stdconc, y = .data$residual_response)
  ) +
    ggiraph::geom_point_interactive(
      aes(
        tooltip = paste0(.data$filename, " \n", .data$residual_response, "%"),
        data_id = .data$filename,
        x = .data$stdconc,
        y = .data$residual_response,
        color = .data$type,
        shape = .data$include
      ),
      size = 3
    ) +
    ggplot2::scale_shape_manual(
      values = c("Included" = 16, "Excluded" = 13)
    ) +
    labs(
      title = paste0("Residuals of ", compound_id),
      x = "Actual Concentration",
      y = "Residuals (response)"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_smooth(
      aes(x = .data$stdconc, y = .data$residual_response),
      method = "loess",
      linetype = "dashed",
      se = FALSE
    ) +
    theme_minimal()
  residualsfig <- ggiraph::girafe(
    ggobj = residualsfig,
    width_svg = 7,
    height_svg = 4,
    options = list(
      ggiraph::opts_selection(type = "single"),
      ggiraph::opts_zoom(max = 4, min = 0.8, duration = 300),
      ggiraph::opts_sizing(rescale = TRUE, width = 0.4)
    )
  )
  residualsfig
}

#' @author Omar Elashkar
#' @noRd
# x = actual conc, y = %deviation
plot_deviations <- function(quantres, compound_id) {
  ## extract the linearitytab
  linearitytab <- quantres@linearity[[compound_id]]$linearitytab |>
    dplyr::filter(.data$type %in% c("Standard", "QC", "Suitability"))
  deviationsfig <- ggplot(
    linearitytab |>
      dplyr::mutate(include = ifelse(.data$include, "Included", "Excluded")),
    aes(x = .data$stdconc, y = .data$dev_conc)
  ) +
    ggiraph::geom_point_interactive(aes(
      tooltip = paste0(.data$filename, " \n", .data$dev_conc * 100, "%"),
      data_id = .data$filename,
      x = .data$stdconc,
      y = .data$dev_conc * 100,
      color = .data$type,
      shape = .data$include
    )) +
    ggplot2::scale_shape_manual(
      values = c("Included" = 16, "Excluded" = 13)
    ) +
    labs(
      title = paste0("Deviations of ", compound_id),
      x = "Actual Concentration",
      y = "Deviation (%)"
    ) +
    geom_hline(yintercept = 0, color = "red") +
    geom_smooth(
      aes(x = .data$stdconc, y = .data$dev_conc, linetype = "dashed"),
      method = "loess",
      se = FALSE
    ) +
    theme_minimal()
  deviationsfig <- ggiraph::girafe(
    ggobj = deviationsfig,
    width_svg = 7,
    height_svg = 4,
    options = list(
      opts_selection = list(type = "single"),
      opts_zoom = list(max = 4, min = 0.8, duration = 300),
      opts_sizing = list(rescale = TRUE, width = 0.4)
    )
  )
  deviationsfig
}

#' Plot SD vs actual concentration from blanks and QCs aggregates
#' @param quantres QuantRes object
#' @param compound_id character
#' @noRd
#' @author Omar Elashkar
plot_standard_deviation <- function(quantres, compound_id) {
  linearitytab <- quantres@linearity[[compound_id]]$linearitytab |>
    dplyr::filter(.data$include == TRUE & .data$type %in% c("Blank", "QC")) |>
    dplyr::mutate(stdconc = as.numeric(.data$stdconc)) |>
    dplyr::group_by(.data$stdconc) |>
    dplyr::summarise(sd = stats::sd(.data$estimated_conc, na.rm = TRUE))

  ggplot(
    linearitytab |>
      dplyr::mutate(include = ifelse(.data$include, "Included", "Excluded"))
  ) +
    geom_point(aes(x = .data$stdconc, y = .data$sd)) +
    geom_smooth(aes(x = .data$stdconc, y = .data$sd), method = "loess") +
    labs(
      title = paste0(
        "Standard Deviation vs Actual Concentration of ",
        compound_id
      ),
      x = "Actual Concentration",
      y = "Standard Deviation"
    ) +
    theme_minimal()
}



#' Tabulate linearity summary for QuantRes object
#' @author Omar Elashkar
#' @noRd
setMethod(
  "tabulate_summary_linearity",
  signature(object = "QuantRes"),
  function(object, compound_id = NULL) {
    if (is.null(compound_id)) {
      compound_id <- names(object@linearity)
    }
    # filter compound_id to those with linearity
    compound_id <- compound_id[sapply(compound_id, function(cmpd) {
      has_linearity(object, cmpd)
    })]

    res <- lapply(compound_id, function(cmpd) {
      tabulate_summary_linearity_list(object@linearity[[cmpd]])
    })

    do.call(rbind, res)
  }
)


setMethod(
  "tabulate_summary_linearity_list",
  signature(object = "list"),
  function(object) {
    if (!is.null(object$results)) {
      x <- data.frame(
        compound_id = if (!is.null(object$compound_id)) object$compound_id else NA,
        weight = object$results$weight,
        normalized = object$results$normalized,
        avg_rep = object$results$avg_rep,
        slope = object$results$slope,
        intercept = object$results$intercept,
        r_squared = object$results$r_squared,
        adj_r_squared = object$results$adj_r_squared,
        mape_cs = object$results$mape_cs,
        mape_qc = object$results$mape_qc,
        rmse_cs = object$results$rmse_cs,
        rmse_qc = object$results$rmse_qc,
        aic = object$results$aic,
        lloq_assumed = object$results$lloq_assumed,
        uloq_assumed = object$results$uloq_assumed,
        lloq_passed = object$results$lloq_passed,
        uloq_passed = object$results$uloq_passed,
        rse = object$results$rse_weighted,
        see = object$results$see_weighted,
        standards_passed = object$results$cs_total_passed,
        QCs_passed_level = object$results$qc_level_passed,
        QCs_passed_total = object$results$qc_total_passed,
        stringsAsFactors = FALSE
      )
      x
    } else {
      NULL
    }
  }
)

#' @author Omar Elashkar
#' @noRd
setMethod(
  "tabulate_summary_linearity",
  signature(object = "list"),
  function(object) {
    tabulate_summary_linearity_list(object)
  }
)

#' Exclude file from linearity run
#' This excludes only standards if found
#' @param quantres QuantRes object
#' @param compound_id character
#' @param filesnames character
#' @return QuantRes object
#' @author Omar Elashkar
#' @noRd
exclude_linearity <- function(quantres, compound_id, filesnames) {
  quantres@linearity[[compound_id]]$linearitytab <-
    quantres@linearity[[compound_id]]$linearitytab |>
    dplyr::mutate(
      include = ifelse(
        .data$filename %in% filesnames & .data$type == "Standard",
        FALSE,
        .data$include
      )
    )

  quantres
}


#' Include file from linearity run
#' This includes only standards if found
#' @param quantres QuantRes object
#' @param compound_id character
#' @param filesnames character
#' @return QuantRes object
#' @author Omar Elashkar
#' @noRd
include_linearity <- function(quantres, compound_id, filesnames) {
  quantres@linearity[[compound_id]]$linearitytab <-
    quantres@linearity[[compound_id]]$linearitytab |>
    dplyr::mutate(
      include = ifelse(
        .data$filename %in% filesnames & .data$type == "Standard",
        TRUE,
        .data$include
      )
    )

  quantres
}


# check if cmpound has linearity normalized flag
linearity_normalized <- function(quantres, compound_id) {
  stopifnot(has_linearity(quantres, compound_id))
  quantres@linearity[[compound_id]]$results$normalized
}

#' Convert response to concentration
#' @param quantres QuantRes object
#' @param compound_id character
#' @param response numeric. Must match the response type used in linearity. Either abs_response or rel_response
#' @return numeric
response_to_conc <- function(quantres, compound_id, response) {
  stopifnot(has_linearity(quantres, compound_id))
  fit <- quantres@linearity[[compound_id]]$results$model
  intercept <- quantres@linearity[[compound_id]]$results$intercept
  slope <- quantres@linearity[[compound_id]]$results$slope
  if (is.null(fit)) {
    stop("No linearity model has been run")
  }
  if (is.null(intercept)) {
    stop("No intercept has been calculated")
  }
  if (is.null(slope)) {
    stop("No slope has been calculated")
  }
  (response - intercept) / slope
}

check_QC_reps <- function(quantres, min_levels = 3, min_reps = 6) {}


#' Check if linearity for specific compound has been executed
#' @noRd
has_linearity <- function(quantres, compound_id) {
  checkmate::assertClass(quantres, "QuantRes")
  # checkmate::assertCount(compound_id, positive = TRUE)
  # compound_id <- .cmpds_string_handler(compound_id)

  tryCatch(
    {
      linearity <- quantres@linearity[[compound_id]]$results$model
      !is.null(linearity)
    },
    error = function(e) {
      FALSE
    }
  )
}
