

#'
#' @param m a 96-well matrix
#' @param df data.frame contains plate's metadata
#' @param empty_rows a vector for current active rows
#' @param last_modified last modified date
#' @param plate_id plate id
#' @param descr plate description
#' 
#' @importFrom dplyr mutate slice_tail
#' @noRd
# Define the PlateObj class
setClass(
  "PlateObj",
  slots = list(
    plate = "matrix",
    df = "data.frame",
    empty_rows = "character",
    filling_scheme = "list",
    last_filled = "character",
    last_modified = "POSIXct",
    plate_id = "character",
    descr = "character"
  )
)

setClass("RegisteredPlate", contains = "PlateObj")

setClass("MultiPlate", 
  slots = list(
    plates = "list"
  )
)

#' Subsetting method for MultiPlate
#' @param x MultiPlate object
#' @param i index
#' @param j index
#' @param ... additional arguments
#' @export
#' @returns PlateObj object
setMethod("[[" , signature(x = "MultiPlate", i = "ANY" , j = "ANY"),
  function(x, i, ...) {
    x@plates[[i]]
  }
)

#' Length method for MultiPlate
#' @param x MultiPlate object
#' @export
#' @returns number of plates
setMethod("length", signature(x = "MultiPlate"),
  function(x) {
    length(x@plates)
  }
)

#' Create Injection Sequence
#'
#' @param plate PlateObj object
#' @param method choose method from database
#' @param repeat_std number of re-injections for calibration standards. Default is 1.
#' @param repeat_analyte number of re-injections for unknown samples. Default is 1
#' @param repeat_qc number of re-injections for QC wells. Default is 1
#' @param blank_after_top_conc If TRUE, adding blank after high concentrations of standards and QCS.
#' @param blank_at_end If True, adding blank at the end of queue.
#' @param system_suitability Number of re-injections for suitability vial.
#' @param blank_every_n If no QCs, frequency of injecting blanks between analytes.
#' @param inject_vol volume of injection in micro liters.
#' @param descr Run description.
#' @param suffix string to be added to the end of the filename. Default is "1".
#' @param prefix string at the beginning of the filename. Default is today's date.
#' @param explore_mode options either TRUE or FALSE. Default if FALSE.
#' @param tray Location in sample manager.
#' @param conc_df data.frame matching compound name to a scaling factor. Maximum 20 compounds allowed.
#'
#' @details
#' explore_mode controls if exploratory samples are to be injected. A random sample from each CS and QC group will be sampled along with 1 blank sample.
#' @returns InjecListObj object
#'@export
setGeneric("build_injec_seq", function(plate, method, 
  repeat_std = 1, repeat_qc = 1, repeat_analyte = 1, 
  blank_after_top_conc = TRUE, blank_at_end = TRUE, 
  system_suitability = 0, blank_every_n = NULL, 
  inject_vol, descr = "", prefix = Sys.Date(), 
  suffix = "1", tray = 1, explore_mode = FALSE, conc_df = NULL) standardGeneric("build_injec_seq"))



#' Create Injection Sequence from PlateObj (Single Plate)
#' @importFrom dplyr bind_rows bind_cols mutate add_row filter arrange count group_by group_modify ungroup select
#' @param plate PlateObj object
#' @param method choose method from database
#' @param repeat_std number of re-injections for calibration standards. Default is 1.
#' @param repeat_analyte number of re-injections for unknown samples. Default is 1
#' @param repeat_qc number of re-injections for QC wells. Default is 1
#' @param blank_after_top_conc If TRUE, adding blank after high concentrations of standards and QCS.
#' @param blank_at_end If True, adding blank at the end of queue.
#' @param system_suitability Number of re-injections for suitability vial.
#' @param blank_every_n If no QCs, frequency of injecting blanks between analytes.
#' @param inject_vol volume of injection in micro liters.
#' @param descr Run description.
#' @param suffix string to be added to the end of the filename. Default is "1".
#' @param prefix string at the beginning of the filename. Default is today's date.
#' @param explore_mode options either TRUE or FALSE. Default if FALSE.
#' @param tray Location in sample manager.
#' @param conc_df data.frame matching compound name to a scaling factor. Maximum 20 compounds allowed.
#' @export
#' @keywords internal
#' @returns InjecListObj object
setMethod("build_injec_seq" , "PlateObj" , function(plate,
                        method,
                        repeat_std = 1,
                        repeat_qc = 1,
                        repeat_analyte = 1,
                        blank_after_top_conc = TRUE,
                        blank_at_end = TRUE,
                        system_suitability = 0,
                        blank_every_n = NULL,
                        inject_vol,
                        descr = "",
                        prefix = Sys.Date(),
                        suffix = "1",
                        tray = 1,
                        explore_mode = FALSE,
                        conc_df = NULL) {

  checkmate::assertNumber(repeat_std, finite = TRUE, lower = 1)
  checkmate::assertNumber(repeat_qc, finite = TRUE, lower = 1)
  checkmate::assertNumber(repeat_analyte, finite = TRUE, lower = 1)
  checkmate::assertNumeric(inject_vol, finite = TRUE, lower = 0.1)
  checkmate::assertNumber(blank_every_n, null.ok = TRUE, lower = 1, finite = TRUE)
  checkmate::assertNumber(system_suitability, lower = 0, finite = TRUE)
  checkmate::assertChoice(explore_mode, choices = c(TRUE, FALSE))
  checkmate::checkString(descr, null.ok = TRUE)
  # checkmate::assertString(prefix)
  checkmate::assertString(suffix)
  checkmate::assertCharacter(tray, min.len = 1, max.len = 12, unique = TRUE)
  # checkmate::assertString(tray)
  checkmate::assertDataFrame(conc_df,
    null.ok = TRUE,
    min.rows = 1,
    max.rows = 20,
    type = c("character", "numeric"),
    col.names = "named", ncols = 2, any.missing =  FALSE)

  # assert plate is registered
  if(!.is_registered(plate)){
    stop("Plate is not registered. Please register the plate first.")
  }

  current_plate_id <- plate@plate_id

  # add tray column if single tray (previous call will make it non-NULL if multiplate)
  if(!("tray" %in% colnames(plate@df))){
    stopifnot(length(tray) == 1)
    if(length(tray) != 1){
      stop("Tray must be a single value for single plate")
    }
    plate@df$tray <- tray
  }
  plate <-
    plate@df |> dplyr::mutate(SAMPLE_LOCATION = paste0(tray, ":", .data$SAMPLE_LOCATION))

  df <- plate[FALSE, ] # empty df, same dims

  double_blanks <- dplyr::filter(plate, .data$TYPE == "DoubleBlank")
  IS_blanks <- dplyr::filter(plate, .data$TYPE == "ISBlank")
  # locate positive blanks
  blank_list <- dplyr::filter(plate, .data$TYPE == "Blank")
  # find top conc in std
  std_list <- dplyr::filter(plate, .data$TYPE == "Standard") |> dplyr::arrange(as.numeric(.data$e_rep), as.numeric(.data$conc))
  # find top conc in qc
  qc_list <- dplyr::filter(plate, .data$TYPE == "QC") |>  dplyr::arrange(as.numeric(.data$e_rep), .data$value)
  dqc_list <- dplyr::filter(plate, .data$TYPE == "DQC") |> dplyr::arrange(as.numeric(.data$e_rep), .data$value)
  analyte_list <- dplyr::filter(plate, .data$TYPE == "Analyte") |> dplyr::arrange(.data$samples)

  suitability_list <- filter(plate, .data$TYPE == "Suitability")



  no_qc <- ifelse(nrow(qc_list) == 0, TRUE, FALSE) #
  no_analyte <- ifelse(nrow(analyte_list) == 0, TRUE, FALSE)
  no_dqc <- ifelse(nrow(dqc_list) == 0, TRUE, FALSE)



  if (!no_qc) {
    stopifnot(nrow(qc_list) %% 4 == 0)
    qc_replicates <-
      qc_list |>
      dplyr::count(.data$value, .by = "value") |>
      dplyr::pull(n) |>
      unique()
    stopifnot(length(qc_replicates) == 1)
  }

  ## 1. xplore mode. 1 sample from each group
  if(explore_mode){ 
    xplore_df <- df[FALSE, ] # empty df
    # add random sample from each group
    if(nrow(std_list) > 0){
      std_xplore <- std_list |>
        dplyr::group_by(.data$std_rep) |>
        dplyr::sample_n(1) |>
        dplyr::ungroup()
        xplore_df <-  rbind(xplore_df, std_xplore)
    }

    if(nrow(qc_list) > 0){
      qc_xplore <- qc_list |>
        dplyr::group_by(.data$std_rep) |>
        dplyr::sample_n(1) |>
        dplyr::ungroup()
        xplore_df <-  rbind(xplore_df, qc_xplore)
    }

    if(nrow(dqc_list) > 0){
      dqc_xplore <- dqc_list |>
        dplyr::group_by(.data$std_rep) |>
        dplyr::sample_n(1) |>
        dplyr::ungroup()
        xplore_df <-  rbind(xplore_df, dqc_xplore)
    }

    if(nrow(analyte_list) > 0){
      analyte_xplore <- analyte_list |>
        dplyr::sample_n(1)
        xplore_df <-  rbind(xplore_df, analyte_xplore)
    }

    if(nrow(blank_list) > 0){
      blank_xplore <- blank_list |>
        dplyr::sample_n(1)
      xplore_df <-  rbind(xplore_df, blank_xplore)
    }

    xplore_df <- xplore_df |>
      mutate(value = paste0(.data$value, "_explore"))

    df <- bind_rows(df, xplore_df)
  }

  # 2. blanks
  for(i in 1:2){
  # double blank
    df <- add_row(df, double_blanks)
    # IS blank
    df <- add_row(df, IS_blanks)
  }

  #3. suitability
  if (system_suitability > 0) {
    stopifnot("There is no suitability well in the plate. Please add it using add_suitability()" = nrow(suitability_list) >= 1)
    # n_blanks <- nrow(blank_list)
    # stopifnot(n_blanks >=2) # FIXME

    # df <- add_row(df,
    #     mutate(blank_list, value = paste0(value, "-suitability"))[rep(1, system_suitability),]
    #  )
    # df <- add_row(df, blank_list[-1,])

    for (i in seq(system_suitability)) {
      df <- add_row(df, suitability_list)
    }
  }

  # blanks
  df <- add_row(df, blank_list)

  # standards
  for (i in seq(repeat_std)) {
    df <- bind_rows(df, std_list)

    if (blank_after_top_conc) {
      df <- bind_rows(df, blank_list)
    }
  }

  # no qc, but analyte
  if (no_qc & !no_analyte) {
    # inject analyte if no QCs
    for (i in seq(repeat_analyte)) {
      if (!is.null(blank_every_n)) {
        analyte_list <- .add_every_n(analyte_list, blank_list, blank_every_n)
      }

      df <- bind_rows(df, analyte_list)

      if (blank_after_top_conc) {
        df <- bind_rows(df, blank_list)
      }
    }
  }

  # qc
  if (!no_qc) {
    # TODO repeat analytes and qcs  with n_analyte and n_qc
    if (!no_analyte) {
      # divide analyte list by number of QCs
      fac <- round(nrow(analyte_list) / qc_replicates)
      fac <-
        sort(rep(
          1:qc_replicates,
          by = fac,
          length.out = nrow(analyte_list)
        ))
      analyte_list <- analyte_list |> split(fac)
    }

    group <- rep(1:qc_replicates, length.out = nrow(qc_list))
    qc_list <- qc_list |> split(group)

    # add qc
    for (i in seq_along(qc_list)) {
      df <- bind_rows(df, qc_list[[i]])
      if (!no_analyte) {
        df <- bind_rows(df, analyte_list[[i]])
      }

      if (blank_after_top_conc) {
        df <- bind_rows(df, blank_list)
      }
    }



    if (!blank_after_top_conc & blank_at_end) {
      df <- bind_rows(df, blank_list)
    }
  }

  # dqc
  if (!no_dqc) {
    for (i in seq(repeat_qc)) {
      df <- bind_rows(df, dqc_list)
    }
  }



  if(!is.null(conc_df)){
    # add conc_df to plate
    conc_df <- t(conc_df)
    cmpd_vec <- conc_df[1,]
    cmpd_names <- paste0("COMPOUND_", LETTERS[seq_along(cmpd_vec)])
    conc_vec <- conc_df[2,]
    conc_names <- paste0("CONC_", LETTERS[seq_along(conc_vec)])
    conc_df = data.frame(matrix(nrow = 1, ncol = length(cmpd_vec)*2))
    colnames(conc_df) <- c(cmpd_names, conc_names)
    conc_df[1:length(cmpd_vec)] <- cmpd_vec
    conc_df[(length(cmpd_vec)+1):(length(cmpd_vec)*2)] <- conc_vec

    # min_conc <- min(as.numeric(df$conc))
    df <- df |> dplyr::bind_cols(conc_df) |>  # bind conc_df
          dplyr::mutate(dplyr::across(starts_with("CONC_"),
                                \(x) (as.numeric(x) * as.numeric(.data$conc)))) # multiply conc_df with conc and divide by min conc

  } else{
    df <- dplyr::mutate(df, CONC_A = .data$conc)

  }

  # create filename
  ## Date
  df <- df |>
    dplyr::mutate(
      Index = dplyr::row_number(),
      FILE_NAME = paste0(prefix, "_", .data$value, "_", suffix),
      INJ_VOL = inject_vol,
      # CONC_A = conc,
      FILE_TEXT = descr,
      INLET_METHOD = method
    )


  # TODO
  # if(!is.null(conc_df)){
  #   names <- names(conc_df)
  #   for(i in seq_along(names)){
  #     df <- df |> mutate( {{LETTERS[i]}} = names[i])
  #   }
  #   df |> mutate("Compound_A" = names[1], "Compound_B" = names[2])
  # }

  x <- .injecList(df, current_plate_id)
  print(x)
})



#' Create Injection Sequence from MultiPlate (Multiple Plates)
#' 
#' @param plate MultiPlate object
#' @param method choose method from database
#' @param repeat_std number of re-injections for calibration standards. Default is 1.
#' @param repeat_analyte number of re-injections for unknown samples. Default is 1
#' @param repeat_qc number of re-injections for QC wells. Default is 1
#' @param blank_after_top_conc If TRUE, adding blank after high concentrations of standards and QCS.
#' @param blank_at_end If True, adding blank at the end of queue.
#' @param system_suitability Number of re-injections for suitability vial.
#' @param blank_every_n If no QCs, frequency of injecting blanks between analytes.
#' @param inject_vol volume of injection in micro liters.
#' @param descr Run description.
#' @param suffix string to be added to the end of the filename. Default is "1".
#' @param prefix string at the beginning of the filename. Default is today's date.
#' @param explore_mode options either TRUE or FALSE. Default if FALSE.
#' @param tray Location in sample manager.
#' @param conc_df data.frame matching compound name to a scaling factor. Maximum 20 compounds allowed.
#' @keywords internal
#' @export
#' @returns InjecListObj object
setMethod("build_injec_seq", "MultiPlate",  function(plate, method,
  repeat_std = 1, repeat_qc = 1, repeat_analyte = 1,
  blank_after_top_conc = TRUE, blank_at_end = TRUE, system_suitability = 0,
  blank_every_n = NULL, inject_vol, descr = "",
  prefix = Sys.Date(), suffix = "1", tray, explore_mode = FALSE, conc_df = NULL) {

  checkmate::assertCharacter(tray, min.len = 1, max.len = 12, unique = TRUE)

  plate <- plate@plates
  if(length(plate) == 1){
    plate <- plate[[1]]
  } else{
    ## assert length of tray is equal to number of plates
    if(length(tray) != length(plate)){
      stop("Number of tray slots must be equal to number of plates")
    }
    ## assert all plates are registered
    if(!all(sapply(plate, .is_registered))){
      stop("All plates are not registered. Please register the plates first.")
    }

    m <- lapply(plate, function(x) x@plate)
    m <- do.call(rbind, m)

    df <- lapply(1:length(plate), function(i){
      x <- plate[[i]]@df
      x$tray <- tray[i]
      x
    })

    df <- do.call(rbind, df)

    plate_id <- sapply(plate, function(x) x@plate_id)

    descr <- sapply(plate, function(x) x@descr) |> paste0(collapse = ", ")

    empty_rows <- sapply(plate, function(x) x@empty_rows)

    last_modified <- sapply(plate, function(x) x@last_modified)

    plate <- new("RegisteredPlate", plate = m, df = df, plate_id = plate_id,  descr = descr,
      # Note: all from here is dummy to avoid class checking error, this plate will not be returned 
      empty_rows = empty_rows[,1], last_modified = as.POSIXct(Sys.Date()),  
      filling_scheme = plate[[1]]@filling_scheme, last_filled = plate[[1]]@last_filled) 
    plate
  }

  build_injec_seq(plate, method = method,
                  repeat_std = repeat_std, repeat_qc = repeat_qc, repeat_analyte = repeat_analyte,
                  blank_after_top_conc = blank_after_top_conc, blank_at_end = blank_at_end,
                  system_suitability = system_suitability, blank_every_n = blank_every_n,
                  inject_vol = inject_vol, descr = descr, prefix = prefix, suffix = suffix,
                  tray = tray, explore_mode = explore_mode, conc_df = conc_df)

})




# create validity method for PlateObj
setValidity("PlateObj", function(object) {
  if (!is.matrix(object@plate)) {
    stop("plate must be a matrix")
  }
  if (!is.data.frame(object@df)) {
    stop("df must be a data.frame")
  }

  
  col_type <- c("integer", "integer", "character", "character", "character", "character", 
        "numeric", "character", "character", "integer", "integer")
  cols <- c("row", "col", "value", "SAMPLE_LOCATION", "samples", "conc", 
   "time", "factor", "dosage", "TYPE", "std_rep", "e_rep", 'dil')
  checkmate::assertNames(names(object@df), must.include = cols)


  if(!is.list(object@filling_scheme)){
    stop("filling_scheme must be a list")
  }

  if(!(object@filling_scheme$scheme %in% c("h", "v", "hv"))){
    stop("filling_scheme must be either 'h', 'v' or 'hv'")
  }

  if (!is.character(object@empty_rows)) {
    stop("empty_rows must be a character")
  }
  if (!is.character(object@last_filled)) {
    stop("last_filled must be a character")
  }
  
  if (!is.character(object@plate_id)) {
    stop("plate_id must be a character")
  }
  if (!is.character(object@descr)) {
    stop("descr must be a character")
  }

  TRUE
})


setMethod(
  "show",
  signature = "PlateObj",
  definition = function(object) {
    print(object)
  }
)



#' Register a plate
#' This will save the plate to the database
#' @param plate PlateObj object or MultiPlate object
#' @returns PlateObj object or list of PlateObj objects
#' @export
setGeneric("register_plate", function(plate) standardGeneric("register_plate"))


#' Register a plate
#' This will save the plate to the database
#' @param plate PlateObj object
#' @export
#' @keywords internal
#' @returns Registered PlateObj object
setMethod("register_plate", "PlateObj", function(plate){
  .register_plate_logic(plate)
})

#' Register a multiple plates at once
#' @param plate MultiPlate object
#' @export
#' @keywords internal
#' @return a list of RegisteredPlate objects
setMethod("register_plate", "MultiPlate", function(plate){
  lapply(plate@plates, .register_plate_logic)
})
