#'@import PKbioanalysis
.generate_testing_injeseq <- function() {
  plate <- generate_96() |>
    add_DB() |>
    add_DB() |>
    add_blank() |>
    add_cs_curve(c(1, 2, 5, 10, 20, 50)) |>
    register_plate()

  x <- build_injec_seq(
    plate,
    injec_vol = 2,
    tray = "1",
    method = 1,
    suffix = "NT",
    prefix = "2024-04-15"
  )

  x |> write_injec_seq()

  # each of the compounds could be associated with multiple different transitions. Either parent of not

  compounds <- # q1-q3
    c(
      "GABA1", # 104.02-42.6  & 104.02-38.9
      # "GABA2",
      "Histamine 1", # 112.04-67.9 & 112.04-82.9
      # "Histamine 2",
      "acetylcholine 1", # 146.07-59.99 & 146.07-86.99
      # "acetylcholine 2",
      "glutamate 1", # 148.03-84.13 & 148.03-102.13
      # "glutamate 2",
      "domapine 1", # 154.07-91.1 & 154.07-118.9
      # "domapine 2",
      "dopamine d4", # 158.07-94.1
      "3-methoxytyramine 1", # 168.05-91.1 & 168.05-118.9
      # "3-methoxytyramine 2",
      "norepinephrine 1", # 170.06-106.1 & 170.06-134.8
      # "norepinephrine 2",
      "serotonin 1", #177.1-105.02 & 177.1-115.03
      # "serotonin 2",
      "epinephrine 1", #184.10-102.02 & 184.10-134.9
      # "epinephrine 2",
      "5-HIAA 1" # 192.08-118.19 & 192.08-146.04
      # "5-HIAA 2"
    )

  # repeat file names for each compound
  # names <- paste(x$injec_list$FILE_NAME, paste0("R", 1:length(x$injec_list$FILE_NAME)), sep = "_")
  names <- system.file(
    "extdata",
    "waters_MZML_ex",
    package = "PKChromaMetrics"
  ) |>
    list.files() |>
    basename()

  peak_start <- NA
  peak_end <- NA
  # compound <- rep(compounds, each = length(x$injec_list$FILE_NAME))
  q1 <- c(
    104.02,
    104.02, # GABA
    112.04,
    112.04, # histamine
    146.07,
    146.07, # acetylcholine
    148.03,
    148.03, # glutamate
    154.07,
    154.07, # domapine
    158.07, # dopamine d4
    168.05,
    168.05, # 3-methoxytyramine
    170.06,
    170.06, # norepinephrine
    177.1,
    177.1, # serotonin
    184.1,
    184.1, # epinephrine
    192.08,
    192.08
  ) # 5-HIAA
  q3 <- c(
    42.6,
    68.9, # GABA
    67.9,
    82.9, # histamine
    59.99,
    86.99, # acetylcholine
    84.13,
    102.13, # glutamate
    91.1,
    118.9, # domapine
    94.7, # dopamine d4
    91.1,
    118.9, # 3-methoxytyramine
    106.9,
    134.8, # norepinephrine
    105.02,
    115.03, # serotonin
    107.02,
    134.9, # epinephrine
    118.19,
    146.04
  ) # 5-HIAA
  trans <- data.frame(
    q1,
    q3,
    compound = c(
      "GABA",
      "GABA",
      "histamine",
      "histamine",
      "acetylcholine",
      "acetylcholine",
      "glutamate",
      "glutamate",
      "dopamine",
      "dopamine",
      "dopamine d4",
      "3-methoxytyramine",
      "3-methoxytyramine",
      "norepinephrine",
      "norepinephrine",
      "serotonin",
      "serotonin",
      "epinephrine",
      "epinephrine",
      "5-HIAA",
      "5-HIAA"
    )
  )
  nrow(trans)
  all <- trans |> mutate(all = paste(.data$q1, .data$q3, .data$compound, sep = "__")) |> pull(.data$all)
  length(all)

  imported_peaks_path <- system.file(
    "sample_peaktab.csv",
    package = "PKChromaMetrics"
  )
  expand.grid(filename = names, all = all) |>
    separate_wider_delim("all", names = c("q1", "q3", "compound"), delim = "__") |>
    mutate(peak_start = NA, peak_end = NA) |>
    arrange(.data$filename, .data$compound) |>
    mutate(IS = "dopamine-d4") |>
    utils::write.csv(imported_peaks_path, row.names = F)
}


write_test_pkset <- function(){

  # load extdata 
  d <- utils::read.csv(system.file("extdata", "08122019_MTG_injeclist_parsed.csv", package = "PKbioanalysis"))
  d <- d |> dplyr::filter(.data$index != 1)

  # clean and sanitize

  # Create pseudo study
  study <- create_new_study(
    data.frame(type = "SD", title = "Kartom Study", description = "", subject_type = "Animal", pkstudy = FALSE)
  )

  # write subjects
  add_dosing_db(study$id, data.frame(group_label = "13.5mg/kg", 
                                    dose_amount = 13.5, 
                                    dose_unit = "mg", 
                                    route = "PO", 
                                    period_number = 1,
                                    dose_freq = 1, 
                                    dose_addl = 0, 
                                    formulation = "solution"
                                    ))

  add_subjects_db(study$id, data.frame(
    study_id = study$id,
    subject_id = unique(d$subject_id)[unique(d$subject_id) != ""],
    group_label = "13.5mg/kg",
    sex = "M"
    ))


  subjectdf <- d |> 
    dplyr::filter(.data$type == "analyte") |> 
    dplyr::mutate(dilution = as.numeric(gsub("X", "", .data$dilution)))

  # create and write log PK dataset 
  log_ids <- add_sample_log(study$id, data.frame(
    subject_id = subjectdf$subject_id,
    nominal_time = subjectdf$time
  ))

  # create and write injection sequence
  plate <- generate_96() |>
    add_blank() |>
    add_cs_curve(c(1, 2, 5, 10, 20, 50)) |>
    add_samples_db(logIds = log_ids$log_id, dil = subjectdf$dilution) |>
    register_plate()

  injec_seq <- build_injec_seq(
    plate,
    rep_suitability = 0,
    injec_vol = 5,
    tray = "1",
    method = 1,
    prefix = "2024-06-10"
  )
  write_injec_seq(injec_seq)

  # query the database manually to change injec_id to match d$Index 
  db <- .connect_to_db()
  on.exit(.close_db(db, TRUE), add = TRUE)
  injsq <- injec_seq$injec_list |> 
    dplyr::filter(.data$TYPE == "Analyte")
  dd <- d |> 
    dplyr::filter(.data$type == "analyte")
  stopifnot(nrow(injsq) == nrow(dd))

  for (i in seq_along(injsq$FILE_NAME)) {
    DBI::dbExecute(
      db,
      paste0(
        "UPDATE samples SET injec_id = '",
        dd$index[i],
        "' WHERE file_name = '",
        injsq$FILE_NAME[i],
        "'"
      )
    )
  }

}
