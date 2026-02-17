empty_string_to_na <- function(df) {
  df <- df |> dplyr::mutate(dplyr::across(dplyr::everything(), trimws))
  df[df == ""] <- NA
  df
}


#'@noRd
.connect_to_db <- function() {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  db
}

.close_db <- function(db, gc = FALSE) {
  duckdb::dbDisconnect(db, shutdown = TRUE)
  if (gc) {
    gc()
  }
}


#' Delete samples database
#' @noRd
.reset_samples_db <- function() {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")

  if (file.exists(db_path)) {
    file.rename(db_path, paste0(db_path, "_old"))
  }
}


#' Return metadata table for sample list
#' @noRd
.get_samplesdb_metadata <- function() {
  .check_sample_db()
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  platesdb <- DBI::dbGetQuery(db, "SELECT * FROM platesdb")
  .close_db(db)

  platesdb
}

.get_samplelist <- function(id = NULL) {
  .check_sample_db()
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  if (is.null(id)) {
    query <- "SELECT * FROM samples"
  } else {
    query <- paste0("SELECT * FROM samples WHERE list_id = ", id)
  }
  sample_list <- DBI::dbGetQuery(db, query)
  .close_db(db)
  sample_list
}

# create it if not exists
.check_sample_db <- function() {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")

  # Check if the database file exists
  db <- duckdb::dbConnect(duckdb::duckdb(), db_path)
  # This id auto increments and is assigned to list_id above
  DBI::dbExecute(
    db,
    "
    CREATE TABLE IF NOT EXISTS platesdb (
      list_id INTEGER PRIMARY KEY, 
      date TEXT,
      assoc_plates TEXT,
      description TEXT,
      UNIQUE(list_id)
    );
  "
  ) # id, date, assoc_plates

  DBI::dbExecute(
    db,
    "
  CREATE TABLE IF NOT EXISTS samples (
    file_name TEXT PRIMARY KEY,

    list_id INTEGER REFERENCES platesdb(list_id),
    injec_id TEXT NOT NULL, -- unique for each injection, back link
    plate_id INTEGER,

    study_id TEXT, -- soft reference to study table
    log_id TEXT, -- soft reference to log table

    inlet_method TEXT,
    row INTEGER,
    col INTEGER,
    value TEXT,
    sample_location TEXT,
    samples TEXT,
    type TEXT,
    std_rep INTEGER,
    e_rep INTEGER,
    tray TEXT,
    inj_vol REAL,
    conc TEXT, -- don't remove
    conc_a TEXT,
    conc_b TEXT,
    conc_c TEXT,
    conc_d TEXT,
    conc_e TEXT,
    conc_f TEXT,
    conc_g TEXT,
    conc_h TEXT,
    conc_i TEXT,
    conc_j TEXT,
    conc_k TEXT,
    conc_l TEXT,
    conc_m TEXT,
    conc_n TEXT,
    conc_o TEXT,
    conc_p TEXT,

    compound_a TEXT,
    compound_b TEXT,
    compound_c TEXT,
    compound_d TEXT,
    compound_e TEXT,
    compound_f TEXT,
    compound_g TEXT,
    compound_h TEXT,
    compound_i TEXT,
    compound_j TEXT,
    compound_k TEXT,
    compound_m TEXT,
    compound_n TEXT,
    compound_l TEXT,
    compound_o TEXT,
    compound_p TEXT,

    file_text TEXT,
    a_group TEXT,
    factor TEXT,
    dil REAL,

    time TEXT,
    dose TEXT,
    dose_unit TEXT,
    ii REAL,
    addl INTEGER,
    route TEXT,
    cmt TEXT,
    sex TEXT,

    UNIQUE(file_name),
    UNIQUE(injec_id)
  );
"
  )

  DBI::dbExecute(
    db,
    " 
  CREATE TABLE IF NOT EXISTS chromexpdb (
    exp_id INTEGER PRIMARY KEY,
    exp_name TEXT NOT NULL
  )
"
  )

  # methods tab
  ## method_descr: description of the method
  DBI::dbExecute(
    db,
    "
CREATE TABLE IF NOT EXISTS methodstab (
  method_id INTEGER PRIMARY KEY,
  method TEXT NOT NULL,
  method_descr TEXT,
  method_gradient TEXT,
  method_column TEXT,
  UNIQUE(method_id),
  UNIQUE(method)
);"
  )

  # chromatogram table
  DBI::dbExecute(
    db,
    "
CREATE TABLE IF NOT EXISTS chroms (
  chrom_id INTEGER PRIMARY KEY,
  exp_id INTEGER NOT NULL REFERENCES chromexpdb(exp_id),
  method_id INTEGER NOT NULL REFERENCES methodstab(method_id),
  file_name TEXT NOT NULL,
  type TEXT,
  sample_location TEXT,
  inj_vol REAL,
  date TEXT,
  UNIQUE(file_name)
);"
  )

  # gradient methods table
  ## method_id: this will auto increment and unique number
  ## method_gradient: gradient of the method
  ## q1: q1 value
  ## q3: q3 value
  ## inlet_method: inlet method
  ## transition_label: q1 > q3
  ## transition_id: T1, T2, T3, etc
  ## last unique assertations might be important to avoid repeating identical method entries

  DBI::dbExecute(
    db,
    "
CREATE TABLE IF NOT EXISTS transtab (
  transition_id INTEGER PRIMARY KEY,
  transition_label TEXT,
  method_id INTEGER NOT NULL REFERENCES methodstab(method_id),
  q1 REAL,
  q3 REAL,
  UNIQUE(method_id, transition_id)
);"
  )

  # non on the three first columns are unique.
  # the unqiuness is based on all method_id trans_id compound_id
  # IS is a property of compound. Call get_IS_name to get the IS for a compound
  DBI::dbExecute(
    db,
    "
  CREATE TABLE IF NOT EXISTS compoundstab (
    compound_id INTEGER NOT NULL PRIMARY KEY,
    transition_id INTEGER NOT NULL REFERENCES transtab(transition_id),
    qualifier BOOLEAN NOT NULL,
    compound TEXT NOT NULL,
    expected_peak_start REAL,
    expected_peak_end REAL,
    expected_rt REAL,
    IS_id TEXT,
    UNIQUE(transition_id, compound_id)
  );

"
  )

  DBI::dbExecute(
    db,
    "
  CREATE TABLE IF NOT EXISTS peakstab (
    peak_id INTEGER PRIMARY KEY,
    chrom_id INTEGER NOT NULL REFERENCES chroms(chrom_id),
    compound_id INTEGER NOT NULL REFERENCES compoundstab(compound_id),
    observed_rt REAL,
    observed_rt_start REAL,
    observed_rt_end REAL,
    observed_peak_height REAL,
    area REAL,
    manual INTEGER NOT NULL DEFAULT 0,
    date TEXT
  );

"
  )

  studydesign_db(db)

  .close_db(db)
}


rename_db_col <- function(old, new, tablename) {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  DBI::dbExecute(
    db,
    paste0("ALTER TABLE ", tablename, " RENAME COLUMN ", old, " TO ", new)
  )
  .close_db(db)
}


studydesign_db <- function(con) {
  # 1. Study
  DBI::dbExecute(
    con,
    "
  CREATE TABLE IF NOT EXISTS study (
    id      TEXT PRIMARY KEY, --uuid
    type    TEXT CHECK (type IN ('SD', 'MD', 'FE', 'BE', 'NA')),
    title   TEXT NOT NULL,
    subject_type TEXT CHECK (subject_type IN ('Human', 'Animal', 'InVitro', 'Other')) DEFAULT NULL,
    pkstudy BOOLEAN NOT NULL,
    description    TEXT,
    status  TEXT CHECK (status IN ('Planned', 'Ongoing', 'Completed', 'Terminated')),
    start_date  DATE,
    end_date  DATE
  );
  "
  )

  # 2. Subject
  DBI::dbExecute(
    con,
    "
  CREATE TABLE  IF NOT EXISTS subject (
    uuid_subject        TEXT PRIMARY KEY,
    subject_id          TEXT NOT NULL, 
    study_id            TEXT REFERENCES study(id),
    group_label         TEXT NOT NULL, -- soft key not enforced
    group_replicate     INTEGER DEFAULT NULL,
    sex                 TEXT CHECK (sex IN ('M', 'F')) DEFAULT NULL,
    age                 INTEGER CHECK (age >= 0) DEFAULT NULL,
    race                TEXT DEFAULT NULL,
    extra_factors       TEXT DEFAULT NULL,
    UNIQUE(subject_id, study_id)
  );
  "
  )

  # 3. Dosing

  DBI::dbExecute(
    con,
    "
  CREATE TABLE IF NOT EXISTS dosing (
    arm_id        TEXT PRIMARY KEY,
    study_id      TEXT NOT NULL REFERENCES study(id),
    group_label   TEXT, -- match arm_id
    period_number  INTEGER,
    dose_freq      REAL,
    dose_addl      INTEGER,
    dose_amount    REAL,
    dose_unit      TEXT CHECK (dose_unit IN ('g', 'mg', 'ug', 'NA', 'mg/kg', 'g/kg', 'ug/kg')),
    route          TEXT CHECK (route IN ('PO', 'IV', 'SC', 'IM', 'IP', 'SL', 'NA')),
    formulation    TEXT, 
    UNIQUE(group_label, arm_id, study_id)
  );
  "
  )

  # 4. Sample log
  DBI::dbExecute(
    con,
    "
  CREATE TABLE IF NOT EXISTS sample_log (
    log_id       TEXT PRIMARY KEY, --uuid
    subject_id   TEXT NOT NULL, -- soft reference to subject(id), enforced if study(pkstudy)
    study_id     TEXT NOT NULL REFERENCES study(id),
    nominal_time  TEXT, -- in hours
    actual_time  TEXT, -- in hours
    time_unit   TEXT,
    status       TEXT CHECK (status IN ('Planned', 'Collected', 'Processed')),
    sample_type   TEXT CHECK (sample_type IN ('Plasma', 'Serum', 'Whole Blood', 'Urine', 'Depot', 'CSF', 'Tissue', 'Saliva', 'Other')),
    notes         TEXT
  );
  "
  )

  # Sample quant
  DBI::dbExecute(
    con,
    "
  CREATE TABLE IF NOT EXISTS quant_meta (
    quant_id     TEXT PRIMARY KEY, 
    quant_date   DATE
  );
  "
  )

  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS quant_samples (
      quant_id     TEXT NOT NULL REFERENCES quant_meta(quant_id),
      log_id       TEXT NOT NULL, -- soft reference to sample_log(log_id)
      file_name    TEXT NOT NULL, 
      compound_id  INTEGER REFERENCES compoundstab(compound_id),
      concentration REAL,
      conc_unit   TEXT,
      UNIQUE(quant_id, log_id, file_name)
    );
  "
  )

}

get_injecseq_relation <- function(study_id) {
  # map sample_log.log_id to samples.log_id
  # get unique samples.id
  df <- retrieve_full_study_log(study_id)
  study_ids <- df |>
    dplyr::filter(!is.na(.data$log_id)) |>
    dplyr::select("log_id")

  sample_list <- .get_samplelist()

  sample_list <- study_ids |>
    dplyr::left_join(sample_list, by = c("log_id" = "log_id")) |>
    dplyr::select("log_id", "file_name", "list_id", "plate_id") |>
    dplyr::group_by(.data$log_id) |>
    dplyr::summarise(
      file_name = paste(.data$file_name, collapse = "; "),
      list_id = paste(unique(.data$list_id), collapse = "; "),
      plate_id = paste(unique(.data$plate_id), collapse = "; ")
    ) |>
    dplyr::ungroup() |>
    distinct()

  sample_list
}


