
#'@noRd
.connect_to_db <- function(){
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  db

}


#' Load methods database
#' @noRd
.get_methodsdb <- function(){
    .check_sample_db()
    db <- .connect_to_db()
    methods <- DBI::dbGetQuery(db, "SELECT * FROM methodstab")
    duckdb::dbDisconnect(db, shutdown = TRUE)
    methods
}

#' Delete samples database
#' @noRd
.reset_samples_db <- function() {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
    
  if(file.exists(db_path)) {
    file.rename(db_path, paste0(db_path, "_old"))
  }

}

#' Return metadata table for sample list
#' @noRd
.get_samplesdb_metadata <- function(){
  .check_sample_db()

  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  metadata <- DBI::dbGetQuery(db, "SELECT * FROM metadata")
  duckdb::dbDisconnect(db, shutdown = TRUE)

  metadata
}

.get_samplelist <- function(id){
  .check_sample_db()
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  sample_list <- DBI::dbGetQuery(db, paste0("SELECT * FROM samples WHERE list_id = ", id))
  duckdb::dbDisconnect(db, shutdown = TRUE)
  sample_list
}

# create it if not exists
.check_sample_db <- function() {

  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")

  # Check if the database file exists
  db <- duckdb::dbConnect(duckdb::duckdb(), db_path)
  DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS samples (
    file_name TEXT PRIMARY KEY,
    list_id INTEGER,
    inlet_method TEXT,
    row INTEGER,
    col INTEGER,
    value TEXT,
    sample_location TEXT,
    samples TEXT,
    type TEXT,
    std_rep INTEGER,
    tray TEXT,
    inj_vol REAL,

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
    conc TEXT,
    time TEXT,
    factor TEXT,
    UNIQUE(file_name)
  );
")

# This id auto increments and is assigned to list_id above
DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS metadata (
    id INTEGER PRIMARY KEY,
    date TEXT,
    assoc_plates TEXT,
    description TEXT,
    UNIQUE(id)
  );
") # id, date, assoc_plates

# chromatogram table
DBI::dbExecute(db, "
CREATE TABLE IF NOT EXISTS chroms (
  chrom_id INTEGER PRIMARY KEY,
  file_name TEXT NOT NULL,
  compound TEXT,
  compound_id INTEGER NOT NULL,
  transition_id INTEGER NOT NULL,
  observed_rt REAL,
  observed_rt_start REAL,
  observed_rt_end REAL,
  observed_peak_height REAL,
  area REAL,
  manual INTEGER NOT NULL DEFAULT 0,
  date TEXT,
  UNIQUE(chrom_id)
);")


# gradient methods table
DBI::dbExecute(db, "
CREATE TABLE IF NOT EXISTS methodstab (
  method_id INTEGER PRIMARY KEY,
  method TEXT,
  method_descr TEXT,
  UNIQUE(method_id, method)
);" )

DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS peakstab (
    peak_id INTEGER PRIMARY KEY,
    file_name TEXT NOT NULL,
    compound TEXT,
    compound_id INTEGER NOT NULL,
    transition_id INTEGER NOT NULL,
    observed_rt REAL,
    observed_rt_start REAL,
    observed_rt_end REAL,
    observed_peak_height REAL,
    area REAL,
    manual INTEGER NOT NULL DEFAULT 0,
    date TEXT,
    UNIQUE(peak_id)
  );

")

DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS transtab (
    transition_id INTEGER PRIMARY KEY,
    transition_label TEXT,
    q1 REAL,
    q3 REAL,
    inlet_method TEXT,
    UNIQUE(transition_id),
    UNIQUE(q1, q3, inlet_method)
  );
")

DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS compoundstab (
    compound_id TEXT,
    compound TEXT,
    method_id INTEGER,
    transition_id INTEGER,
    expected_rt_start REAL,
    expected_rt_end REAL,
    expected_rt REAL
  );

")

duckdb::dbDisconnect(db, shutdown = TRUE)
}
