skip_if_no_py <- function() {
  if (!reticulate::py_module_available("src"))
    skip("scipy not available for testing")
}


# PKbioanalysis:::.reset_samples_db()
# .generate_testing_injeseq()
# path <- system.file("extdata", "waters_MZML_ex", package="PKChromaMetrics")
# main <- read_chrom(path, format = "mzML", peaks = imported_peaks)

# path <- system.file("extdata", "waters_raw_ex", package="PKChromaMetrics")
# imported_peaks_path <- system.file("sample_peaktab.csv", package="PKChromaMetrics")
# imported_peaks <- read.csv(imported_peaks_path)
# main <- read_chrom(path, method = "3")

.reset_samples_db()
x <- system.file("cmpds.yaml", package = "PKbioanalysis") |>
  .parse_cmpds() |>
  suppressWarnings()
.save_cmpd_db(x)

# imported_peaks <- read_experiment_results(system.file("extdata", "waters_NEU_PK/quandata.xml", package="PKChromaMetrics"), vendor = "targetlynx")
# imported_peaks <- .peakresToDF(imported_peaks)

if(reticulate::py_module_available("src")){
  path <- system.file("extdata", "waters_raw_ex", package = "PKbioanalysis")
  path <- list.files(path, full.names = TRUE, pattern = "raw$", all.files = TRUE)
  main_nosmooth <- read_chrom(path, method = 1)
  main <- smooth_chrom(main_nosmooth, filter = "savgol", window = 3)
}

## quant obj
cmpyml <- system.file("cmpds_MTG.yaml", package = "PKbioanalysis")
cmpyml <- .parse_cmpds(cmpyml) |> suppressWarnings()
.save_cmpd_db(cmpyml)

dat <- system.file("extdata", "08122019_MTG.txt", package = "PKbioanalysis")
suppressWarnings(
  quantobj <- read_experiment_results(dat, vendor = "targetlynx_csv")
)

quantobj <- create_quant_object(quantobj, 2)
