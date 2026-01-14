#' @title Export Expected RT
#' @description Export expected RT values for each peak in the chromatogram.
#' @param chrom_res ChromRes object
#' @param path Path to save the file
#' @import checkmate
#' @import dplyr
#' @export
export_integration <- function(chrom_res, path) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertString(path)

  dat <- chrom_res@runs
  peaktab <- chrom_res@peaks
  detected_peaks_dat <- peaktab |>
    dplyr::select(
      "transition",
      "sample",
      "transition_id",
      "sample_id",
      "expected_peak_start",
      "expected_peak_end",
      "expected_peak_RT"
    ) |>
    write.csv(file = path, row.names = F)
}


save_compounds_config <- function(chrom_res, path) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertPathForOutput(path)
}


read_compounds_config <- function(chrom_res, compound_config) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertFileExists(compound_config)
}


add_sample_info <- function(chrom_res, sample_info) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertFileExists(sample_info)
}


save_experiment <- function(chrom_res, path) {
  checkmate::assertClass(chrom_res, "ChromRes")
  checkmate::assertPathForOutput(path)
}
