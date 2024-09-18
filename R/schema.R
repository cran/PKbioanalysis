
.check_plate_df <- function(x) {
    cols <- c("row", "col", "value", "SAMPLE_LOCATION", "samples", "conc", "time", "factor", "TYPE", "std_rep")
    col_type <- c("integer", "integer", "character", "character", "character", "character", 
        "numeric", "character", "character", "integer")
    checkmate::assertDataFrame(x, ncols = length(cols), types = col_type)
    checkmate::assertNames(names(x), identical.to = cols)

}
