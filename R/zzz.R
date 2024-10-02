PKbioanalysis_env <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("Welcome to ", pkgname, " version ", packageVersion(pkgname), ".")
    packageStartupMessage("For citation information, type citation(\"", pkgname, "\").")
  }

  # check if the database exists
    PKbioanalysis_env$data_dir <- tools::R_user_dir("PKbioanalysis", "data")
    PKbioanalysis_env$data_dir <- PKbioanalysis_env$data_dir

    if(!dir.exists(PKbioanalysis_env$data_dir)){
        dir.create(PKbioanalysis_env$data_dir, showWarnings = F, recursive = T)
    }

    if(!dir.exists(file.path(PKbioanalysis_env$data_dir, "plates_cache"))){
        dir.create(file.path(PKbioanalysis_env$data_dir, "plates_cache"), showWarnings = T, recursive = T)
    }

    invisible()
}
