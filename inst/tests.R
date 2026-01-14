devtools::test(filter = "methodsfile")
devtools::test(filter = "studydesign")
devtools::test(filter = "plate")
devtools::test(filter = "plate_expr")
devtools::test(filter = "injec_list")
devtools::test(filter = "dil_map")
devtools::test(filter = "apps")

devtools::test(filter = "chrom_parsers")
devtools::test(filter = "chrom_res")
devtools::test(filter = "integrate")
devtools::test(filter = "update_RT")

devtools::test(filter = "quant_object")
devtools::test(filter = "quant_parser")
devtools::test(filter = "quant_misc")

devtools::test(filter = "suitability")
devtools::test(filter = "linearity")

devtools::test(filter = "calculations")


devtools::check()
devtools::check_rhub()
devtools::check_man()


chk <- checkglobals::checkglobals(pkg = ".")
chk$globals
chk$imports

unquoted_to_string <- function(x) {
  deparse(substitute(x))
}

unquote_to_vector <- function(...) {
  sapply(substitute(list(...))[-1], deparse)
}
