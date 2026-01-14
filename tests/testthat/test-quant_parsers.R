test_that("parse_tlynx_csv", {
  x <- system.file("extdata", "08122019_MTG.txt", package = "PKbioanalysis")
  x <- .parse_tlynx_csv(x)
  class(x) |> expect_equal("PeakRes")
  x$res[[1]]
})


test_that("parse_tlynx_xml", {
  x <- system.file("extdata", "sampleTargetLynx.xml", package = "PKbioanalysis")
  x <- .parse_tlynx_xml(x)
})
