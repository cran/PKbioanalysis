test_that("generate_96", {
  x <- generate_96()
  expect_equal(dim(x$plate), c(8, 12))
  .check_plate_df(x$df) |> expect_no_error()
  .is_registered(x) |> expect_equal(FALSE)
  plot(x) |> expect_no_error()
  .plate_subid(x) |> expect_equal(1)
})

test_that("plate_registration", {
  skip_on_cran()

  x <- generate_96() |> add_blank(TRUE, FALSE)

  x <- register_plate(x)
  .is_registered(x) |> expect_equal(TRUE)
  plot(x) |> expect_no_error()

  register_plate(x) |> expect_error()

})

test_that("reuse_plate", {
  skip_on_cran()
  x <- generate_96() |> add_blank(TRUE, FALSE) |> register_plate()
  x <- reuse_plate(1, 4)
  .is_registered(x) |> expect_equal(FALSE)

  .check_plate_df(x$df) |> expect_no_error()


}
)

test_that("suitability", {
  x <- generate_96() |> add_suitability("Suitability", conc = 2)
  as.vector(x$plate[1, 1]) |> expect_equal("Suitability")
})

test_that("plate_filled", {
  generate_96() |> add_samples(paste0("S", 1:96)) |> expect_no_error()
})



test_that("multiple_stds_n_qcs" ,{
  # return 0 if no std or qcs
  generate_96() |>  add_cs_curve(1:10) |> .last_std() |> expect_equal(1)
  generate_96() |>  add_cs_curve(1:10) |>
    add_cs_curve(1:10) |>
   .last_std() |> expect_equal(2)

  # retun 0, even if there is cs without qc
  generate_96() |>  add_cs_curve(1:10) |> .last_qc() |> expect_equal(0)

  generate_96() |>  add_cs_curve(1:10) |> add_qcs( 3,4.5,9)  |>
    .last_qc() |>
    expect_equal(1)

})

test_that("Last position", {
  x <- generate_96(empty_rows = c("H"), extra_fill = 4) |>
    add_cs_curve(c(50, 20, 10, 5, 2, 1)) |>
    add_blank(IS = TRUE, analyte = FALSE)  |>
    add_blank(IS = FALSE, analyte = FALSE)
  x$plate[8, 12] |> unname()|> expect_equal("DB")
})

test_that("test_factor_samples", {
  generate_96() |>
    add_samples(samples = 1:10, time = 1:10*30, conc = 1:10)  |>
    plot(color = "conc") |> expect_no_error()

  generate_96() |>
    add_samples(samples = 1:10, time = 10, conc = 1:10)  |>
    plot(color = "conc") |> expect_no_error()

  generate_96() |>
    add_samples(samples = 1:10, time = 1:10*30, conc = 1)  |>
    plot(color = "factor") |> expect_no_error()

  generate_96() |>
  add_samples(samples = 1:10, time = 1:10*30, conc = NA)  |>
  plot(color = "time") |> expect_no_error()

  generate_96() |>
  add_samples(samples = 1:10, time = 1:10*30, conc = 1:2)  |>
  plot(color = "time") |> expect_error()

})

test_that("make_metabolic_study", {
  x <- make_metabolic_study(letters[1:8])
  plot(x[[1]], color = "time") |> expect_no_error()
  plot(x[[1]], color = "factor") |> expect_no_error()
  plot(x[[1]], color = "conc") |> expect_no_error()
  plot(x[[1]], color = "TYPE") |> expect_no_error()
  plot(x[[1]], color = "samples") |> expect_no_error()
  

}
)

test_that("metabolic_last_plate", {
  x <- make_metabolic_study(letters[1:8])
  length(x) |> expect_equal(8)
  is.na(x[[8]]$df$time[1]) |> expect_equal(FALSE)
})

test_that("combination_samples", {

  x <- generate_96() |> 
    add_samples_c(samples = 1:3, time = 0:5*30, conc = c(1,2), factor = c("M", "F")) 
  plot(x)

  sum(!is.na(x$df$samples)) |> expect_equal(72) # cartesian product 

})


test_that("qc_ranges", {
  suppressWarnings({
    generate_96() |> 
      add_cs_curve(c(10,50,100,250,500,1000, 1500,2500)) |> 
      add_qcs(75, 750 , 1750) |> expect_warning()
  })
})