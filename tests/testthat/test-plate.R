test_that("generate_96", {
  x <- generate_96()
  expect_equal(dim(x@plate), c(8, 12))
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
  skip_on_ci()
  x <- generate_96() |> add_blank(TRUE, FALSE) |> register_plate()
  .is_registered(x) |> expect_equal(TRUE)

  x <- reuse_plate(1, 4)
  .is_registered(x) |> expect_equal(FALSE)

  validObject(x) |> expect_equal(TRUE)
}
)

test_that("suitability", {
  x <- generate_96() |> add_suitability("Suitability", conc = 2)
  as.vector(x@plate[1, 1]) |> expect_equal("Suitability")
})

test_that("plate_filled", {
  generate_96() |> add_samples(paste0("S", 1:96)) |> expect_no_error()
})



test_that("multiple_stds_n_qcs" ,{
  # return 0 if no std or qcs
  generate_96() |>  add_cs_curve(1:10) |> .last_entity("Standard") |> expect_equal(1)
  generate_96() |>  add_cs_curve(1:10) |>
    add_cs_curve(1:10) |>
   .last_entity("Standard") |> expect_equal(2)

  # retun 0, even if there is cs without qc
  generate_96() |>  add_cs_curve(1:10) |> .last_entity("QC") |> expect_equal(0)

  generate_96() |>  add_cs_curve(1:10) |> add_QC( 3,4.5,9)  |>
    .last_entity("QC")  |> expect_equal(1)
})

test_that("rep_stdTest", {
  skip_on_cran()
  skip_on_ci()

  generate_96() |> 
    fill_scheme("v", tbound = "A", bbound = "C") |>
    add_samples(rep("SLOQ/4", 3), conc = 0.25) |>
    fill_scheme("h", lbound = 2, rbound = 9) |>
    add_cs_curve(c(1, 3, 5, 10, 20, 50, 100, 200), rep =3) |> 

    fill_scheme("h", lbound = 9, rbound = 12) |>
    add_DB() |>
    add_blank() |> 
    add_blank() |> 
    fill_scheme("v", tbound = "D", bbound = "G") |>
    # fill_scheme("h", lbound = 1, rbound = 12) |>
    add_QC(3, 80, 180, reg = T, n_qc = 6, qc_serial = F) |> 
    add_QC(3, 80, 180, reg = T, n_qc = 6, qc_serial = F) |>
    fill_scheme("h") |>
    add_samples(rep("QLOQ/4", 6), conc = 0.25, prefix = "Q")  |>
    plot() |> expect_warning()

    
  # prompt: 
  # generate 96 well plate
  # fill vertically from A to C. Add sample of LLOQ/4 with conc 0.25. 
  # Fill horizontally from 2 to 9. Add 8 standards from 1 to 200 with 3 replicates.
  # Fill horizontally from 9 to 12. Add 2 DB and 2 blanks. 
  # Fill horizontally from D to H. 
  # Add 6 samples of LLOQ/4 with conc 0.25 with prefix Q.
  # Add 9 samples of LLOQ/4 with conc 0.25 with prefix DQ.
  # fill vertically from D to G. 
  # Add 3 QCs with 80, 180 and reg = T. Add 3 QCs with 80, 180 and reg = T with dil = 50.

  # Ans:
  x <- generate_96() |> 
    fill_scheme("v", tbound = "A", bbound = "C") |>
    add_samples(rep("LOQ/4", 3), conc = 0.25, prefix = "S") |>
    fill_scheme("h", lbound = 2, rbound = 9) |>
    add_cs_curve(c(1, 3, 5, 10, 20, 50, 100, 200), rep =3) |> # 8 points, 3 replicates

    fill_scheme("h", lbound = 9, rbound = 12) |>
    add_DB() |>
    add_blank() |> 
    add_blank() |> 
    fill_scheme("h", tbound = "D", bbound = "H") |>
    add_samples(rep("QLOQ/4", 6), conc = 0.25, prefix = "Q")  |>
    add_samples(rep("DQLOQ/4", 6), conc = 0.25, prefix = "DQ")  |>
    fill_scheme("v", tbound = "D", bbound = "H") |>
    add_QC(3, 90, 180, reg = T, n_qc = 6, qc_serial = F) |> 
    add_QC(3, 90, 180, reg = T, n_qc = 6, qc_serial = F) 

  # register_plate(x)
    
  plot(x, color = "conc", watermark = F)
  plot(x, color = "conc", watermark = F, transform_dil = T) 
    
})


test_that("Last position", {
  x <- generate_96(start_row  = "H", start_col  = 5) |>
    add_cs_curve(c(50, 20, 10, 5, 2, 1)) |>
    add_blank(IS = TRUE, analyte = FALSE)  |>
    add_blank(IS = FALSE, analyte = FALSE)
  x@plate[8, 12] |> unname()|> expect_equal("DB")
})

test_that("test_factor_samples", {
  x <- generate_96() |>
    add_samples(samples = 1:5, time = 1:10*30, conc = 20)  
    
  x |> plot(color = "conc") |> expect_no_error()
  x |> plot(color = "factor") |> expect_no_error()
  x |> plot(color = "time") |> expect_no_error()
})

test_that("make_metabolic_study", {
  x <- make_metabolic_study(letters[1:8])
  plot(x[[1]], color = "time") |> expect_no_error()
  plot(x[[1]], color = "factor") |> expect_no_error()
  plot(x[[1]], color = "conc") |> expect_no_error()
  plot(x[[1]], color = "samples") |> expect_no_error()
}
)

test_that("metabolic_last_plate", {
  x <- make_metabolic_study(letters[1:8])
  length(x) |> expect_equal(8)
  is.na(x[[8]]@df$time[1]) |> expect_equal(FALSE)
})

test_that("qc_ranges", {
  suppressWarnings({
    generate_96() |> 
      add_cs_curve(c(10,50,100,250,500,1000, 1500,2500)) |> 
      add_QC(30, 750 , 1750) |> expect_error()
  })

  ## errors with LQC >3
  suppressWarnings({
    generate_96() |> 
      add_cs_curve(c(10,50,100,250,500,1000, 1500,2500)) |> 
      add_QC(50, 750 , 1750, reg = FALSE) |> expect_warning()
  })

})

test_that("fill_horizontalTest", {
  generate_96() |> fill_scheme("v", tbound = "A", bbound = "C")  |> 
     add_samples(1:10)|> expect_no_error()

  # avoid filling previously filled spots #TODO raise warning and continue
})

test_that("fill_verticalTest", {
  generate_96() |> 
    add_cs_curve(c(10,50,100,250,500,1000, 1500,2500)) |>
    fill_scheme("h",  lbound = 1, rbound = 12) |>
    add_QC(30, 758 , 1880, reg = T) |> 
    expect_no_error()
})


test_that("spot_maskTest", {
  generate_96() |> 
    fill_scheme(fill = "v", tbound = "D", bbound = "E") |>
    .spot_mask()
})


test_that("samples_vectorization", {
  # multiple samples, multiple time, one dose
  x <- generate_96() |> 
    add_samples(samples = 1, time = 1:10*30, dosage = "A")
  
  expect_equal(length(unique(x@df$time)), 10+1)
  expect_equal(length(unique(x@df$dosage)), 1+1)
  expect_equal(length(unique(x@df$samples)), 1+1)

  # multiple samples, one time, multiple doses 
  x <- generate_96() |> 
    add_samples(samples = 1:10, time = 1, dosage = "A")
  x <- select(x@df, "samples", "time", "dosage") |> dplyr::distinct()
  expect_equal(nrow(x), 10+1)
  expect_equal(length(unique(x$time)), 1+1)
  expect_equal(length(unique(x$dosage)), 1+1)
  expect_equal(length(unique(x$samples)), 10+1)

  # multiple samples, multiple time, multiple doses 
  x <- generate_96() |> 
    add_samples(samples = 1:5, time = 1:10*30, dosage = c("A"))
  x <- select(x@df, "samples", "time", "dosage") |> dplyr::distinct() 
  expect_equal(nrow(x), 5*10+1) 
  expect_equal(length(unique(x$time)), 10+1)
  expect_equal(length(unique(x$dosage)), 1+1)
  expect_equal(length(unique(x$samples)), 5+1)
})

test_that("samples_combination", {

  
  x <- generate_96() |> 
    add_samples_c(n_rep= 3, time = 0:5*30, conc = c(1,2), factor = c("M", "F")) 
  plot(x)

  sum(!is.na(x@df$samples)) |> expect_equal(72) # cartesian product 

  # 2 x 2 x 2 
  x <- generate_96() |> 
    add_samples_c(n_rep = 2, time = 1:10*30, dosage = c("A", "B"), 
      factor = c("M", "F"))

  x <- x@df |> 
    dplyr::select("samples", "time", "dosage", "factor") |> 
    dplyr::distinct()
  expect_equal(nrow(x), 2*2*2*10+1)
  expect_equal(length(unique(x$time)), 10+1)
  expect_equal(length(unique(x$dosage)), 2+1) 
  expect_equal(length(unique(x$samples)), 2*2*2+1) 
  expect_equal(length(unique(x$factor)), 2+1) 
})


test_that("plotDesignTest", {
  generate_96() |> 
    add_samples(1:5, dosage = "A", factor = "M", time = 1:5*30) |>
    add_samples(6:10, dosage = "A", factor = "F", time = 1:5*30) |>
    add_samples(11:15, dosage = "B", factor = "M", time = 1:5*30) |>
    add_samples(16:18, dosage = "B", factor = "F", time = 1:3*30) |>
    plot_design() |> expect_no_error()
})

test_that("DQC", {
  x <- generate_96() |> 
    add_DQC(conc = 500, fac = 10, rep= 10)

  plot(x, transform_dil = T)  |> expect_no_error()
  plot(x, transform_dil = F)  |> expect_no_error()

  
  generate_96() |> 
    fill_scheme("h", lbound = 1, rbound = 10) |>
    add_cs_curve(c(50, 20, 10, 5, 2, 1)) |>
    add_DQC(conc = 500, fac = 10, rep= 10) |>
    add_DQC(conc = 500, fac = 100, rep= 10) |>
    add_samples(1:10, dil = 10) |> 
    expect_no_error()
})
