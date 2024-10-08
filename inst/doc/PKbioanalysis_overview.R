## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(PKbioanalysis)

## -----------------------------------------------------------------------------
plate <- generate_96() 
plot(plate)

## -----------------------------------------------------------------------------
plate <- generate_96("Calibration study") |> 
    add_cs_curve(c(1, 3, 5, 10, 20, 50, 100, 200, 500)) |>
    add_DB() |> 
    add_DB() |>
    add_blank() |>   # CS0IS1
    add_suitability(conc = 20) |>
    add_qcs(2, 200 ,400, qc_serial = FALSE) |> 
    add_DB() |> 
    add_blank(analyte = TRUE, IS = FALSE) |> # CS1IS0
    add_cs_curve(c(1, 3, 5, 10, 20, 50, 100, 200, 500)) |>
    add_qcs(lqc_conc = 2, mqc_conc =  180 , hqc_conc = 400) 

plot(plate)
plot(plate, color = "TYPE")

## ----eval = FALSE-------------------------------------------------------------
#  plate <- register_plate(plate)
#  plot(plate)

## ----eval = FALSE-------------------------------------------------------------
#  plate <- generate_96() |>
#      add_suitability(conc = 10) |>
#      make_calibration_study(
#          plate_std = c(1, 3, 5, 10, 20, 50, 100, 200, 500),
#          lqc_conc = 2,
#          mqc_conc = 40,
#          hqc_conc = 100,
#          n_qc = 4,
#          qc_serial = TRUE,
#          n_CS0IS0 = 2,
#          n_CS1IS0 = 2,
#          n_CS0IS1 = 2)
#  plot(plate, color = "TYPE")
#  plot(plate, color  = "conc")
#  

## ----eval = FALSE-------------------------------------------------------------
#  plate <- register_plate(plate)

## ----eval = FALSE-------------------------------------------------------------
#  plate_app()

## -----------------------------------------------------------------------------
plate <- generate_96() |> add_samples(1:20)

plot(plate)

## -----------------------------------------------------------------------------
plate <- generate_96()

plate <- plate |> 
    add_samples("sample1") |> 
    add_samples("sample2") |>
    add_samples("sample3")

plot(plate)

## -----------------------------------------------------------------------------
data(Indometh)
plate <- generate_96() |> 
    add_samples(samples = Indometh$Subject, 
                time = Indometh$time)
plot(plate, color = "time")
plot(plate, color = "samples")

## -----------------------------------------------------------------------------
plate <- generate_96() |> 
    # add_samples_c(c("subject1", "subject2"), time = c(0, 10, 30, 60, 120), factor = "Male") |>
    add_samples_c("subject1", time = c(0, 10, 30, 60, 120), 
                  factor = "Male") |>
    add_samples_c("subject2", time =   c(0, 10, 30, 120), 
                  factor = "Female") 

plot(plate, color = "time")
plot(plate, color = "samples")
plot(plate, color = "factor")
plot(plate, color = "conc")

## -----------------------------------------------------------------------------
plate <- generate_96() |> 
    add_samples_c(1:4, time = c(0, 10, 30, 60, 120), 
         factor = c("Fed", "Fast"))
plot(plate, color = "time")
plot(plate, color = "samples")
plot(plate, color = "factor")
plot(plate, color = "conc")

## -----------------------------------------------------------------------------
plates <- make_metabolic_study(
    cmpd = c("NE", "DA", "5HT", "HVA", 
             "DOPAC", "MHPG", "5HIAA", "VMA"),
    time_points = c(0, 5, 10, 15, 30, 45, 60, 90, 120),
    n_NAD = 3, 
    n_noNAD = 2
)


## -----------------------------------------------------------------------------
length(plates)
plot(plates[[1]], color = "samples", label_size = 9)
plot(plates[[2]], color = "samples", label_size = 9)
plot(plates[[3]], color = "samples", label_size = 9)
plot(plates[[4]], color = "samples", label_size = 9)
plot(plates[[5]], color = "samples", label_size = 9)
plot(plates[[6]], color = "samples", label_size = 9)



## ----eval = FALSE-------------------------------------------------------------
#  register_plate(plates)

