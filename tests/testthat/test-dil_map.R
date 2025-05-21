
test_that("conc_calculation_correct", {
.conc_ratio("1mg/ml", "100ug/ml") |> expect_equal("10")
.conc_ratio("1mg", "100ug")  |> expect_equal("10")

.conc_ratio("1000", "100") |> expect_equal("10")

# not same unit
.conc_ratio("1", "100ug/ml") |> expect_error()

# decimals 
.conc_ratio("150", "100") |> expect_equal("1.5")
.conc_ratio("1.5mg", "100ug") |> expect_equal("15")
})



test_that("dil_map_graph", {
# dataspecs
# to ensure parallel, last two rows must be unique by automatically by adding _wellLocation

# example of multistock parallel dilution
v0 <- paste0(c(200, 100, 50, 10, 5, 2), "ng_A,",  3:8)
v1 <- paste0(c(2000, 1000, 500, 100, 50, 20), "ng")
v2 <- paste0(c(10, 10, 5, 5, 2, 2), "ug")
v3 <- paste0(c(1, 1, 1, 1, 1,1), "mg")
par_df <- data.frame(v3, v2, v1, v0)

# example of hyprid dilution
v0 <- paste0(c(200, 100, 50, 10, 5, 2), "ng_A,",  3:8)
v1 <- paste0(c(2000, 1000, 500, 100, 50, 20), "ng")
#v2 <- c("200ug" , "100ug", "500ug", "100ug", "50ug", 500)
v3 <- paste0(c(1, 1, 1, 1000, 500,100), c("mg", "mg", "ug", "ng", "ng", "ng"))
hyprid_df <- data.frame(v3, v1, v0)


# example of serial dilution
v0 <- paste0(c(200, 100, 50, 10, 5, 2), "ng_A,",  3:8)
v1 <- paste0(c(2000, 1000, 500, 100, 50, 20), "ng")
#v2 <- c("10ug", "2000ng", "1000ng", "500ng", "200ng", "100ng")
v3 <- c("1mg", "2000ng", "1000ng", "500ng", "100ng","50ng")
serial_df <- data.frame(v3, v1, v0)


# done parallel
.gen_graph(par_df) |>  expect_no_error()
# done serial 
.gen_graph(serial_df) |> expect_no_error()
# done hyprid
.gen_graph(hyprid_df) |> expect_no_error()

})



test_that("parallel_dilution", {
    x <- generate_96() |> 
        add_cs_curve(c(1,2,3,4,5), rep = 3) |> 
        add_QC(2,2.5,4.5)  |> 
        add_cs_curve(c(2,4,6,8,9), rep = 3) |> 
        add_DQC(100, 10)   

    .parallel_dilution(x, type = "Standard")$TYPE |> unique() |> expect_equal("Standard")
    .parallel_dilution(x, type = "Standard", rep = 2)$TYPE |> unique() |> expect_equal("Standard")

    .parallel_dilution(x, type = "QC")$TYPE |> unique() |> expect_equal("QC")

    .parallel_dilution(x, type = "DQC")$TYPE |> unique() |> expect_equal("DQC")

    # negative 
    .parallel_dilution(x, type = "Standard", rep = 3) |> expect_error()

    .parallel_dilution(x, type = "QC", rep = 2) |> expect_error()

})