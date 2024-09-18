test_that("plate_registered_before_seq", {
  generate_96() |>
    build_injec_seq( inject_vol = 2) |>
    expect_error()
})


test_that("save_injecseq_csv", {
  skip_on_cran()

  .reset_samples_db()
  x <- generate_96() |>
    add_samples(1:20) |>
    add_blank() |>
    plate_metadata("new") |>
    register_plate()

  mylist <- x |> build_injec_seq(inject_vol = 2, tray = "1", inlet_method = "method.ql")

  write_injec_seq(mylist) |> expect_no_error()

  x |> build_injec_seq(inject_vol = 2, suffix = "4", tray = "1", inlet_method = "method.q") |> # suffix changed
    write_injec_seq()  |> expect_no_error() # changed the suffix

  write_injec_seq(mylist)  |> expect_message()  # already written

})

test_that("multiple_plates", { # expect 1 list
  skip_on_cran()

  .reset_samples_db()
  x <- generate_96() |>
    add_samples(1:20) |>
    add_blank() |>
    plate_metadata("new") |>
    register_plate()

  y <- generate_96() |>
    add_samples(1:20) |>
    add_blank() |>
    plate_metadata("new") |>
    register_plate()

  x <- combine_plates(list(x,y))

  length(x) |> expect_equal(2)

  build_injec_seq(x, tray = c("1", "2"), inject_vol = 2, inlet_method = "method.q") |> expect_no_error()
})

test_that("exploratory_samples_added", {
   skip_on_cran() 

  x <- generate_96() |>
    add_cs_curve(c(1,10, 30, 40 , 100, 200)) |> # 1
    add_samples(1:20) |> # 2
    add_blank() |>  # 3
    plate_metadata("new") |>
    register_plate()
  build_injec_seq(x, tray = "1", inject_vol = 2, explore_mode = TRUE, inlet_method = "method.q1")  |> 
    expect_no_error()
}
)


test_that("writing_increment_id", {
  skip_on_cran()

  # The metadata table is accommodating for the list
  .reset_samples_db()
  x <- generate_96() |>
    add_cs_curve(c(1,10, 30, 40 , 100, 200)) |> # 1
    add_samples(1:20) |> # 2
    add_blank() |>  # 3
    plate_metadata("new") |>
    register_plate()
  build_injec_seq(x, inlet_method = "method.ql", tray = "1", inject_vol = 2, explore_mode = TRUE)  |>
    write_injec_seq()

  .last_list_id() |> expect_equal(1)

  build_injec_seq(x, inlet_method = "method.ql", tray = "1", inject_vol = 2, explore_mode = TRUE, suffix = "2")  |>
    write_injec_seq()


  .last_list_id() |> expect_equal(2)

})
