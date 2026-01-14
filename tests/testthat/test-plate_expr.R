test_that("extract_last_layout", {
  x <- bquote(
    generate_96(descr = "My Plate") |>
      add_blank(group = "A") |>
      fill_scheme(fill = "h") |>
      add_suitability() |>
      fill_scheme(tbound = 1, fill = "v")
  )

  check_last_fill(x) |> expect_equal("v")

  x <- bquote(
    generate_96(descr = "My Plate")
  )

  check_last_fill(x) |> expect_equal(NULL) # no expr

  x <- quote(
    generate_96(descr = "My Plate") |>
      add_blank(group = "A")
  )
  check_last_fill(x) |> expect_equal(NULL)
})

test_that("undo 1 step", {
  x <- quote(
    generate_96(descr = "My Plate") |>
      add_blank(group = "A") |>
      fill_scheme(fill = "h") |>
      add_suitability() |>
      fill_scheme(tbound = 1, fill = "v")
  )

  y <- undo_last_call(x)
  z <- undo_last_call(y)
  expect_equal(
    y,
    quote(
      generate_96(descr = "My Plate") |>
        add_blank(group = "A") |>
        fill_scheme(fill = "h") |>
        add_suitability()
    )
  )
  expect_equal(
    z,
    quote(
      generate_96(descr = "My Plate") |>
        add_blank(group = "A") |>
        fill_scheme(fill = "h")
    )
  )
})


test_that("print_expr", {
  x <- quote(
    generate_96(descr = "My Plate") |>
      add_blank(group = "A") |>
      fill_scheme(fill = "h") |>
      add_suitability() |>
      fill_scheme(tbound = 1, fill = "v")
  )

  cat(print_expr(x))
})
