box::use(
  tt = testthat,
  pal = palettes,
  pr = purrr,
)

box::use(
  app / logic / srqlib / srqcolor[ramp]
)

tt$test_that("ramp creates color palettes for 2:7", {
  valid_num_colors <-
    pr$map_lgl(2:7, \(i) pal$is_color(ramp(i))) %>%
    pr$reduce(`&`)
  tt$expect_true(valid_num_colors)
})

tt$test_that("ramp errors on numeric values outside 2:7", {
  
  tt$expect_true(valid_num_colors)
})

tt$test_that("ramp creates abyss palette", {
  
  tt$expect_true(valid_num_colors)
})
