box::use(
  pr = purrr,
  tt = testthat,
)

box::use(
  ase = app / logic / aux_server,
)

tt$test_that("iconostasis creates correct number of icons for valid input", {
  input <- list(kon = "Man", outcome = "n", lan = c("a", "b", "c"))
  tt$expect_equal(length(pr$map2(names(input), input, \(x, y) iconostasis[[x]](y))), 3)
})

tt$test_that("iconostasis errors when an input name has no matching icon function", {
  input <- list(kon = "Man", outcome = "n", lan = c("a", "b", "c"), unknown = c(1, 2, 3))
  tt$expect_error(length(pr$map2(names(input), input, \(x, y) iconostasis[[x]](y))))
})
