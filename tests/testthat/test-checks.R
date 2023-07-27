box::use(
  tt = testthat,
)

box::use(
  ase = app / logic / aux_server,
)

# vali_date
tt$test_that("vali_date detects single invalid date", {
  input <- list(inkluderad = "202222-01-01")
  tt$expect_equal(ase$vali_date(input)$valid, FALSE)
})

tt$test_that("vali_date detects invalid date in vector", {
  input <- list(inkluderad = c("202222-01-01", "2021-01-01"))
  tt$expect_equal(ase$vali_date(input)$valid, FALSE)
})

tt$test_that("vali_date detects single out-of-range date", {
  input <- list(inkluderad = "1998-01-01")
  tt$expect_false(ase$vali_date(input)$inrange)
})

tt$test_that("vali_date detects out-of-range date in vector", {
  input <- list(inkluderad = c("2001-01-01", "2045-01-01"))
  tt$expect_false(ase$vali_date(input)$inrange)
})

tt$test_that("vali_date captures target variable name", {
  input <- list(inkluderad = c("2001-01-01", "2045-01-01"))
  tt$expect_equal(ase$vali_date(input)$var, "inkluderad")
})

tt$test_that("vali_date captures nothing if there is no target", {
  input <- list(some_date = c("2001-01-01", "2045-01-01"))
  tt$expect_equal(c(ase$vali_date(input)$var, ase$vali_date(input)$inrange), c(NULL, TRUE))
})

# check_samplesize
tt$test_that("check_samplesize returns TRUE if there are more than 5 entries for any lan", {
  df <- data.frame(lan = c(rep("lan1", 6), rep("lan2", 3), rep("lan3"), 12))
  tt$expect_true(ase$check_samplesize(df))
})

tt$test_that("check_samplesize returns FALSE if there are less than 5 entries for all lan", {
  df <- data.frame(lan = rep("example-lan", 6))
  tt$expect_true(ase$check_samplesize(df))
})
