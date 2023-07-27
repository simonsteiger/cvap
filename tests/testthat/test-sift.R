box::use(
  tt = testthat,
  dp = dplyr,
  sh = shiny,
  pr = purrr,
  magrittr[`%>%`],
  palmerpenguins[penguins],
)

box::use(
  app / view / wrangle / sift,
)

tt$test_that("sift_vars requires data as reactive", {
  tt$expect_error(sift$server(mtcars))
})

# Can't figure out how to properly test the whole sift$server at once
# Testing the version of sift_vars that doesn't use reactivity
tt$test_that("sift_vars subsets numeric ranges correctly", {
  input <- list(year = c(2007, 2009), body_mass_g = c(3700, 4400))
  ref <- dp$filter(penguins, year >= 2007 & year <= 2009, body_mass_g >= 3700 & body_mass_g <= 4400)
  sieve <- test_sift_vars(penguins, input)
  tt$expect_equal(ref, penguins[sieve, ])
})

tt$test_that("sift_vars subsets strings correctly", {
  input <- list(species = c("Adelie", "Gentoo"), island = "Biscoe")
  ref <- dp$filter(penguins, species %in% c("Adelie", "Gentoo") & island %in% "Biscoe")
  sieve <- test_sift_vars(penguins, input)
  tt$expect_equal(ref, penguins[sieve, ])
})

# Need a data set with dates to test that, too

tt$test_that("sift_vars doesn't filter `skip`ed variables", {
  input <- list(species = c("Adelie", "Gentoo"), island = "Biscoe")
  ref <- dp$filter(penguins, species %in% c("Adelie", "Gentoo"))
  sieve <- test_sift_vars(penguins, input, skip = "island")
  tt$expect_equal(ref, penguins[sieve, ])
})