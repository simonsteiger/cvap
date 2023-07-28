box::use(
  tt = testthat,
  dp = dplyr,
  sh = shiny,
  pr = purrr,
  magrittr[`%>%`],
  palmerpenguins[penguins],
)

box::use(
  ase = app / logic / aux_server,
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
  sieve <- ase$sift_vars(penguins, input, test = TRUE)
  tt$expect_equal(ref, penguins[sieve, ])
})

tt$test_that("sift_vars subsets strings correctly", {
  input <- list(species = c("Adelie", "Gentoo"), island = "Biscoe")
  ref <- dp$filter(penguins, species %in% c("Adelie", "Gentoo") & island %in% "Biscoe")
  sieve <- ase$sift_vars(penguins, input, test = TRUE)
  tt$expect_equal(ref, penguins[sieve, ])
})

# Need a data set with dates to test that, too

tt$test_that("sift_vars doesn't filter `skip`ed variables", {
  input <- list(species = c("Adelie", "Gentoo"), island = "Biscoe")
  ref <- dp$filter(penguins, species %in% c("Adelie", "Gentoo"))
  sieve <- ase$sift_vars(penguins, input, skip = "island", test = TRUE)
  tt$expect_equal(ref, penguins[sieve, ])
})

tt$test_that("count_nonmissing_above_cutoff adds values above cutoff", {
  input <- list(outcome = "carb", lan = "not-null")
  alt_cars <- dp$rename(mtcars, lan = cyl)
  ref <- dp$count(alt_cars)[["n"]]
  tt$expect_equal(ref, ase$count_nonmissing_above_cutoff(alt_cars, input, NULL))
})

tt$test_that("count_nonmissing_above_cutoff excludes values below cutoff", {
  input <- list(outcome = "cyl", lan = "not-null")
  alt_cars <- dp$rename(mtcars, lan = carb)
  ref <- dp$count(alt_cars, lan) %>% dp$filter(n > 4) %>% dp$pull(n) %>% sum()
  tt$expect_equal(ref, ase$count_nonmissing_above_cutoff(alt_cars, input, NULL))
})

tt$test_that("count_nonmissing_above_cutoff returns 0 if input$lan is NULL", {
  input <- list(outcome = "cyl", lan = NULL)
  tt$expect_equal(0, ase$count_nonmissing_above_cutoff(mtcars, input, NULL))
})
