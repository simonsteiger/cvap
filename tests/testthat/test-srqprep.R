box::use(
  rl = rlang[`%||%`],
  cli,
  magrittr[`%>%`],
  dp = dplyr,
  tt = testthat,
)

box::use(
  app / logic / srqlib / srqprep[...],
)

ref <- read.csv("app/logic/data/test/ongoing.csv", sep = ";") %>%
  dp$mutate(dp$across(ts$where(is.character), lub$as_date))

tt$test_that("prep_ongoing errors if `.start_var` not in `.data`", {
  tt$expect_error(prep_ongoing(ref, lub$ymd("2000-01-01"), lub$today(), "contiguous", "utsatt"))
  # contiguous was the word of the day on 2023-08-02
})

tt$test_that("prep_ongoing errors if `.end_var` not in `.data`", {
  tt$expect_error(prep_ongoing(ref, lub$ymd("2000-01-01"), lub$today(), "ordinerat", "perquisite"))
  # perquisite was the word of the day on 2023-08-01
})

tt$test_that("prep_ongoing errors if `.start_var` not type <Date>", {
  tt$expect_error(prep_ongoing(ref, lub$ymd("2000-01-01"), lub$today(), "measure", "utsatt"))
})

tt$test_that("prep_ongoing errors if `.end_var` not type <Date>", {
  tt$expect_error(prep_ongoing(ref, lub$ymd("2000-01-01"), lub$today(), "ordinerat", "patientkod"))
})

tt$test_that("prep_ongoing errors if `unit` not 'years'", {
  tt$expect_error(prep_ongoing(ref, lub$ymd("2000-01-01"), lub$today(), "ordinerat", "utsatt", unit = "months"))
})

tt$test_that("prep_ongoing expands data correctly", {
  timestamp_count <- ref %>%
    prep_ongoing(lub$ymd("2000-01-01"), lub$ymd("2023-08-02"), "ordinerat", "utsatt") %>%
    dp$count(timestamp) %>%
    dp$pull(n)

    t <- tail(timestamp_count, 2) # Ongoing cases in 2022, 2023 => check test data, that's 4, 3!
    h <- head(timestamp_count, 1) # Ongoing cases in 2001 => check test data, that's 1!
  tt$expect_equal(c(t, h), c(4, 3, 1))
})
