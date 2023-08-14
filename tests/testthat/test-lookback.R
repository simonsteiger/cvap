box::use(
  tt = testthat,
  lub = lubridate,
  magrittr[`%>%`],
  dp = dplyr,
  ts = tidyselect,
  rl = rlang[`%||%`],
  here,
)

box::use(
  app / view / wrangle / lookback[lookback],
  app / logic / srqlib / srqdict,
)

ref <- read.csv(here$here("app/logic/data/test/ongoing.csv"), sep = ";") %>%
  dp$mutate(dp$across(ts$where(is.character), lub$as_date))

tt$test_that("lookback errors if `.data` not reactive", {
  tt$expect_error(lookback(mtcars, list(lookback = 2)))
})

tt$test_that("lookback renames input$outcome to `outcome`", {
  target <- "measure" # measure is the toy column with our outcome of interest
  idx <- ref$measure[which(names(ref) == target)] # get index of that column
  test <- lookback(ref, list(outcome = target, ongoing = lub$ymd("2020-01-01"), lookback = 3))
  tt$expect_equal(names(test[idx]), "outcome")
})

tt$test_that("lookback renames .var to `outcome`", {
  target <- "measure" # measure is the toy column with our outcome of interest
  idx <- ref$measure[which(names(ref) == target)] # get index of that column
  test <- lookback(ref, list(ongoing = lub$ymd("2020-01-01"), lookback = 3), .var = target)
  tt$expect_equal(names(test[idx]), "outcome")
})

tt$test_that("lookback filters correctly", {
  input <- list(ongoing = lub$ymd("2020-01-01"), lookback = 3)
  .var <- "measure"
  # Steps below are relevant for correct filtering
  # If fil_ongoing breaks, there will be an error in test-srqdict, too
  # The remaining functions are dplyr
  ref_sub <- ref %>%
    dp$rename(outcome = .data[[.var %||% input$outcome]]) %>%
    dp$filter(!is.na(outcome)) %>%
    dp$filter(
      !!srqdict$fil_ongoing(input$ongoing),
      datum >= input$ongoing - as.numeric(input$lookback) * lub$dyears(1)
    ) %>%
    dp$arrange(patientkod, dp$desc(datum)) %>%
    dp$distinct(patientkod, .keep_all = TRUE)
  test <- lookback(ref, input, .var)
  tt$expect_equal(test, ref_sub)
})
