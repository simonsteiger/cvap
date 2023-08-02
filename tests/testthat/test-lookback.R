box::use(
  tt = testthat,
  lub = lubridate,
  magrittr[`%>%`],
  dp = dplyr,
  ts = tidyselect,
)

box::use(
  app / view / wrangle / lookback[lookback],
)

ref <- read.csv("app/logic/data/test/testdata_ongoing.csv", sep = ";") %>%
  dp$mutate(dp$across(ts$where(is.character)), ~ as.factor(.x))

tt$test_that("lookback errors if `.data` not reactive", {
  tt$expect_error(lookback(mtcars, list(lookback = 2)))
})



lookback <- function(.data, input, .var = NULL) {
  stopifnot(!sh$is.reactive(.data)) # no reactive input
  .data %>%
    dp$rename(outcome = .data[[.var %||% input$outcome]]) %>%
    # separately get rid of missing outcome values to avoid picking NA outcomes
    dp$filter(!is.na(outcome)) %>%
    dp$filter(
      !!srqdict$fil_ongoing(input$ongoing),
      datum >= input$ongoing - as.numeric(input$lookback) * lub$dyears(1)
    ) %>%
    dp$arrange(patientkod, dp$desc(datum)) %>%
    dp$distinct(patientkod, .keep_all = TRUE)
}
