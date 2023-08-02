box::use(
  dp = dplyr,
  lub = lubridate,
)

box::use(
  app / logic / srqlib / srqdict
)

ref <- read.csv("app/logic/data/test/ongoing.csv", sep = ";") %>%
  dp$mutate(dp$across(ts$where(is.character), lub$as_date))

tt$test_that("fil_ongoing works as intended", {
  at <- lub$ymd("2015-01-01")
  ref_sub <- dp$filter(ref, ordinerat <= at & (pagaende == 1 | utsatt > at))
  tt$expect_equal(dp$filter(ref, !!srqdict$fil_ongoing(at)), ref_sub)
})
