box::use(
  magrittr[`%>%`],
  tt = testthat,
  tbl = tibble,
  dp = dplyr,
  pr = purrr,
  lub = lubridate,
  here,
)

box::use(
  ase = app / logic / aux_server,
  app / logic / aux_server / wrangle / sort[get_target_level],
)

ref <- read.csv(here$here("app/logic/data/test/sort.csv"), sep = ";") %>%
  dp$mutate(
    dp$across(c(datum, ordinerat), lub$as_date),
    some_fct = as.factor(pr$map(1:4, \(i) sample(c("a", "b"), 4, replace = TRUE)) %>% unlist())
  )

tt$test_that("date sort sorts by group within lan", {
  test <- ase$sort_date(ref, "datum") %>%
    dp$mutate(rank = dp$dense_rank(datum), .by = patientkod) %>%
    dp$pull(rank)

  tt$expect_equal(test, rep(c(1, 2, 3, 4), 4))
})

# It's currently unclear how `sort_fct` should treat groups without values on `target level`
# see also aux_server/sort.R
tt$test_that("fct sort sorts by group (`some_fct`) within lan", {
  test <- ref %>%
    dp$summarise(outcome = sum(outcome), .by = c(lan, some_fct)) %>%
    ase$sort_fct("some_fct") %>%
    dp$filter(some_fct == get_target_level(., "some_fct")) %>%
    dp$pull(lan) %>%
    as.character() %>%
    unique()

  compare <- ref %>%
    dp$summarise(outcome = sum(outcome), .by = c(lan, some_fct)) %>%
    dp$filter(some_fct == get_target_level(., "some_fct")) %>%
    dp$arrange(outcome) %>%
    dp$pull(lan) %>%
    unique()

  tt$expect_equal(test, compare)
})

tt$test_that("num sort sorts by group (`measure`) within lan", {
  test <- ref %>%
    dp$summarise(outcome = sum(outcome), .by = c(lan, measure)) %>%
    ase$sort_num("measure") %>%
    dp$pull(lan) %>%
    as.character() %>%
    unique()

  compare <- ref %>%
    dp$summarise(outcome = sum(outcome), .by = c(lan, measure)) %>%
    dp$filter(measure == max(measure)) %>%
    dp$arrange(outcome, lan) %>%
    dp$pull(lan) %>%
    unique()

  tt$expect_equal(test, compare)
})

tt$test_that("num sort sorts NA to lowest (first) level", {
  ref <- ref %>%
    dp$mutate(outcome = ifelse(lan == "Kalmar" & measure == max(measure), NA, outcome))

  test <- ref %>%
    ase$sort_num("measure") %>%
    dp$pull(lan) %>%
    as.character() %>%
    unique()

  tt$expect_equal(test[1], "Kalmar") # Kalmar's NA on measure is equivalent to outcome == 0
})

# These next to pipelines go outside because testthat is unhappy with the warnings
ref_fct_na_to_0 <- ref %>%
  dp$mutate(
    outcome = ifelse(lan == "Kalmar" & some_fct == get_target_level(., "some_fct"),
      NA, some_fct
    )
  )

# Warning here caused by srqprep$custom_reorder
# Not sure why since I've set .na_rm = FALSE
test_fct_na_to_0 <- ref_fct_na_to_0 %>%
  dp$summarise(outcome = sum(outcome), .by = c(lan, some_fct)) %>%
  ase$sort_fct("some_fct") %>%
  dp$pull(lan) %>%
  as.character() %>%
  unique()

tt$test_that("fct sort sorts NA to lowest (first) level", {
  tt$expect_equal(test_fct_na_to_0[1], "Kalmar")
  # Kalmar's NA on measure is equivalent to outcome == 0
})
