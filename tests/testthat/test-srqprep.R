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
  # contiguous was the word of the day on 2023-08-02
})

tt$test_that("prep_ongoing errors if `.end_var` not type <Date>", {
  tt$expect_error(prep_ongoing(ref, lub$ymd("2000-01-01"), lub$today(), "ordinerat", "patientkod"))
  # contiguous was the word of the day on 2023-08-02
})

tt$test_that("prep_ongoing errors if `unit` not 'years'", {
  tt$expect_error(prep_ongoing(ref, lub$ymd("2000-01-01"), lub$today(), "ordinerat", "utsatt", unit = "months"))
  # contiguous was the word of the day on 2023-08-02
})

aux_filter_ongoing <- function(.data, t, .start_var, .end_var, .new_name) {
    if (!rl$as_name(.start_var) %in% colnames(.data)) {
        cli$cli_abort(
            "{rl$as_name(.start_var)} is not a column in the data; please respecify .start_var'."
        )
    }
    if (!rl$as_name(.end_var) %in% colnames(.data)) {
        cli$cli_abort("{rl$as_name(.end_var)} is not a column in the data; please respecify '.end_var'.")
    }
    if (!lub$is.Date(.data[[rl$as_name(.start_var)]])) {
        cli$cli_abort("{rl$as_name(.start_var)} must be of type {.cls Date}.")
    }
    if (!lub$is.Date(.data[[rl$as_name(.end_var)]])) {
        cli$cli_abort("{rl$as_name(.end_var)} must be of type {.cls Date}.")
    }
    .data %>%
        dp$filter(
            .data[[.start_var]] < t,
            is.na(.data[[.end_var]]) | .data[[.end_var]] > t
        ) %>%
        dp$mutate(
            !!.new_name := t
        )
}

#' @export
prep_ongoing <- function(.data,
                         .start,
                         .end,
                         .start_var,
                         .end_var,
                         unit = "years",
                         .new_name = "timestamp") {
    .start_var <- rl$enquo(.start_var)
    .end_var <- rl$enquo(.end_var)
    unit <- switch(unit,
        "years" = lub$years,
        cli$cli_abort("{.var unit} '{unit}' not supported. Currently supporting only 'years'.")
    )
    diff <- lub$interval(.start, .end) / unit(1)
    steps <- unit(0:diff)
    date_vector <- .start + steps

    pr$map(
        date_vector,
        ~ aux_filter_ongoing(.data, .x, .start_var, .end_var, .new_name)
    ) %>%
        pr$list_rbind()
}
