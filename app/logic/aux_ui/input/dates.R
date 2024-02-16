box::use(
    sh = shiny,
    lub = lubridate,
)

last_day_prev_month <- lub$floor_date(lub$today(), "months") - lub$ddays(1)

#' @export
#' Wrapper with presets for single date input
inp_date <- function(id, label, value = last_day_prev_month) {
    sh$div(
        class = "mb-4 mx-1",
        sh$dateInput(
            inputId = id,
            label = label,
            value = value,
            min = lub$ymd("1999-01-01"),
            max = last_day_prev_month,
            format = "yyyy-mm-dd",
            startview = "year",
            language = "sv"
        )
    )
}

#' @export
#' Wrapper with presets for date range input
inp_daterange <- function(id, label, start = lub$ymd("2020-01-01"), end = lub$ymd("2021-12-31")) {
    sh$div(
        class = "mb-4 mx-1",
        sh$dateRangeInput(
            inputId = id,
            label = label,
            start = start,
            end = end,
            min = lub$ymd("1999-01-01"),
            max = last_day_prev_month,
            format = "yyyy-mm-dd",
            startview = "year",
            language = "sv",
            separator = "till"
        )
    )
}

#' @export
#' Wrapper with presets for date range input
inp_datecompare <- function(id, label, start = lub$ymd("2020-01-01"), end = lub$ymd("2021-12-31")) {
    sh$div(
        class = "mb-4 mx-1",
        sh$dateRangeInput(
            inputId = id,
            label = label,
            start = start,
            end = end,
            min = lub$ymd("1999-01-01"),
            max = last_day_prev_month,
            format = "yyyy",
            startview = "year",
            language = "sv",
            separator = "jämfört med"
        )
    )
}
