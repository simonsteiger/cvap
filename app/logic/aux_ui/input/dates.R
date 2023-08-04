box::use(
    sh = shiny,
    lub = lubridate,
)

#' @export
#' Wrapper with presets for single date input
inp_date <- function(id, label, value = lub$today()) {
    sh$div(
        class = "mb-4 mx-1",
        sh$dateInput(
            inputId = id,
            label = label,
            value = value,
            min = lub$ymd("1999-01-01"),
            max = lub$today(),
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
            format = "yyyy",
            startview = "year",
            language = "sv",
            separator = "jämfört med"
        )
    )
}
