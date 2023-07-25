box::use(
    sh = shiny,
    lub = lubridate,
)

#' @export
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
