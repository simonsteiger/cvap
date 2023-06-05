box::use(
    sh = shiny,
    lub = lubridate,
)

inp_daterange <- function(id, label, start = lub$ymd("2020-01-01"), end = lub$ymd("2021-12-31")) {
    sh$div(
        class = "mb-4",
        sh$dateRangeInput(
            inputId = id,
            label = label,
            start = start,
            end = end,
            min = lub$ymd("1999-01-01"),
            max = lub$today(),
            format = "yyyy-mm-dd",
            startview = "year",
            separator = "till"
        )
    )
}
