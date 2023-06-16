box::use(
    sh = shiny,
    lub = lubridate,
)

#' @export
inp_date <- function(id, label, value = lub$today()) {
    sh$div(
        class = "mb-3 mx-1",
        sh$dateInput(
            inputId = id,
            label = label,
            value = value,
            min = lub$ymd("1999-01-01"),
            max = lub$today(),
            format = "yyyy-mm-dd",
            startview = "year"
        )
    )
}
