box::use(
    magrittr[`%>%`],
    sh = shiny,
    dp = dplyr,
    rl = rlang,
    lub = lubridate,
    ts = tidyselect,
)

box::use(
    ase = app / logic / aux_server,
)

#' @export
ui <- function(id) {
    ns <- sh$NS(id)
    sh$tagList()
}

#' @export
server <- function(id, .data, ...) {
    sh$moduleServer(id, function(input, output, session) {
        stopifnot(sh$is.reactive(.data))

        dots <- rl$quos(...)

        sh$reactive(
            .data() %>%
                dp$mutate(dp$across(ts$where(lub$is.Date), \(x) lub$floor_date(x, "years"))) %>%
                ase$squash(...)
        )
    })
}
